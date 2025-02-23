#' Download Raster Data from an ESRI REST Image Service
#'
#' This function downloads raster data from an ESRI REST image service for a specified area-of-interest (AOI).
#' The AOI should be provided as an \code{sf} or \code{sfc} object. The function divides the AOI into tiles (if needed),
#' downloads each tile using the ESRI REST API, merges the tiles into a single raster, and optionally masks the output
#' raster to the AOI boundary.
#'
#' @param aoi An \code{sf} or \code{sfc} object representing the area-of-interest.
#' @param base_url A character string with the URL of the ESRI REST image service (e.g.,
#'   \code{"https://di-usfsdata.img.arcgis.com/arcgis/rest/services/CONUS_site_productivity_2018_masked_202106032103033/ImageServer/exportImage"}).
#' @param output_path A character string specifying the file path for the output raster (GeoTIFF). Default is \code{"esri_rest_output.tif"}.
#' @param max_tile_size Integer specifying the maximum size (in pixels) of each downloaded tile. Default is \code{20000}.
#' @param pixel_size Numeric value for the size of one pixel in map units (e.g., \code{30} for a 30 × 30 m raster). Default is \code{30}.
#' @param target_crs An integer or character specifying the EPSG code or CRS for the AOI and output raster. Default is \code{3857} (Esri Web Mercator).
#' @param mask Logical; if \code{TRUE}, the merged raster will be masked to the AOI boundary. Default is \code{TRUE}.
#' @param quiet Logical; if \code{TRUE}, suppresses progress messages. Default is \code{FALSE}.
#'
#' @return A \code{SpatRaster} object representing the merged (and optionally masked) raster. The final raster is also saved to \code{output_path}.
#'
#' @details The function works by first transforming the AOI to the specified CRS, calculating the bounding box and total pixel dimensions
#' based on the provided \code{pixel_size}, and then dividing the area into smaller tiles. For each tile, it constructs a query with parameters
#' (including bounding box, image size, spatial reference, etc.) and downloads the tile using the ESRI REST API. After downloading, it merges the
#' tiles and (if requested) masks the merged raster to the AOI.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Define a sample AOI as a bounding box polygon
#' aoi <- st_as_sfc(st_bbox(c(xmin = -100, ymin = 40, xmax = -99, ymax = 41), crs = st_crs(4326)))
#'
#' # Download the raster from an ESRI REST image service
#' raster <- download_esri_raster(aoi,
#'                                base_url = "https://di-usfsdata.img.arcgis.com/arcgis/rest/services/CONUS_site_productivity_2018_masked_202106032103033/ImageServer/exportImage",
#'                                output_path = "conus_productivity.tif")
#' }
#'
#' @export

download_esri_raster <- function(aoi,
                                 base_url,
                                 output_path = "esri_rest_output.tif",
                                 max_tile_size = 20000,
                                 pixel_size = 30,
                                 target_crs = 3857,
                                 mask = TRUE,
                                 quiet = FALSE) {
  # Validate inputs: ensure aoi is an sf/sfc object
  if (!inherits(aoi, c("sf", "sfc"))) {
    stop("aoi must be an sf or sfc object")
  }

  # Transform AOI to the target CRS (default EPSG:3857)
  aoi_transformed <- sf::st_transform(aoi, target_crs)
  bbox <- sf::st_bbox(aoi_transformed)

  if (!quiet) {
    message("Original CRS: ", sf::st_crs(aoi)$input)
    message("Transformed bounds: ", paste(bbox, collapse = ", "))
  }

  # Calculate total dimensions in pixels using the pixel size
  total_width <- as.integer((bbox["xmax"] - bbox["xmin"]) / pixel_size)
  total_height <- as.integer((bbox["ymax"] - bbox["ymin"]) / pixel_size)

  if (!quiet) {
    message(sprintf("Total area: %dx%d pixels", total_width, total_height))
  }

  # Determine the number of tiles along each axis
  n_tiles_x <- ceiling(total_width / max_tile_size)
  n_tiles_y <- ceiling(total_height / max_tile_size)

  # Calculate dimensions for each tile in pixels
  tile_width <- ceiling(total_width / n_tiles_x)
  tile_height <- ceiling(total_height / n_tiles_y)

  if (!quiet) {
    message(sprintf("Breaking into %dx%d tiles", n_tiles_x, n_tiles_y))
    message(sprintf("Base tile dimensions: %dx%d pixels", tile_width, tile_height))
  }

  # Create a temporary directory to hold downloaded tiles
  temp_dir <- tempfile("esri_rest_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  tile_paths <- character()

  # Loop over the grid of tiles and download each one
  for (j in seq_len(n_tiles_y)) {
    for (i in seq_len(n_tiles_x)) {
      # Calculate the geographic bounds for the current tile (in map units)
      x_start <- bbox["xmin"] + ((i - 1) * tile_width * pixel_size)
      x_end   <- if (i == n_tiles_x) bbox["xmax"] else x_start + (tile_width * pixel_size)

      y_end   <- bbox["ymax"] - ((j - 1) * tile_height * pixel_size)
      y_start <- if (j == n_tiles_y) bbox["ymin"] else y_end - (tile_height * pixel_size)

      # Calculate the dimensions of this tile in pixels
      width  <- as.integer((x_end - x_start) / pixel_size)
      height <- as.integer((y_end - y_start) / pixel_size)

      if (!quiet) {
        message(sprintf("\nDownloading tile %d,%d of %d,%d", i, j, n_tiles_x, n_tiles_y))
        message(sprintf("Tile size: %dx%d pixels", width, height))
      }

      # Set up the query parameters for the ESRI REST API call
      params <- list(
        bbox      = paste(x_start, y_start, x_end, y_end, sep = ","),
        bboxSR    = target_crs,
        size      = paste(width, height, sep = ","),
        imageSR   = target_crs,
        format    = "tiff",
        pixelType = "F32",
        noData    = "",
        interpolation = "RSP_BilinearInterpolation",
        f         = "image"
      )

      # Define the file path for this tile
      tile_path <- file.path(temp_dir, sprintf("tile_%d_%d.tif", i, j))
      tile_paths <- c(tile_paths, tile_path)

      # Download the tile using httr
      tryCatch({
        response <- httr::GET(
          base_url,
          query = params,
          httr::write_disk(tile_path, overwrite = TRUE),
          if (!quiet) httr::progress()
        )
        httr::stop_for_status(response)
      }, error = function(e) {
        stop("Error downloading tile: ", e$message)
      })
    }
  }

  if (!quiet) message("\nMerging tiles...")

  # Merge the downloaded tiles using terra
  if (length(tile_paths) == 1) {
    merged_rast <- terra::rast(tile_paths[1])
  } else {
    rast_list <- lapply(tile_paths, terra::rast)
    merged_rast <- terra::merge(rast_list[[1]], rast_list[-1])
  }

  # Optionally, mask the merged raster to the AOI
  if (mask) {
    if (!quiet) message("Masking to AOI boundary...")
    aoi_vect <- terra::vect(sf::st_geometry(aoi_transformed))
    merged_rast <- terra::mask(merged_rast, aoi_vect)
  }

  # Write the final raster to disk
  terra::writeRaster(merged_rast, output_path, overwrite = TRUE)

  if (!quiet) message("Final raster saved to ", output_path)

  return(merged_rast)
}
