#' USFS CONUS Site Productivity 2018 Downloader
#'
#' @description
#' This downloads the USFS CONUS Site Productivity layer from the USFS ArcGIS REST API server.
#' The site productivity raster is a 30x30 m grid that ranks the site from 1 - 7.
#'
#' From the USFS:
#' This image service was developed using data from over 213,000 national forest inventory plots measured during the period 2014-2018 from the USFS Forest Inventory and Analysis (FIA) program, in conjunction with other auxiliary information. Roughly 4,900 Landsat 8 OLI scenes, collected during the same time period, were processed to extract information about vegetation phenology. This information, along with climatic and topographic raster data, were used in an ecological ordination model of tree species. The model produced a feature space of ecological gradients that was then used to impute FIA plots to pixels. The plots imputed to each pixel were then used to assign values of site productivity.
#'
#' Code Description
#' 1 = 225+ cubic feet/acre/year.
#' 2 = 165-224 cubic feet/acre/year.
#" 3 = 120-164 cubic feet/acre/year.
#" 4 = 85-119 cubic feet/acre/year.
#' 5 = 50-84 cubic feet/acre/year.
#' 6 = 20-49 cubic feet/acre/year.
#' 7 = 0-19 cubic feet/acre/year.
#'
#' More information and citation of the data along with methodology can be found here : https://www.arcgis.com/home/item.html?id=b1b10e7890ea4116b863ae790d9b718c
#' @details
#' The raster is 30x30 m in resolution. The REST API allows for a max tile size of 117545 x 214620
#'
#'
#' @param aoi the Area of Interest that you would like to download the site productivity raster for
#' @param output_path A character string specifying the file path for the output raster (GeoTIFF). Default is \code{"esri_rest_output.tif"}.
#' @param max_tile_size Integer specifying the maximum size (in pixels) of each downloaded tile. Default is \code{20000}.
#' @param mask Logical; if \code{TRUE}, the merged raster will be masked to the AOI boundary. Default is \code{TRUE}.
#' @param quiet Logical; if \code{TRUE}, suppresses progress messages. Default is \code{FALSE}.
#' @return A \code{SpatRaster} object representing the merged (and optionally masked) raster. The final raster is also saved to \code{output_path}.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Define a sample AOI as a bounding box polygon
#' aoi <- st_as_sfc(st_bbox(c(xmin = -100, ymin = 40, xmax = -99, ymax = 41), crs = st_crs(4326)))
#'
#' # Download the raster from an ESRI REST image service
#' raster <- ddownload_conus_productivity(aoi,
#'                                output_path = "conus_productivity.tif")
#' }
#'
#'@export

download_conus_productivity <- function(aoi,
                                        output_path = "conus_productivity_output.tif",
                                        max_tile_size = 20000,
                                        mask = TRUE,
                                        quiet = FALSE) {
  # Validate inputs
  if (!inherits(aoi, c("sf", "sfc"))) {
    stop("aoi must be an sf or sfc object")
  }

  # Transform to Esri Web Mercator (EPSG:3857)
  aoi_transformed <- sf::st_transform(aoi, 3857)
  bbox <- sf::st_bbox(aoi_transformed)

  if (!quiet) {
    message("Transformed bounds: ", paste(bbox, collapse = ", "))
  }

  # Calculate dimensions in pixels (30 meter resolution)
  total_width <- as.integer((bbox["xmax"] - bbox["xmin"]) / 30)
  total_height <- as.integer((bbox["ymax"] - bbox["ymin"]) / 30)

  if (!quiet) {
    message(sprintf("Total area: %dx%d pixels", total_width, total_height))
  }

  # Calculate number of tiles based on actual dimensions
  n_tiles_x <- ceiling(total_width / max_tile_size)
  n_tiles_y <- ceiling(total_height / max_tile_size)

  # Calculate actual tile sizes
  tile_width <- ceiling(total_width / n_tiles_x)
  tile_height <- ceiling(total_height / n_tiles_y)

  if (!quiet) {
    message(sprintf("Breaking into %dx%d tiles", n_tiles_x, n_tiles_y))
    message(sprintf("Base tile dimensions: %dx%d pixels", tile_width, tile_height))
  }

  # Create temporary directory for tiles
  temp_dir <- tempfile("conus_productivity_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Base URL for the image service
  base_url <- "https://di-usfsdata.img.arcgis.com/arcgis/rest/services/CONUS_site_productivity_2018_masked_202106032103033/ImageServer/exportImage"

  # Store all tile paths
  tile_paths <- character()

  # Download tiles
  for (j in seq_len(n_tiles_y)) {
    for (i in seq_len(n_tiles_x)) {
      # Calculate tile bounds using actual dimensions (in meters)
      x_start <- bbox["xmin"] + ((i - 1) * tile_width * 30)
      x_end <- if(i == n_tiles_x) bbox["xmax"] else x_start + (tile_width * 30)

      y_end <- bbox["ymax"] - ((j - 1) * tile_height * 30)
      y_start <- if(j == n_tiles_y) bbox["ymin"] else y_end - (tile_height * 30)

      # Calculate actual dimensions for this tile in pixels
      width <- as.integer((x_end - x_start) / 30)
      height <- as.integer((y_end - y_start) / 30)

      if (!quiet) {
        message(sprintf("\nDownloading tile %d,%d of %d,%d", i, j, n_tiles_x, n_tiles_y))
        message(sprintf("Tile size: %dx%d pixels", width, height))
      }

      # Construct query parameters
      params <- list(
        bbox = paste(x_start, y_start, x_end, y_end, sep = ","),
        bboxSR = 3857,
        size = paste(width, height, sep = ","),
        imageSR = 3857,
        format = "tiff",
        pixelType = "F32",
        noData = "",
        interpolation = "RSP_BilinearInterpolation",
        f = "image"
      )

      # Create tile filename
      tile_path <- file.path(temp_dir, sprintf("tile_%d_%d.tif", i, j))
      tile_paths <- c(tile_paths, tile_path)

      # Download tile
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

  # Read and merge tiles
  if (length(tile_paths) == 1) {
    merged_rast <- terra::rast(tile_paths[1])
  } else {
    rast_list <- lapply(tile_paths, terra::rast)
    merged_rast <- terra::merge(rast_list[[1]], rast_list[-1])
  }

  # Mask to AOI if requested
  if (mask) {
    if (!quiet) message("Masking to AOI boundary...")
    aoi_vect <- terra::vect(sf::st_geometry(aoi_transformed))
    merged_rast <- terra::mask(merged_rast, aoi_vect)
  }

  # Write final raster
  terra::writeRaster(merged_rast, output_path, overwrite = TRUE)

  if (!quiet) message("Final raster saved to ", output_path)

  return(merged_rast)
}
