#' Download Oregon Canopy Height Model (CHM) Data
#'
#' Downloads CHM LiDAR data from DOGAMI's REST API service for a specified area of interest.
#' The Canopy Height Model represents the height of vegetation above ground level,
#' derived from the difference between Digital Surface Model (DSM) and Digital Terrain
#' Model (DTM).
#'
#' @param aoi sf or sfc object representing the area of interest
#' @param output_path Character string specifying the output file path
#' @param max_tile_size Numeric value specifying the maximum tile size in pixels
#' @param mask Logical indicating whether to mask the output to the AOI boundary
#' @param quiet Logical indicating whether to suppress progress messages
#' @return Path to the downloaded and merged CHM raster file
#' @details
#' The CHM data has a 3-meter resolution and represents vegetation height in meters.
#' Values typically range from 0 (ground level) to 60+ meters (tall trees).
#' The data is provided by Oregon Department of Geology and Mineral Industries (DOGAMI).
#'
#' @export
#' @importFrom sf st_transform st_bbox st_geometry
#' @importFrom terra rast merge writeRaster mask vect
#' @importFrom httr GET write_disk progress
#' @examples
#' \dontrun{
#' library(sf)
#' aoi <- st_read("path/to/aoi.gpkg")
#' # Download and mask to exact AOI boundary
#' chm_path <- download_oregon_chm(aoi, "chm_output.tif", mask = TRUE)
#' }
download_oregon_chm<- function(aoi, output_path = "oregon_lidar_output.tif",
                                max_tile_size = 2000, mask = TRUE, quiet = FALSE) {
  # Validate inputs
  if (!inherits(aoi, c("sf", "sfc"))) {
    stop("aoi must be an sf or sfc object")
  }

  # Transform to Oregon State Plane (EPSG:2913)
  aoi_transformed <- sf::st_transform(aoi, 2913)
  bbox <- sf::st_bbox(aoi_transformed)

  if (!quiet) {
    message("Original CRS: ", sf::st_crs(aoi)$input)
    message("Transformed bounds: ", paste(bbox, collapse = ", "))
  }

  # Calculate dimensions
  total_width <- as.integer((bbox["xmax"] - bbox["xmin"]) / 3)  # 3 meter pixels
  total_height <- as.integer((bbox["ymax"] - bbox["ymin"]) / 3)

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
  temp_dir <- tempfile("oregon_lidar_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Base URL for the image service
  base_url <- paste0("https://gis.dogami.oregon.gov/arcgis/rest/services/",
                     "lidar/CANOPY_HEIGHT_MODEL_MOSAIC/ImageServer/exportImage")

  # Store all tile paths
  tile_paths <- character()

  # Download tiles
  for (j in seq_len(n_tiles_y)) {
    for (i in seq_len(n_tiles_x)) {
      # Calculate tile bounds using actual dimensions
      x_start <- bbox["xmin"] + ((i - 1) * tile_width * 3)
      x_end <- if(i == n_tiles_x) bbox["xmax"] else x_start + (tile_width * 3)

      y_end <- bbox["ymax"] - ((j - 1) * tile_height * 3)
      y_start <- if(j == n_tiles_y) bbox["ymin"] else y_end - (tile_height * 3)

      # Calculate actual dimensions for this tile
      width <- as.integer((x_end - x_start) / 3)
      height <- as.integer((y_end - y_start) / 3)

      if (!quiet) {
        message(sprintf("\nDownloading tile %d,%d of %d,%d", i, j, n_tiles_x, n_tiles_y))
        message(sprintf("Tile size: %dx%d pixels", width, height))
      }

      # Construct query parameters
      params <- list(
        bbox = paste(x_start, y_start, x_end, y_end, sep = ","),
        bboxSR = 2913,
        size = paste(width, height, sep = ","),
        imageSR = 2913,
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

