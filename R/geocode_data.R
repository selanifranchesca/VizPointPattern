#' Geocode Contamination Sample Addresses
#' This function takes a data frame with an address column and performs
#' geocoding using the 'osm' method from `tidygeocoder`. It then filters out
#' rows where geocoding failed (missing latitude/longitude) and converts
#' the resulting data into an `sf` (simple features) object for spatial analysis.
#' @param data A data frame containing the addresses to be geocoded.
#'   This should typically be the output from `clean_contamination_data()`,
#'   which already has a united address column.
#' @param address_col A character string specifying the name of the column
#'   in `data` that contains the full addresses to be geocoded.
#' @param lat_col_name A character string specifying the desired name for the
#'   latitude column in the geocoded output (default: "latitude").
#' @param lon_col_name A character string specifying the desired name for the
#'   longitude column in the geocoded output (default: "longitude").
#' @param crs_value An integer or character string specifying the Coordinate
#'   Reference System (CRS) for the output `sf` object (default: 4269, WGS84).
#' @param verbose Logical. If TRUE, `tidygeocoder` will print messages
#'   during the geocoding process (default: FALSE).
#' @return An `sf` object (simple features data frame) with geocoded coordinates
#'   and filtered out rows where geocoding was unsuccessful. The geometry column
#'   will be points representing the geocoded locations.
#' @export
#' @importFrom tidygeocoder geocode
#' @importFrom dplyr filter
#' @importFrom sf st_as_sf
#' @importFrom rlang sym
#' @importFrom purrr discard
#' @importFrom magrittr %>%
geocode_contamination_data <- function(
    data,
    address_col,
    lat_col_name = "latitude",
    lon_col_name = "longitude",
    crs_value = 4269, # WGS84, common for lat/lon
    verbose = FALSE
) {
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(address_col) || length(address_col) != 1) {
    stop("`address_col` must be a single character string specifying the address column name.")
  }
  if (!(address_col %in% names(data))) {
    stop(paste0("Address column '", address_col, "' not found in the input data frame."))
  }

  # --- Geocoding ---
  message("Starting geocoding process...")
  # Use rlang::sym() for dynamic column names in tidygeocoder::geocode
  lat_longs <- data %>%
    tidygeocoder::geocode(
      address = !!rlang::sym(address_col), # Dynamically select address column
      method = 'osm',
      lat = !!rlang::sym(lat_col_name),   # Dynamically set output latitude column name
      long = !!rlang::sym(lon_col_name),  # Dynamically set output longitude column name
      verbose = verbose
    )
  message("Geocoding complete.")

  # --- Filter out unsuccessful geocodes ---
  initial_rows <- nrow(lat_longs)
  lat_longs_filtered <- lat_longs %>%
    dplyr::filter(!is.na(!!rlang::sym(lat_col_name)) & !is.na(!!rlang::sym(lon_col_name)))

  num_dropped <- initial_rows - nrow(lat_longs_filtered)
  if (num_dropped > 0) {
    message(paste0("Removed ", num_dropped, " rows where geocoding was unsuccessful (missing lat/lon)."))
  } else {
    message("All rows were successfully geocoded.")
  }

  # --- Convert to sf object ---
  # Check if longitude and latitude columns exist after filtering
  if (!(lon_col_name %in% names(lat_longs_filtered) && lat_col_name %in% names(lat_longs_filtered))) {
    stop(paste0("Expected longitude ('", lon_col_name, "') or latitude ('", lat_col_name, "') column not found after geocoding and filtering. This indicates a problem with geocoder output or column naming."))
  }

  # Convert to sf object
  geocoded_sf <- lat_longs_filtered %>%
    sf::st_as_sf(coords = c(lon_col_name, lat_col_name), crs = crs_value)

  message("Data converted to sf object with CRS: ", crs_value)

  return(geocoded_sf)
}
