#' Filter Spatial Data by Bounding Box
#'
#' Filters a spatial (sf) data frame to include only points within a specified
#' bounding box defined by min/max latitude and longitude.
#'
#' @param sf_data An `sf` object (simple features data frame) with point geometries
#'   and latitude/longitude columns.
#' @param lat_min Numeric. Minimum latitude for filtering data.
#' @param lat_max Numeric. Maximum latitude for filtering data.
#' @param lon_min Numeric. Minimum longitude for filtering data.
#' @param lon_max Numeric. Maximum longitude for filtering data.
#' @param lat_col_name Character string. Name of the latitude column in `sf_data`
#'   (e.g., "latitude"). Used for internal filtering.
#' @param lon_col_name Character string. Name of the longitude column in `sf_data`
#'   (e.g., "longitude"). Used for internal filtering.
#' @return An `sf` object containing only the points within the specified bounds.
#' @export
#' @importFrom dplyr filter
#' @importFrom rlang sym
filter_spatial_boundaries <- function(
    sf_data,
    lat_min, lat_max,
    lon_min, lon_max,
    lat_col_name = "latitude",
    lon_col_name = "longitude"
) {
  # Input validation (can be more robust if needed)
  if (!inherits(sf_data, "sf")) {
    stop("`sf_data` must be an sf object.")
  }
  if (!all(c(lat_col_name, lon_col_name) %in% names(sf_data))) {
    stop(paste0("Latitude ('", lat_col_name, "') or longitude ('", lon_col_name, "') column not found in `sf_data`."))
  }

  filtered_data <- sf_data %>%
    dplyr::filter(
      !!rlang::sym(lat_col_name) >= lat_min,
      !!rlang::sym(lat_col_name) <= lat_max,
      !!rlang::sym(lon_col_name) >= lon_min,
      !!rlang::sym(lon_col_name) <= lon_max
    )

  if (nrow(filtered_data) == 0) {
    warning("No data points remaining after spatial boundary filtering.")
  }

  return(filtered_data)
}
