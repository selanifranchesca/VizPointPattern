#' Visualize Contamination Data on an Interactive Map and Perform Spatial Analysis
#' This comprehensive function filters geocoded contamination data by boundaries,
#' calculates Moran's I for spatial autocorrelation, and generates an interactive
#' Leaflet map with circle markers representing the contamination sample points.
#' @param sf_data An `sf` object (simple features data frame) with point geometries
#'    and numeric contamination data, typically the output from `geocode_contamination_data()`.
#' @param contamination_col Character string. Name of the numeric column in `sf_data`
#'    that contains the contamination values (e.g., "lead_ppb", "arsenic_mg_kg").
#' @param boundary_lon_min Numeric. Minimum longitude for filtering data.
#' @param boundary_lon_max Numeric. Maximum longitude for filtering data.
#' @param boundary_lat_min Numeric. Minimum latitude for filtering data.
#' @param boundary_lat_max Numeric. Maximum latitude for filtering data.
#' @param lat_col_name Character string. Name of the latitude column in `sf_data`
#'    (e.g., "latitude"). Used for internal filtering.
#' @param lon_col_name Character string. Name of the longitude column in `sf_data`
#'    (e.g., "longitude"). Used for internal filtering.
#' @param color_palette Character string or vector. The RColorBrewer palette name
#'    (e.g., "YlOrRd", "Greens") or a vector of colors for the contamination gradient.
#'    Defaults to "YlOrRd".
#' @param legend_title Character string. Title for the map legend (e.g., "Lead (PPB)").
#' @param map_title Character string. Overall title displayed on the map (optional).
#' @param map_zoom Integer. Initial zoom level for the Leaflet map (default: 12).
#' @param circle_marker_radius Numeric. Radius of the circle markers on the map (default: 5).
#' @param morans_k_neighbors Integer. Number of nearest neighbors for Moran's I calculation (default: 8).
#' @param morans_alternative Character string. Alternative hypothesis for Moran's I
#'    ("greater", "less", "two.sided", default: "greater").
#' @param sample_id_col Character string. Name of the column with sample IDs for labels (default: "Sample.Number").
#' @return A `leaflet` map object. Also prints Moran's I results to the console.
#' @export
#' @importFrom leaflet leaflet addTiles addLegend addCircleMarkers addLayersControl layersControlOptions addScaleBar addEasyButton easyButton JS addControl
#' @importFrom dplyr filter mutate
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang sym
#' @importFrom stats na.omit
#' @importFrom magrittr %>%
#' @importFrom rlang :=
plot_contamination_map <- function(
    sf_data,
    contamination_col,
    boundary_lon_min, boundary_lon_max,
    boundary_lat_min, boundary_lat_max,
    lat_col_name = "latitude",
    lon_col_name = "longitude",
    color_palette = "YlOrRd",
    legend_title = "Contamination (Units)",
    map_title = NULL,
    map_zoom = 12,
    circle_marker_radius = 5,
    morans_k_neighbors = 8,
    morans_alternative = "greater",
    sample_id_col = "Sample.Number" # Assuming you have a sample ID column
) {
  # --- 0. Initial Input Validation & Preparation ---
  if (!inherits(sf_data, "sf")) {
    stop("Input `sf_data` must be an sf object.")
  }
  if (!(contamination_col %in% names(sf_data))) {
    stop(paste0("Contamination column '", contamination_col, "' not found in `sf_data`."))
  }
  if (!is.numeric(sf_data[[contamination_col]])) {
    stop(paste0("Contamination column '", contamination_col, "' must be numeric."))
  }
  if (!(sample_id_col %in% names(sf_data))) {
    warning(paste0("Sample ID column '", sample_id_col, "' not found. Map labels will use row numbers or omit sample ID."))
    sample_id_col_exists <- FALSE
  } else {
    sample_id_col_exists <- TRUE
  }

  # Ensure contamination column is numeric and filter out NA values at this stage
  sf_data_clean <- sf_data %>%
    dplyr::mutate(!!rlang::sym(contamination_col) := as.numeric(!!rlang::sym(contamination_col))) %>%
    dplyr::filter(!is.na(!!rlang::sym(contamination_col)))

  if (nrow(sf_data_clean) == 0) {
    stop("No valid data points for analysis after cleaning and NA filtering of contamination column.")
  }

  # --- 1. Filter by Boundaries (using the helper function) ---
  # FIX: Removed the extra `(...)` after the function name
  filtered_data <- VizPointPattern::filter_spatial_boundaries(
    sf_data = sf_data_clean,
    lat_min = boundary_lat_min,
    lat_max = boundary_lat_max,
    lon_min = boundary_lon_min,
    lon_max = boundary_lon_max,
    lat_col_name = lat_col_name,
    lon_col_name = lon_col_name
  )

  # Check again if data remains after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remaining after boundary filtering. Adjust boundaries or check input data.")
  }

  # --- 2. Prepare Color Palette for point markers ---
  pal_points_domain <- range(filtered_data[[contamination_col]], na.rm = TRUE)
  if (length(pal_points_domain) == 0 || any(is.infinite(pal_points_domain))) {
    warning("Domain for point color palette is empty or infinite. Defaulting to a fixed range (0-100).")
    pal_points_domain <- c(0, 100) # Fallback range
  }

  if (is.character(color_palette) && length(color_palette) == 1 && color_palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    # Use RColorBrewer palette
    point_color_func <- leaflet::colorNumeric(
      palette = color_palette,
      domain = pal_points_domain
    )
  } else {
    # Use custom color vector or default if RColorBrewer name is invalid
    if (is.character(color_palette) && length(color_palette) > 1) {
      message("Using custom color vector for palette.")
    } else {
      warning("Invalid RColorBrewer palette name or color vector. Defaulting to 'YlOrRd'.")
      color_palette <- "YlOrRd" # Fallback
    }
    point_color_func <- leaflet::colorNumeric(
      palette = color_palette,
      domain = pal_points_domain
    )
  }

  # --- 3. Calculate Moran's I Statistic (using the helper function) ---
  moran_output <- VizPointPattern::perform_morans_i(
    sf_data = filtered_data,
    contamination_col = contamination_col,
    k_neighbors = morans_k_neighbors,
    alternative = morans_alternative
  )
  moran_result <- moran_output$moran_test_result


  # --- 4. Build Leaflet Map with only Circle Markers ---
  map_obj <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      data = as.data.frame(filtered_data), # Convert back to data.frame for leaflet's simple mapping
      lng = ~get(lon_col_name), # Dynamic column selection
      lat = ~get(lat_col_name), # Dynamic column selection
      radius = circle_marker_radius,
      color = ~point_color_func(get(contamination_col)), # Dynamic column selection
      stroke = FALSE,
      fillOpacity = 0.8,
      label = ~paste0(
        if(sample_id_col_exists) paste("Sample:", get(sample_id_col), "<br>") else "",
        "Contamination: ", get(contamination_col)
      ),
      group = "Sample Points"
    ) %>%
    # Add legend only for the circle markers
    leaflet::addLegend(
      "bottomright",
      pal = point_color_func,
      values = pal_points_domain,
      title = legend_title,
      opacity = 0.7,
      labFormat = leaflet::labelFormat(suffix = ""), # Removed "ppb" to generalize
      className = "small-legend"
    ) %>%
    # No more overlayGroups for heatmap, just the single group for points
    leaflet::addLayersControl(
      overlayGroups = c("Sample Points"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) %>%
    leaflet::addScaleBar(position = "bottomleft") %>%
    leaflet::addEasyButton(leaflet::easyButton(
      icon = "fa-globe", title = "Reset View",
      onClick = leaflet::JS(paste0("function(btn, map){ map.setView([",
                                   (boundary_lat_min + boundary_lat_max) / 2, ", ",
                                   (boundary_lon_min + boundary_lon_max) / 2, "], ",
                                   map_zoom, "); }"))
    ))

  # Add a title if provided
  if (!is.null(map_title)) {
    map_obj <- map_obj %>%
      leaflet::addControl(html = paste0("<h3>", map_title, "</h3>"), position = "topleft")
  }

  message("Map generated successfully. Check the RStudio Viewer or open in browser.")
  return(map_obj)
}
