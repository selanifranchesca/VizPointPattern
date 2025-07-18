#' Perform Moran's I Test for Spatial Autocorrelation
#'
#' Calculates Moran's I statistic for a given contamination variable and
#' provides an interpretation of the result based on the p-value.
#'
#' @param sf_data An `sf` object with point geometries and a numeric contamination column.
#' @param contamination_col Character string. Name of the numeric column in `sf_data`
#'   that contains the contamination values.
#' @param k_neighbors Integer. Number of nearest neighbors to consider for
#'   constructing the spatial weights matrix (default: 8).
#' @param alternative Character string. Alternative hypothesis for Moran's I
#'   test: "greater", "less", or "two.sided" (default: "greater").
#' @return A list containing:
#'   - `moran_test_result`: The raw result object from `spdep::moran.test()`.
#'   - `interpretation`: A character string interpreting the Moran's I result.
#' @export
#' @importFrom spdep knearneigh knn2nb nb2listw moran.test card
#' @importFrom methods as
#' @importFrom stats na.omit
perform_morans_i <- function(
    sf_data,
    contamination_col,
    k_neighbors = 8,
    alternative = "greater"
) {
  # Validate input
  if (!inherits(sf_data, "sf")) {
    stop("`sf_data` must be an sf object.")
  }
  if (!contamination_col %in% names(sf_data)) {
    stop(paste0("Contamination column '", contamination_col, "' not found in sf_data."))
  }
  if (!is.numeric(sf_data[[contamination_col]])) {
    stop(paste0("Contamination column '", contamination_col, "' must be numeric."))
  }
  if (nrow(sf_data) < 2) { # Moran's I requires at least 2 points
    stop("Insufficient data points for Moran's I calculation (need at least 2).")
  }

  # Ensure data is clean of NAs in the contamination column for Moran's I
  data_for_moran <- sf_data[!is.na(sf_data[[contamination_col]]), ]
  if (nrow(data_for_moran) < 2) {
    stop("Insufficient valid data points (after NA removal) for Moran's I calculation.")
  }

  # Convert sf object to sp object for spdep package
  sp_data <- as(data_for_moran, "Spatial")

  # Extract contamination values
  contamination_values <- sp_data[[contamination_col]]

  # Create k-nearest neighbors list (spatial weights)
  # Ensure k_neighbors is not greater than the number of available points - 1
  actual_k_neighbors <- min(k_neighbors, nrow(sp_data) - 1)
  if (actual_k_neighbors < 1) {
    stop("Cannot form a neighbor list with fewer than 2 points or k_neighbors too small.")
  }

  nn <- spdep::knearneigh(sp_data, k = actual_k_neighbors)
  nb <- spdep::knn2nb(nn)

  # Check if there are any isolated points (no neighbors)
  if (any(spdep::card(nb) == 0)) {
    warning("Some points have no neighbors. Consider adjusting k_neighbors or data density.")
    # You might want to filter out isolated points or adjust neighbor definition
    # For now, we proceed, but this can lead to issues with spatial weights matrix
  }

  # Create a listw (list of spatial weights) object
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE) # zero.policy = TRUE to handle no-neighbor points

  # Perform Moran's I test
  moran_test_result <- spdep::moran.test(contamination_values, lw, alternative = alternative)

  # Interpret the result
  interpretation <- paste0(
    "Moran's I: ", round(moran_test_result$estimate["Moran I"], 4), "\n",
    "P-value: ", format.p(moran_test_result$p.value), "\n"
  )

  if (moran_test_result$p.value < 0.05) {
    if (moran_test_result$estimate["Moran I"] > 0) {
      interpretation <- paste0(interpretation, "Result: Significant positive spatial autocorrelation (values tend to cluster).\n")
    } else {
      interpretation <- paste0(interpretation, "Result: Significant negative spatial autocorrelation (values tend to be dispersed).\n")
    }
  } else {
    interpretation <- paste0(interpretation, "Result: No significant spatial autocorrelation (values are randomly distributed).\n")
  }

  message(interpretation)

  return(list(
    moran_test_result = moran_test_result,
    interpretation = interpretation
  ))
}

#' Helper function for formatting p-values.
#' @param p Numeric. A p-value.
#' @return Character string. Formatted p-value.
#' @keywords internal
format.p <- function(p) {
  if (is.na(p)) {
    return("NA")
  } else if (p < 0.001) {
    return("< 0.001")
  } else {
    return(as.character(round(p, 3)))
  }
}
