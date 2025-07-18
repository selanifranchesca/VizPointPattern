utils::globalVariables(c("Value", "Measurement_Type"))

#' Plot Contamination Measurement Histograms
#' This function generates histograms for specified contamination measurement columns
#' in a given data frame. It can plot multiple histograms, either individually
#' or faceted, and allows customization of titles and labels.
#' @param data A data frame containing the contamination measurements.
#'   This should typically be the output from `clean_contamination_data()`.
#' @param measurement_cols A character vector of column names from `data`
#'   that contain the numeric contamination measurements you wish to plot.
#' @param plot_title A character string for the overall title of the plot.
#'   Defaults to "Distribution of Contamination Measurements".
#' @param x_axis_label A character string for the X-axis label.
#'   Defaults to "Contamination Level (PPB)".
#' @param fill_color A character string specifying the fill color of the histogram bars.
#'   Defaults to "steelblue".
#' @param bins An integer specifying the number of bins for the histogram.
#'   Defaults to 30.
#' @param facet_plots Logical. If TRUE, creates separate plots (facets) for each
#'   measurement column. If FALSE, plots individual histograms one by one.
#'   Defaults to TRUE.
#' @return A ggplot object if `facet_plots` is TRUE, otherwise it prints
#'   individual base R histogram plots.
#' @export
#' @importFrom ggplot2 ggplot aes geom_histogram labs facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select
#' @importFrom graphics hist par mtext
#' @importFrom grDevices dev.off
#' @importFrom magrittr %>%
# For internal use if creating multiple plots
plot_contamination_histograms <- function(
    data,
    measurement_cols,
    plot_title = "Distribution of Contamination Measurements",
    x_axis_label = "Contamination Level (PPB)",
    fill_color = "steelblue",
    bins = 30,
    facet_plots = TRUE
) {
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!is.character(measurement_cols) || length(measurement_cols) == 0) {
    stop("`measurement_cols` must be a character vector of at least one column name.")
  }

  # Check if all specified measurement columns exist and are numeric
  missing_cols <- setdiff(measurement_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following `measurement_cols` were not found in the data: ",
         paste(missing_cols, collapse = ", "))
  }

  non_numeric_cols <- measurement_cols[!sapply(data[measurement_cols], is.numeric)]
  if (length(non_numeric_cols) > 0) {
    warning("The following `measurement_cols` are not numeric and will be excluded: ",
            paste(non_numeric_cols, collapse = ", "))
    measurement_cols <- setdiff(measurement_cols, non_numeric_cols)
  }

  if (length(measurement_cols) == 0) {
    stop("No valid numeric measurement columns provided for plotting.")
  }


  # --- Plotting Logic ---
  if (facet_plots) {
    # Prepare data for ggplot2's facet_wrap: pivot to long format
    data_long <- data %>%
      dplyr::select(dplyr::all_of(measurement_cols)) %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "Measurement_Type",
        values_to = "Value"
      ) %>%
      # Filter out NA values for plotting
      dplyr::filter(!is.na(Value))

    if (nrow(data_long) == 0) {
      warning("No valid data points to plot after filtering NA values for faceted histograms.")
      return(NULL) # Or return an empty ggplot object
    }

    # Create faceted histograms using ggplot2
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = Value)) +
      ggplot2::geom_histogram(binwidth = (max(data_long$Value, na.rm = TRUE) - min(data_long$Value, na.rm = TRUE)) / bins,
                              fill = fill_color, color = "white", na.rm = TRUE) +
      ggplot2::labs(title = plot_title, x = x_axis_label, y = "Frequency") +
      ggplot2::facet_wrap(~ Measurement_Type, scales = "free_x") + # free_x allows different x-axes for different ranges
      ggplot2::theme_minimal()

    print(p)
    return(p) # Return the ggplot object for further manipulation
  } else {
    # Plot individual histograms using base R graphics
    # Set up plotting area
    num_plots <- length(measurement_cols)
    if (num_plots > 0) {
      # Dynamically set layout based on number of plots
      # Max 2 columns for better readability if many plots
      ncols <- min(2, num_plots)
      nrows <- ceiling(num_plots / ncols)
      graphics::par(mfrow = c(nrows, ncols), oma = c(0, 0, 3, 0), mar = c(4, 4, 2, 1) + 0.1) # oma for outer margin for main title

      for (col_name in measurement_cols) {
        values <- data[[col_name]]
        # Filter out NA values for plotting
        values <- values[!is.na(values)]

        if (length(values) > 0) {
          graphics::hist(values,
                         main = paste(col_name, "Distribution"),
                         xlab = x_axis_label,
                         ylab = "Frequency",
                         col = fill_color,
                         breaks = bins)
        } else {
          warning("No valid data points to plot for column: ", col_name)
          # You might want to plot an empty histogram or just skip.
          # For now, we'll skip to avoid error.
        }
      }
      # Add overall title if more than one plot
      if (num_plots > 1) {
        graphics::mtext(plot_title, side = 3, line = 1, outer = TRUE, cex = 1.2, font = 2)
      }
      graphics::par(mfrow = c(1, 1)) # Reset plotting layout
    } else {
      message("No numeric columns available for plotting individual histograms.")
    }
    return(invisible(NULL)) # Returns nothing explicitly, as plots are printed to device
  }
}
