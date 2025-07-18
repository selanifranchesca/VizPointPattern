#' Clean Contamination Sample Data
#' This function loads, cleans, and processes the Flint water sample data.
#' It renames columns, removes irrelevant ones, handles missing values,
#' and unites address components for geocoding.
#' @param file_path A character string specifying the path to the Excel data file.
#' @param sheet The sheet number or name to read from the Excel file (default: 1).
#' @param contamination_col_mapping A named character vector or list.
#'   Names should be the desired new column names (e.g., "Lead", "Copper_250ml"),
#'   and values should be the existing column names in the raw data
#'   (e.g., "Analysis..Lead.", "X250.ml.Bottle..PPB.").
#' @param address_cols A character vector of column names containing address components
#'   to be united (e.g., c("Street", "Street.Name", "City", "Zip.Code")).
#' @param other_cols_to_drop A character vector of any other column names
#'   to drop from the dataset (e.g., "Analysis..Copper."). If NULL, no
#'   additional columns are dropped.
#' @param unite_address_col_name The desired name for the united address column (default: "Address").
#' @param remove_address_components Logical. If TRUE, the original address_cols will be removed
#'   after uniting them into a single address column (default: FALSE).
#' @return A tibble (data frame) with cleaned and processed contamination sample data.
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr select rename filter mutate
#' @importFrom tidyr unite
#' @importFrom magrittr %>%
#' @importFrom rlang syms
clean_contamination_data <- function(
    file_path,
    sheet = 1,
    contamination_col_mapping, # This will be the main flexible part for measurements
    address_cols,              # Generalize address components
    other_cols_to_drop = NULL, # Allow dropping other specific columns
    unite_address_col_name = "Address",
    remove_address_components = FALSE
) {
  # --- Input Validation ---
  if (!file.exists(file_path)) {
    stop("File not found at specified path: ", file_path)
  }
  if (!is.character(contamination_col_mapping) || is.null(names(contamination_col_mapping))) {
    stop("`contamination_col_mapping` must be a named character vector/list.")
  }
  if (!is.character(address_cols) || length(address_cols) == 0) {
    stop("`address_cols` must be a character vector of at least one column name.")
  }

  # Load data
  raw_data <- readxl::read_excel(file_path, sheet = sheet, col_names = TRUE)

  # --- Drop specified columns ---
  data_processed <- raw_data

  # First, identify all columns to drop: those in other_cols_to_drop
  # and those that are *source* names in contamination_col_mapping.
  cols_to_drop_final <- unique(c(other_cols_to_drop, as.character(contamination_col_mapping)))

  # Ensure columns to drop actually exist in the data
  existing_cols_to_drop <- cols_to_drop_final[cols_to_drop_final %in% names(data_processed)]
  if (length(existing_cols_to_drop) > 0) {
    message("Dropping columns: ", paste(existing_cols_to_drop, collapse = ", "))
    data_processed <- data_processed %>%
      dplyr::select(-dplyr::all_of(existing_cols_to_drop))
  } else {
    message("No specified columns to drop were found in the dataset or are already handled by renaming.")
  }


  # --- Rename contamination columns based on mapping ---
  # We only rename the *target* names in the data_processed.
  # The original names will have been dropped if they were in cols_to_drop_final
  # or if they are simply being replaced by the renaming operation.
  # Use `any_of` for renaming in case some source columns don't exist
  rename_list <- names(contamination_col_mapping) # The new names
  names(rename_list) <- as.character(contamination_col_mapping) # The old names

  data_processed <- data_processed %>%
    dplyr::rename(!!!syms(rename_list)) # Use `!!!syms` for dynamic renaming


  # --- Remove rows with all NA or empty values ---
  # This filter applies to all columns. Consider if you want to be more specific.
  data_processed <- data_processed[!apply(is.na(data_processed) | data_processed == "", 1, all),]
  message("Removed rows with all NA or empty values.")

  # --- Unite address columns ---
  # Ensure address_cols exist before uniting
  existing_address_cols <- address_cols[address_cols %in% names(data_processed)]
  if (length(existing_address_cols) < length(address_cols)) {
    warning("Some specified address columns were not found in the data: ",
            paste(setdiff(address_cols, existing_address_cols), collapse = ", "))
  }
  if (length(existing_address_cols) > 0) {
    data_processed <- data_processed %>%
      tidyr::unite(
        !!sym(unite_address_col_name), # Use !!sym for dynamic column name
        dplyr::all_of(existing_address_cols), # Use all_of to select columns by name
        sep = " ",
        remove = remove_address_components # Control if original columns are removed
      )
    message("United address columns into '", unite_address_col_name, "'.")
  } else {
    message("No valid address columns found to unite.")
  }


  # Final check: Convert any character columns that should be numeric
  # (e.g., contamination_col_mapping target names) to numeric.
  # This is a common issue with data loading.
  for (new_col_name in names(contamination_col_mapping)) {
    if (new_col_name %in% names(data_processed)) {
      data_processed[[new_col_name]] <- as.numeric(data_processed[[new_col_name]])
    }
  }


  return(data_processed)
}
