create_mixed_dataframe <- function(n_rows = 10000, n_factor_cols = 15, n_other_cols = 15) {

  create_factor_column <- function(levels, n = n_rows) {
    factor(sample(levels, n, replace = TRUE))
  }

  factor_levels <- list(
    c("Low", "Medium", "High"),
    c("Red", "Green", "Blue"),
    c("Yes", "No"),
    c("A", "B", "C"),
    c("One", "Two", "Three"),
    c("Cat", "Dog", "Bird"),
    c("Small", "Medium", "Large"),
    c("Alpha", "Beta", "Gamma"),
    c("True", "False"),
    c("Left", "Right"),
    c("North", "South", "East", "West"),
    c("Day", "Night"),
    c("Up", "Down"),
    c("Male", "Female"),
    c("Summer", "Winter", "Spring", "Fall")
  )

  factor_columns <- map_dfc(factor_levels[1:n_factor_cols], create_factor_column)
  colnames(factor_columns) <- paste0("fac_col", 1:n_factor_cols)

  # Function to create random numeric, integer, or string columns
  create_other_column <- function(type, n = n_rows) {
    switch(type,
           "int" = sample(1:100, n, replace = TRUE),
           "num" = runif(n, 0, 100),
           "str" = sample(letters, n, replace = TRUE)
    )
  }

  column_types <- c(
    "int", "int", "num", "num", "str",
    "str", "int", "num", "str", "int",
    "num", "str", "int", "num", "str"
  )

  other_columns <- map_dfc(column_types[1:n_other_cols], create_other_column)
  colnames(other_columns) <- paste0("col", (n_factor_cols + 1):(n_factor_cols + n_other_cols))
  df <- bind_cols(factor_columns, other_columns)

  return(df)
}

# Load necessary libraries
library(dplyr)
library(purrr)

# Function to modify factor levels for partial overlap
partial_overlap_levels <- function(levels1, levels2) {
  common <- intersect(levels1, levels2)
  unique1 <- setdiff(levels1, common)
  unique2 <- setdiff(levels2, common)
  new_levels <- c(common, sample(unique1, length(unique1) * 0.5), sample(unique2, length(unique2) * 0.5))
  return(new_levels)
}

# Function to create additional dataframes with specific conditions
create_additional_dataframes <- function(base_df, n_rows = 10000, df_names = c("df1", "df2", "df3")) {

  # Create three new dataframes based on the base dataframe
  df_list <- list()

  for (i in seq_along(df_names)) {
    # Select 80% of the columns to overlap
    overlap_cols <- sample(colnames(base_df), size = round(0.8 * ncol(base_df)))

    # Create a copy of the base dataframe
    df <- base_df

    # Modify 20% of the overlapping columns to have different classes
    cols_to_modify_class <- sample(overlap_cols, size = round(0.2 * length(overlap_cols)))

    for (col in cols_to_modify_class) {
      current_class <- class(df[[col]])
      new_class <- switch(current_class,
                          "factor" = as.character(df[[col]]),   # Factor to character
                          "character" = as.factor(df[[col]]),   # Character to factor
                          "numeric" = as.integer(df[[col]]),    # Numeric to integer
                          "integer" = as.numeric(df[[col]]),    # Integer to numeric
                          df[[col]])                            # Default, keep as is
      df[[col]] <- new_class
    }

    # For 50% of factor columns that overlap, change the levels for partial overlap
    factor_cols <- colnames(base_df)[sapply(base_df, is.factor)]
    overlap_factor_cols <- intersect(overlap_cols, factor_cols)
    cols_to_modify_levels <- sample(overlap_factor_cols, size = round(0.5 * length(overlap_factor_cols)))

    for (col in cols_to_modify_levels) {
      original_levels <- levels(base_df[[col]])
      new_levels <- partial_overlap_levels(original_levels, original_levels)
      df[[col]] <- factor(df[[col]], levels = new_levels)
    }

    # Add 20% new columns with unique names in each dataframe
    n_new_cols <- round(0.2 * ncol(base_df))
    new_col_names <- paste0(df_names[i], "_new_col_", 1:n_new_cols)  # Unique column names per dataframe
    new_cols <- data.frame(matrix(runif(n_rows * n_new_cols), ncol = n_new_cols))
    colnames(new_cols) <- new_col_names

    # Combine new columns with the dataframe
    df <- bind_cols(df, new_cols)

    # Store the dataframe in the list
    df_list[[df_names[i]]] <- df
  }

  return(df_list)
}
