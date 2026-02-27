# R Data Cleaning Script for insurance.csv
# Usage: source('r_data_cleaning.R')

input_path <- "insurance.csv"
output_path <- "insurance_clean.csv"

clean_insurance_data <- function(df) {
  # Standardize column names
  names(df) <- trimws(tolower(names(df)))

  # Trim whitespace for character columns
  char_cols <- vapply(df, is.character, logical(1))
  df[char_cols] <- lapply(df[char_cols], trimws)

  # Convert expected numeric columns safely
  numeric_cols <- c("age", "bmi", "children", "charges")
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
  }

  # Remove duplicate rows
  df <- unique(df)

  # Drop rows with missing required values
  required_cols <- c("age", "sex", "bmi", "children", "smoker", "region", "charges")
  existing_required <- intersect(required_cols, names(df))
  df <- df[complete.cases(df[, existing_required]), , drop = FALSE]

  # Keep only plausible ranges
  df <- subset(
    df,
    age >= 0 & age <= 120 &
      bmi >= 10 & bmi <= 70 &
      children >= 0 &
      charges >= 0
  )

  # Normalize category text
  if ("sex" %in% names(df)) {
    df$sex <- tolower(df$sex)
  }
  if ("smoker" %in% names(df)) {
    df$smoker <- tolower(df$smoker)
  }
  if ("region" %in% names(df)) {
    df$region <- tolower(df$region)
  }

  # Coerce categorical variables to factors
  for (col in c("sex", "smoker", "region")) {
    if (col %in% names(df)) {
      df[[col]] <- as.factor(df[[col]])
    }
  }

  # Children should be integer
  if ("children" %in% names(df)) {
    df$children <- as.integer(round(df$children))
  }

  df
}

if (!file.exists(input_path)) {
  stop(sprintf("Input file not found: %s", input_path))
}

raw_data <- read.csv(input_path, stringsAsFactors = FALSE)
clean_data <- clean_insurance_data(raw_data)

write.csv(clean_data, output_path, row.names = FALSE)

cat("Data cleaning complete.\n")
cat(sprintf("Input rows: %d\n", nrow(raw_data)))
cat(sprintf("Cleaned rows: %d\n", nrow(clean_data)))
cat(sprintf("Saved cleaned file to: %s\n", output_path))
