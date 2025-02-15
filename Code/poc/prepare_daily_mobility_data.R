# --- --- --- --- --- --- --- --- --- --- --- ---
# 
# --- --- --- --- --- --- --- --- --- --- --- ---

# Placer AI has 99 sites with daily data and 29 with weekly data.

# Lets use mobiltiy data for a subset of 99 sites with daily data: 

# Load required libraries
library(dplyr)
library(stringr)
library(lubridate)

# Set the directory containing the CSV files
folder_path <- "/Users/diegoellis/Downloads/PlacerDownloads/"

# List all CSV files in the directory
all_csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Keep only those containing "Daily"
daily_files <- grep("Daily", all_csv_files, value = TRUE)

# Exclude any that might also contain "Weekly" (if you want to ensure no overlap)
daily_files <- daily_files[!grepl("Weekly", daily_files)]

# Helper function to extract name:
extract_name <- function(full_colname) {
  # Remove leading "W0001." or "WXXXX."
  no_prefix <- str_remove(full_colname, "^W\\d{4}\\.")
  
  # Split by dots
  tokens <- str_split(no_prefix, "\\.")[[1]]
  
  # Remove empty tokens caused by consecutive dots
  non_empty <- tokens[tokens != ""]
  
  # For this example, we take the first two pieces to form "Ahern.Home" etc.
  # Adjust if you need more or fewer pieces.
  if (length(non_empty) >= 2) {
    name_val <- paste(non_empty[1:2], collapse = ".")
  } else {
    name_val <- no_prefix
  }
  
  return(name_val)
}


process_csv <- function(file_path) {
  # Read the CSV
  df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  
  # Rename columns to: Date, daily_visits
  original_colname <- colnames(df)[2]
  colnames(df) <- c("Date", "daily_visits")
  
  # Extract site name (e.g., "Ahern.Home")
  df$Name <- extract_name(original_colname)
  
  # Convert date to Date format (adjust if your format is not "YYYY-MM-DD")
  df$Date <- as.Date(df$Date) 
  
  # Keep only dates between 2022-01-01 and 2023-12-31
  df <- df %>%
    filter(between(Date, as.Date("2022-01-01"), as.Date("2023-12-31")))
  
  return(df)
}



# Apply the process to the filtered daily_files
processed_list <- lapply(daily_files, process_csv)

# Combine all into one data frame
combined_data <- bind_rows(processed_list)

# Inspect the combined data
head(combined_data)
length(unique(combined_data$Name))

write.csv(combined_data, file = 'Outdir/PlacerAI_clean_daily_97sites.csv')


