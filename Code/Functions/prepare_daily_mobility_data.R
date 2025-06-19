# --- --- --- --- --- --- --- --- --- --- --- ---
# 
# --- --- --- --- --- --- --- --- --- --- --- ---

# Placer AI has 99 sites with daily data and 29 with weekly data.

# Lets use mobiltiy data for a subset of 99 sites with daily data: 

# Load required libraries
library(dplyr)
library(stringr)
library(lubridate)
library(fuzzyjoin)
require(tidyverse)

# # Load Lauren time period of study sites
puzzles  = read.csv('/Users/diegoellis/Downloads/NewStantonPuzzleStudyInfo_06172025.csv') |>
  mutate(states_abbrev ='CA')

# Set the directory containing the CSV files
folder_path <- "/Users/diegoellis/Downloads/PlacerDownloads/"

# List all CSV files in the directory
all_csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# # Keep only those containing "Daily"
# daily_files <- grep("Daily", all_csv_files, value = TRUE)
# 
# # Exclude any that might also contain "Weekly" (if you want to ensure no overlap)
# daily_files <- daily_files[!grepl("Weekly", daily_files)]

# my_vec_trimmed <- substr(basename(all_csv_files), 7, nchar(all_csv_files))
# extract = name = gsub('Daily Visits Trend 2017-01-01 - 2025-01-31.csv','',my_vec_trimmed)
# extract = name = gsub('Weekly Visits Trend 2017-01-01 - 2025-01-31.csv','',extract)
# extract = name = gsub(' Weekly Visits Trend 2017-01-02 - 2025-01-20.csv','',extract)
# extract = name = gsub('Weekly Visits Trend 2017-01-01 - 2025-01-01.csv','',extract)
# extract = name = gsub('Daliy Visits Trend 2017-01-01 - 2025-01-31.csv','',extract)
# # Helper function to extract name:
# extract_name <- function(full_colname) {
#   # Remove leading "W0001." or "WXXXX."
#   no_prefix <- str_remove(full_colname, "^W\\d{4}\\.")
#   
#   # Split by dots
#   tokens <- str_split(no_prefix, "\\.")[[1]]
#   
#   # Remove empty tokens caused by consecutive dots
#   non_empty <- tokens[tokens != ""]
#   
#   # For this example, we take the first two pieces to form "Ahern.Home" etc.
#   # Adjust if you need more or fewer pieces.
#   if (length(non_empty) >= 2) {
#     name_val <- paste(non_empty[1:2], collapse = ".")
#   } else {
#     name_val <- no_prefix
#   }
#   
#   return(name_val)
# }

# file_path
# all_csv_files
# df <- read.csv(all_csv_files[1], header = TRUE, stringsAsFactors = FALSE)

process_csv <- function(file_path) {
  # Read the CSV
  df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  
my_vec_trimmed <- substr(basename(file_path), 7, nchar(file_path))
extract = name = gsub('Daily Visits Trend 2017-01-01 - 2025-01-31.csv','',my_vec_trimmed)
extract = name = gsub('Weekly Visits Trend 2017-01-01 - 2025-01-31.csv','',extract)
extract = name = gsub(' Weekly Visits Trend 2017-01-02 - 2025-01-20.csv','',extract)
extract = name = gsub('Weekly Visits Trend 2017-01-01 - 2025-01-01.csv','',extract)
extract = name = gsub('Daliy Visits Trend 2017-01-01 - 2025-01-31.csv','',extract)
# Rename columns to: Date, daily_visits
# original_colname <- colnames(df)[2]
colnames(df) <- c("Date", "daily_visits")
# Extract site name (e.g., "Ahern.Home")

df$Name <- extract
# Convert date to Date format (adjust if your format is not "YYYY-MM-DD")
df$Date <- as.Date(df$Date) 

# Keep only dates between 2022-01-01 and 2023-12-31
df <- df %>%
  filter(between(Date, as.Date("2022-01-01"), as.Date("2023-12-31")))

df$Name <- substr(df$Name, 1, nchar(df$Name) - 1)

return(df)
}

# Apply the process to the filtered daily_files
processed_list <- lapply(all_csv_files, process_csv)

# Combine all into one data frame
combined_data <- bind_rows(processed_list)
head(combined_data)

# Remove the first 6 charaters
# my_vec_trimmed <- substr(basename(all_csv_files), 7, nchar(all_csv_files))
# extract = name = gsub('Daily Visits Trend 2017-01-01 - 2025-01-31.csv','',my_vec_trimmed)
# extract = name = gsub('Weekly Visits Trend 2017-01-01 - 2025-01-31.csv','',extract)
# extract = name = gsub(' Weekly Visits Trend 2017-01-02 - 2025-01-20.csv','',extract)
# extract = name = gsub('Weekly Visits Trend 2017-01-01 - 2025-01-01.csv','',extract)
# extract = name = gsub('Daliy Visits Trend 2017-01-01 - 2025-01-31.csv','',extract)


# process_csv <- function(file_path) {
#   # Read the CSV
#   df <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
#   
#   # Rename columns to: Date, daily_visits
#   original_colname <- colnames(df)[2]
#   colnames(df) <- c("Date", "daily_visits")
#   
#   # Extract site name (e.g., "Ahern.Home")
#   df$Name <- extract_name(original_colname)
#   
#   # Convert date to Date format (adjust if your format is not "YYYY-MM-DD")
#   df$Date <- as.Date(df$Date) 
#   
#   # Keep only dates between 2022-01-01 and 2023-12-31
#   df <- df %>%
#     filter(between(Date, as.Date("2022-01-01"), as.Date("2023-12-31")))
#   
#   return(df)
# }
# 
# all_csv_files
# 
# # Apply the process to the filtered daily_files
# processed_list <- lapply(daily_files, process_csv)
# 
# # Combine all into one data frame
# combined_data <- bind_rows(processed_list)

# Inspect the combined data
# head(combined_data)
# length(unique(combined_data$Name))

write.csv(combined_data, file = 'Outdir/PlacerAI_clean_daily_124sites.csv')


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Get median and mean mobility patterns for entire time period, weekday, weekend
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Load all Placer_AI_clean_daily_sites
# Convert Date in placerAI to Date format (assuming format is "m/d/yy" as shown)
# placerAI = read.csv('Outdir/PlacerAI_clean_daily_97sites.csv')%>%

# placerAI = read.csv('Outdir/PlacerAI_clean_daily_124sites.csv')%>%
#   # mutate(Date = mdy(Date))
#   mutate(Date = ymd(Date))
# placerAI$Name = gsub("\\.", " ", placerAI$Name)

placerAI = combined_data

# Manually curate the total of 35 that did not run properly ! You can find tem through:
# Find mismatches by index
p1 <- unique(puzzles$Site.Name)
p2 <- unique(placerAI$Name)
which(p1 != p2)  # Returns indices of differing elements
p1[which(p1 != p2)]  # Mismatched values from puzzles
p2[which(p1 != p2)]  # Corresponding values from placerAI


# Incorrect and correct names
incorrect <- c(
  "Bates-Lutherman Home", "Beery Hom", "Belgum Trai", "Boker T Anderson Park",
  "Briones 1 Sout", "Briones 2 Nort", "Chabot Space Cente", "Claremont-Clark Kerr Trak",
  "Cleary Hom", "Crown Memorial Stat Beach", "Hamilton Hom", "Huckleberry Botanic Regional Preserv",
  "Kennedy Grov", "Lake Chabot Valley", "Lake Chabot Dumpster", "Maon Home",
  "Mason Home", "Miller Knox-Picklewee", "Osborne Hom", "Oz Cali Trail",
  "Oz Creek", "Oz SEKI Gate", "Point Molate Beack Par", "Point Pinole Wes",
  "Rifle Range Road Trailhea", "Roberts Regional-Graham Trai", "Scharnagi Home", "The Dog Social Club Cooperative",
  "Tilden Nature Area-Jewel Lak", "Tilden Nature Area-Loop Roa", "Tilden Regional Park-Arroyo Road Parkin", "Tilden Regional Park-Lake Anz",
  "Tilden Regional Park-Nimitz Wa", "Tilden Regional Park-Seaview Trai", "University Village Community"
)

correct <- c(
  "Bates-Luterman Home", "Beery Home", "Belgum Trail", "Booker T. Anderson Park",
  "Briones 1 South", "Briones 2 North", "Chabot Space Center", "Claremont-Clark Kerr Track",
  "Cleary Home", "Crown Memorial State Beach", "Hamilton Home", "Huckleberry Botanic Regional Preserve",
  "Kennedy Grove", "Lake Chabot Valley", "Lake Chabot Dumpster", "Mason Home",
  "McKinley Park", "Miller Knox-Pickleweed", "Osborne Home", "OZ Cali Trail",
  "OZ Creek", "OZ SEKI Gate", "Point Molate Beach Park", "Point Pinole West",
  "Rifle Range Road Trailhead", "Roberts Regional-Graham Trail", "Scharnagl Home", "The Dog Social Club",
  "Tilden Nature Area-Jewel Lake", "Tilden Nature Area-Loop Road Parking", "Tilden Regional Park-Arroyo Pkwy", "Tilden Regional Park-Lake Anza",
  "Tilden Regional Park-Nimitz Way", "Tilden Regional Park-Seaview Trail", "University Village Community Garden"
)

# Replace incorrect names in placerAI$Name
placerAI$Name <- ifelse(placerAI$Name %in% incorrect,
                        correct[match(placerAI$Name, incorrect)],
                        placerAI$Name)


# Double check now:

# Find mismatches by index
p1 <- unique(puzzles$Site.Name)
p2 <- unique(placerAI$Name)
which(p1 != p2)  # Returns indices of differing elements
p1[which(p1 != p2)]  # Mismatched values from puzzles
p2[which(p1 != p2)]  # Corresponding values from placerAI
# Is null now can move on: 

all(unique(puzzles$Site.Name) == unique(placerAI$Name))

setdiff(unique(placerAI$Name), unique(puzzles$Site.Name))

# site_time = read.csv('Indir/StantonPuzzleStudyDates_05152025 (1).csv') %>%
#   mutate(Start.Date = mdy(Start.Date),
#          End.Date = mdy(End.Date))

# unique(site_time$Site.Name)
# unique(placerAI$Name)

# Clean Site.Name and Name columns to match (remove spaces, special chars, case-insensitive)

# Merge site_time info with placerAI based on site name and date ranges
# We'll use a fuzzy join for matching date ranges per site

# Create an empty data frame to hold results
results <- data.frame(Site.Name = puzzles$Site.Name,
                      mean_daily_visits = NA,
                      median_daily_visits = NA)

results <- data.frame(
  Site.Name = puzzles$Site.Name,
  mean_daily_visits = NA,
  median_daily_visits = NA,
  mean_weekday_visits = NA,   # new column
  mean_weekend_visits = NA    # new column
)

# Loop over each row in site_time

# site_time |> filter(Site.Name =='University Village Community Garden')
#                                 'University Village Community Garden'
for (i in 1:nrow(puzzles)) {
  # i = 1
  # i = 116
  print(i)
  # Get the current site name, start, and end date
  this_site <- puzzles$Site.Name[i]
  start_date <- mdy(puzzles$Start.Date[i])
  end_date <- mdy(puzzles$End.Date[i])
  
  # Filter placerAI for matching Name and dates
  subset_data <- placerAI[placerAI$Name == this_site &
                            placerAI$Date >= start_date &
                            placerAI$Date <= end_date, ]
  
  
  # Identify weekdays and weekends
  subset_data$day_type <- ifelse(weekdays(subset_data$Date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
  
  # Calculate mean and median daily visits
  results$mean_daily_visits[i] <- mean(subset_data$daily_visits, na.rm = TRUE)
  results$median_daily_visits[i] <- median(subset_data$daily_visits, na.rm = TRUE)
  
  # Calculate mean for weekdays and weekends
  results$mean_weekday_visits[i] <- mean(subset_data$daily_visits[subset_data$day_type == "weekday"], na.rm = TRUE)
  results$mean_weekend_visits[i] <- mean(subset_data$daily_visits[subset_data$day_type == "weekend"], na.rm = TRUE)
  
}
# See the results
print(results)
# Filter the sites with fully NA values
na_sites <- results[is.na(results$mean_daily_visits) & is.na(results$median_daily_visits), ]
# Print the site names
na_sites
results_v2 = results
  
results_v2 |> write_csv('Outdir/added_mobility_data.csv')
print('Mobility data for sites stored at Outdir/added_mobility_data.csv')
