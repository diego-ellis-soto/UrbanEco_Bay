# Weekend effect and biodiversity access and hollidays?
# Weekend effect !!!!! -> Did puzzle change during weekends?!
# EMAIL SONG GAO
# EMAIL PRESS PULSE PAUSE
# EMAIL WALTER
# We can get this at the census block group or census block level?


# HOW MUCH DO PEOPLE VISIT NATIONAL PARKS IN THE WEEKENDS VS WEEKDAY AND WHAT TIMES OF YEAR ARE PEAK AND HOW DOES THIS OVERLAP WITH BIOVIERSITY AND AREAS OF CONCERN?! -> WHERE IT CAN BE MOST IMPACTFUL TO MAKE ADJUSTMENTS?
library(sf)
library(tigris)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)  # for read_csv
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(sf)
library(tidycensus)
library(stringr)


df_buf = puzzles_lauren_sf |> st_buffer(1000)
ca_bgs <- block_groups(state = "CA", year = 2020) 
ca_bgs <- st_transform(ca_bgs, crs = st_crs(df_buf))

# st_join will attach block group attributes (including GEOID) to each point that falls within the polygon.
puzzles_lauren_with_geoid <- st_join(df_buf, ca_bgs, left = TRUE)
# puzzles_lauren_with_geoid now has a GEOID column from the block group polygons
target_cbgs <- unique(puzzles_lauren_with_geoid$GEOID)

# Path to your data directory
data_dir <- "/Users/diegoellis/Downloads/Mobility/CA_Human_Wildlife/2019/"

# List all files that match the pattern YYYY-MM-DD_cbg_daily.txt
files <- list.files(data_dir, pattern = "\\d{4}-\\d{2}-\\d{2}_cbg_daily\\.txt", full.names = TRUE)

# Function to read and process a single file
read_daily_file <- function(file_path) {
  # Extract date from filename using regex
  # Assuming filename format like "2021-01-03_cbg_daily.txt"
  date_str <- str_extract(basename(file_path), "\\d{4}-\\d{2}-\\d{2}")
  date_val <- as.Date(date_str, format = "%Y-%m-%d")
  
  df <- read_csv(file_path, col_types = cols(cpad_cbg = col_character(), device_count = col_double()))
  
  df <- df %>%
    mutate(date = date_val)
  
  return(df)
}

# Read all daily files into one data frame for laurens locations:
all_data <- do.call(bind_rows, lapply(files, read_daily_file))

# Filter for target CBGs only
filtered_data <- all_data %>%
  filter(cpad_cbg %in% target_cbgs) # We have data for all the census block groups!!! # Ask Song if we can jsut generate this coarsened at the census block level? 

daily_data = filtered_data |> group_by(date) |> summarize(daily_avg = mean(device_count, na.rm = TRUE), .groups = "drop")

ggplot(daily_data, aes(x = date, y = daily_avg, color = daily_avg)) +
  geom_line() +
  labs(title = "Monthly Average Visits by census block group laurens locks",
       x = "Month",
       y = "Average Device Count") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(daily_data, aes(x = date, y = daily_avg, color = daily_avg)) +
  geom_line() +
  labs(title = "Monthly Average Visits by census block group laurens locks",
       x = "Month",
       y = "Average Device Count") +
  theme_minimal() +
  theme(legend.position = "none")

daily_data_v2 <- daily_data %>%
  mutate(
    day_of_week = wday(date, label = TRUE, abbr = TRUE),
    day_type = case_when(
      day_of_week %in% c("Sat", "Sun") ~ "Weekend",
      day_of_week %in% c("Thu", "Fri") ~ "Thu-Fri",
      TRUE ~ "Weekday"
    ),
    # Make day_type a factor for consistent ordering
    day_type = factor(day_type, levels = c("Weekday", "Thu-Fri", "Weekend"))
  )

ggplot(daily_data_v2, aes(x = date, y = daily_avg, color = day_type)) +
  geom_line() +
  labs(
    title = "Monthly Average Visits by Census Block Group Laurens Locks",
    x = "Month",
    y = "Average Device Count"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("Weekday" = "blue", "Thu-Fri" = "green", "Weekend" = "red"))

ggplot(daily_data_v2, aes(y = daily_avg, color = day_type)) +
  geom_boxplot()+theme_classic()+ggtitle('Daily average by day of the week')

daily_data_v2 |> group_by(day_type) |> summarize(mean(daily_avg, na.rm=TRUE))

# MONTGLT:

# Create a year-month column for monthly aggregation
filtered_data <- filtered_data %>%
  mutate(year_month = floor_date(date, "month"))

# Summarize monthly data

monthly_summary <- filtered_data %>%
  group_by(year_month) %>%
  summarize(monthly_avg = mean(device_count, na.rm = TRUE), .groups = "drop")


# Plot monthly patterns
# Example: a line plot of monthly averages over time, faceted by CBG.
ggplot(monthly_summary, aes(x = year_month, y = monthly_avg, color = monthly_avg)) +
  geom_line() +
  labs(title = "Monthly Average Visits by CBG",
       x = "Month",
       y = "Average Device Count") +
  theme_minimal() +
  theme(legend.position = "none")

# For example, we can take the mean device_count per month per CBG.
monthly_summary <- filtered_data %>%
  group_by(cpad_cbg, year_month) %>%
  summarize(monthly_avg = mean(device_count, na.rm = TRUE), .groups = "drop")


# If you prefer a single combined line plot with a legend:
ggplot(monthly_summary, aes(x = year_month, y = monthly_avg, color = cpad_cbg)) +
  geom_line() +
  labs(title = "Monthly Average Visits by CBG Lauren Puzzle",
       x = "Month",
       y = "Average Device Count",
       color = "CBG") +
  theme_minimal()+theme(
strip.text = element_text(size = 14, face = "bold"),  # Style for facet labels
legend.position = "none"
)


ggplot(monthly_summary, aes(x = year_month, y = log(monthly_avg), color = cpad_cbg)) +
  geom_line() +
  labs(title = "Monthly Average Visits by CBG Lauren Puzzle",
       x = "Month",
       y = "Average Device Count",
       color = "CBG") +
  theme_minimal()+theme(
    strip.text = element_text(size = 14, face = "bold"),  # Style for facet labels
    legend.position = "none"
  )


# Census API Key (replace with your own key)
# census_api_key("YOUR_CENSUS_API_KEY", install=TRUE)

# Your previously created monthly_summary dataset:
# monthly_summary has columns: cpad_cbg, year_month, monthly_avg
# Let's rename cpad_cbg -> GEOID for joining with census data
monthly_summary <- monthly_summary %>%
  rename(GEOID = cpad_cbg)

# Determine which counties to pull data for.
# Extract county codes from your CBGs. The first 5 digits of the GEOID
# represent state+county (state=06 for CA). For instance:
unique_counties <- unique(substr(monthly_summary$GEOID, 1, 5))
# This gives you all unique county codes. You'll need to map them to county names.
# For example:
# 06001 = Alameda County
# 06013 = Contra Costa County
# If you have multiple counties, list them:
counties <- c("Alameda", "Contra Costa")  # adjust as needed

# Download block group geometries for the relevant counties using tidycensus
# Using the decennial Census or ACS depending on your preference:
bg_geo <- get_acs(geography = "block group", 
                  variables = "B01001_001E", # population var just to get geometry
                  year = 2019,               # choose a year with available data
                  state = "CA", 
                  county = counties,
                  geometry = TRUE) %>%
  select(GEOID, geometry) %>%
  st_as_sf()

# Join the monthly_summary data with the block group geometries
bg_monthly <- bg_geo %>%
  inner_join(monthly_summary, by = "GEOID") 

# Ensure year_month is a date or factor that gganimate can iterate over
# If it's already a Date, this might be fine. If you want monthly labels:
bg_monthly <- bg_monthly %>%
  mutate(month_label = as.factor(format(year_month, "%Y-%m")))

# Create a static ggplot of one month as a test
p <- ggplot(bg_monthly, aes(fill = monthly_avg, geometry = geometry)) +
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Monthly Human Mobility Patterns", 
       subtitle = "Month: {closest_state}",
       fill = "Avg Device Count") +
  theme_minimal()


library(gganimate)

# Assuming bg_monthly has a month_label column that is a factor or a character 
# representing each month, e.g. "2021-01", "2021-02", ...

bg_monthly <- bg_monthly %>%
  mutate(year_month = as.numeric(year_month))


ggplot(bg_monthly) +
  geom_sf(aes(fill = monthly_avg, geometry = geometry), color = NA) + 
#  geom_sf(data = puzzles_lauren_sf, color = 'red', size = 0.5) +
  scale_fill_viridis_c(option = "plasma", name = "Avg Device Count") +
  labs(title = "Monthly Human Mobility Patterns", 
       fill = "Avg Device Count") +
  theme_minimal() +
  facet_wrap(~year_month) +
  theme(legend.position = "bottom")



ggplot(bg_monthly, aes(fill = monthly_avg, geometry = geometry)) +
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Monthly Human Mobility Patterns", 
       subtitle = "Month: {closest_state}",
       fill = "Avg Device Count") +
  theme_minimal() + facet_grid(~year_month)+ geom_sf(puzzles_lauren_sf_buf)
  

ggplot(bg_monthly, aes(fill = monthly_avg, geometry = geometry)) +
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Monthly Human Mobility Patterns", 
       subtitle = "Month: {closest_state}",
       fill = "Avg Device Count") +
  theme_minimal() +
  transition_states(month_label, transition_length = 2, state_length = 1) +
  ease_aes('linear')
  
  


# Animate and save if desired
my_anim <- animate(p, nframes = length(unique(bg_monthly$month_label)) * 5, fps = 5)
anim_save("monthly_mobility_patterns.gif", animation = my_anim)



# Now add gganimate to animate over time (month_label)
# transition_states will allow month-by-month transitions
animated_map <- p +
  transition_states(month_label, transition_length = 2, state_length = 1) +
  ease_aes('linear')

# Animate the plot and store the result
my_anim <- animate(
  animated_map, 
  nframes = length(unique(bg_monthly$month_label)) * 5, 
  fps = 5
)

anim_save("monthly_mobility_patterns.gif", animation = my_anim)


# Save the animation from the stored object
anim_save("monthly_mobility_patterns.gif", animation = my_anim)



# Render the animation
# You can use animate() function from gganimate to control frames and duration
animate(animated_map, nframes = length(unique(bg_monthly$month_label)) * 5, fps = 5)

# Save the animation as GIF if desired:
anim_save("monthly_mobility_patterns.gif", animation = last_animation())

# Look at mobility patterns for different zones across California ! -> 




# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#
#
# performing a spatial downscaling, using census block geometry and some form of weighting (e.g., population proportion) to allocate block group totals to constituent blocks.
#
#
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
library(tigris)
library(sf)
# Get all census blocks for California
ca_blocks <- blocks(state = "CA", year = 2020)
ca_blocks <- st_transform(ca_blocks, crs = st_crs(puzzles_lauren_sf))

# Block GEOIDs are usually 15 digits: STATE (2) + COUNTY (3) + TRACT (6) + BLOCK GROUP (1) + BLOCK (3)
# Block group GEOID: first 12 digits (STATE+COUNTY+TRACT+BLOCKGROUP)
ca_blocks <- ca_blocks %>%
  mutate(BLKGRP_GEOID = substr(GEOID20, 1, 12))

# Suppose your monthly_summary still has cpad_cbg representing block group GEOIDs

monthly_summary_censusblock <- monthly_summary %>%
  rename(GEOID_BG = cpad_cbg)

ca_blocks_v2 <- ca_blocks %>%
  left_join(monthly_summary_censusblock, by = c("BLKGRP_GEOID" = "GEOID_BG"))


# Get block-level population from redistricting data (PL 94-171) for CA
# Note: This requires setting geometry = FALSE and year = 2020. 
# The decennial "pl" data can provide block level population.
ca_block_pop <- get_decennial(
  geography = "block",
  state = "CA",
  variables = "P1_001N",  # Total population from PL94-171
  year = 2020,
  geometry = FALSE
)

# Join population to ca_blocks
ca_blocks_v3 <- ca_blocks_v2 %>%
  left_join(ca_block_pop, by = c("GEOID20" = "GEOID")) %>%
  rename(block_population = value)

# Calculate the total population in each block group (sum of all blocks within it)
blockgroup_pop <- ca_blocks_v3 %>%
  group_by(BLKGRP_GEOID) %>%
  summarize(bg_population = sum(block_population, na.rm = TRUE), .groups="drop")

blockgroup_pop_tibble = blockgroup_pop |> as_tibble() |> dplyr::select(-geometry)

ca_blocks_v4 <- ca_blocks_v3 %>%
  left_join(blockgroup_pop_tibble, by = "BLKGRP_GEOID")

# Now downscale the monthly_avg visits to blocks proportionally by population:
# block_monthly_avg = monthly_avg (bg level) * (block_population / bg_population)
ca_blocks_v5 <- ca_blocks_v4 %>%
  mutate(block_estimate = monthly_avg * (block_population / bg_population))


# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Weekend effect for a subset of the plots?!
# --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# subset to oakland
puzzle_bbox <- st_bbox(puzzles_lauren_sf)

ggplot(ca_blocks_v5, aes(fill = block_estimate, geometry = geometry)) +
  geom_sf(color = NA) +
  geom_sf(data = puzzles_lauren_sf, color = 'red', size = 0.5) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Estimated Monthly Human Mobility Patterns at Block Level",
       fill = "Estimated Device Count") +
  theme_minimal() +
  coord_sf(
    xlim = c(puzzle_bbox["xmin"], puzzle_bbox["xmax"]), 
    ylim = c(puzzle_bbox["ymin"], puzzle_bbox["ymax"]),
    expand = FALSE
  ) 



ggplot(ca_blocks_v5, aes(fill = block_estimate, geometry = geometry)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Estimated Monthly Human Mobility Patterns at Block Level",
       fill = "Estimated Device Count") +
  theme_minimal()

# Show activity weekend weekday - thursday friday?