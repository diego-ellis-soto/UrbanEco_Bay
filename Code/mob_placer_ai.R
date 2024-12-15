# --- --- --- --- --- --- --- --- --- --- --- ---
# Human Mobility - Placer AI Lauren Camara
# --- --- --- --- --- --- --- --- --- --- --- ---

# Ver como lo hicimos en el pasado! - Hacer gg animate

library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(tigris)
library(osmdata)
library(sf)
library(mapview)

# Annotate block grous 
ca_bgs <- block_groups(state = "CA", year = 2020) # Download 2020 block groups for CA
ca_bgs <- st_transform(ca_bgs, crs = st_crs(puzzles_lauren_sf))
puzzles_lauren_with_geoid <- st_join(puzzles_lauren_sf, ca_bgs, left = TRUE)

# Lake Chabot:
lake_chatot_100percent_within_50miles = st_read('/Users/diegoellis/Downloads/Mobility/LakeChabotLast12Months/Lake Chabot_TradeArea_100Percent_within50miles.shp')
lake_chatot_75percent_within_50miles = st_read('/Users/diegoellis/Downloads/Mobility/LakeChabotLast12Months/Lake Chabot_TradeArea_75Percent_within50miles.shp')
lake_chatot_25percent_within_50miles = st_read('/Users/diegoellis/Downloads/Mobility/LakeChabotLast12Months/Lake Chabot_TradeArea_25Percent_within50miles.shp')
lake_chatot_nationwide = st_read('/Users/diegoellis/Downloads/Mobility/LakeChabotLast12Months/Lake Chabot_TradeArea_CBGs_Nationwide.shp')

# Daily trends in visitors:
daily_lake_chabot = read.csv('/Users/diegoellis/Downloads/Mobility/LakeChabotLast12Months/Lake Chabot Daily Visits Trend 2023-12-01 - 2024-11-30.csv')

lake_c_csv = read.csv('/Users/diegoellis/Downloads/Mobility/LakeChabotLast12Months/Lake Chabot  CBG Export - Timeframe Dec 1st 2023  Nov 30th 2024.csv')

# Annual Mobility patterns of the park!
lake_c_csv_sf = st_as_sf(lake_c_csv, coords = c("lng", "lat"), crs = 4326)
lake_c_csv_sf_buf = lake_c_csv_sf |> st_buffer(1000)

lake_c_csv_sf_geoid <- st_join(lake_c_csv_sf_buf, ca_bgs, left = TRUE)

unique(lake_c_csv_sf_geoid$GEOID)

mapview(lake_c_csv_sf_geoid,fill='red') + mapview(lake_chatot_75percent_within_50miles)

aa = mapview(lake_chatot_25percent_within_50miles) 

bb =  mapview(lake_c_csv_sf_buf)

aa + bb 

# ADD WALKABILIYY INDEX AND TRANPORT INDEX !!!!

head(lake_c_csv)

# Also hourly location visits: Show Chris and Lauren
# Questions for Lauren: Study site -> This is AWESOME TO TEST THE WEEKEND EFFECT !!!! Altough only at one site unfortuantely?

# ADD SAFEGRAPH ! -> DAILY FOR THE CENSUS BLOCK GROUPS OF LAURENS LOCSATION! -> WEEKLY SCORES 

# Weekend effect !!!!! -> Make a GG animate of mobility data 


# --- --- --- --- --- --- --- --- --- ---
# Plot the weekend effect !
# --- --- --- --- --- --- --- --- --- ---

df = daily_lake_chabot |>
  mutate(date = as.Date(X.),
         visits = Lake.Chabot...17600.Lake.Chabot.Rd..Castro.Valley..CA.94546...01.12.23...30.11.24) %>%
  mutate(weekend = if_else(wday(date) %in% c(1,7), "Weekend", "Weekday"))



ggplot(df, aes(x = date, y = visits, color = weekend)) +
  geom_line() +
  labs(title = "Visitation Patterns Over Time Lake Chabot",
       x = "Date",
       y = "Number of Visits",
       color = "Day Type") +
  theme_minimal()

# Alternatively, you could plot average visits by day type over time:

# Create df_summary
df_summary <- df %>%
  group_by(weekend, date) %>%
  summarize(avg_visits = mean(visits, na.rm = TRUE), .groups = "drop")

ggplot(df_summary, aes(x = date, y = avg_visits, color = weekend)) +
  geom_line() +
  labs(title = "Average Daily Visits by Weekend/Weekday",
       x = "Date",
       y = "Average Visits",
       color = "Day Type") +
  theme_minimal()


# pal8 <- c("#1F78B4",  "#FDBF6F",



df_summary <- df %>%
  group_by(weekend, date) %>%
  summarize(avg_visits = mean(visits, na.rm = TRUE), .groups = "drop")

ggplot(df_summary, aes(x = date, y = avg_visits, color = weekend)) +
  geom_line() +
  labs(title = "Average Daily Visits by Weekend/Weekday",
       x = "Date",
       y = "Average Visits",
       color = "Day Type") +
  theme_minimal()

# If you want to highlight differences more clearly, you could plot a boxplot:
ggplot(df, aes(x = weekend, y = visits, fill = weekend)) +
  geom_boxplot() +
  labs(title = "Distribution of Visits by Weekend vs Weekday",
       x = "Day Type",
       y = "Number of Visits") +
  theme_minimal() +
  scale_fill_manual(values = c("Weekday" = "skyblue", "Weekend" = "salmon"))

# Lake Chabot is located in the East Bay region near Oakland, CA.
# Approximate bounding box around Lake Chabot:
bbox <- c(-122.137, 37.70, -122.09, 37.73) 
# Query OSM for features named "Lake Chabot"
lake_chabot_query <- opq(bbox = bbox) %>%
  add_osm_feature(key = "name", value = "Lake Chabot")

lake_chabot_data <- osmdata_sf(lake_chabot_query)

# Extract the polygons layer where water bodies usually appear
lake_chabot_poly <- lake_chabot_data$osm_multipolygons

# If multiple features are returned, you can filter by `name`:
lake_chabot_poly <- subset(lake_chabot_poly, name == "Lake Chabot")

# Visualize the lake using mapview

mapview(lake_chabot_poly, legend = TRUE) + mapview(lake_c_csv_sf_buf)+mapview(lake_chatot_25percent_within_50miles)
  



mapview(lake_chabot_poly, legend = TRUE, col.regions = "blue") + 
  mapview(puzzles_lauren_with_geoid, col.regions = "red") + 
  mapview(lake_chatot_25percent_within_50miles, col.regions = "green")+
mapview(lake_chatot_75percent_within_50miles, col.regions = "forestgreen")



# Next safegraph data:

