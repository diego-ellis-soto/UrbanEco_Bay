# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load roads
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# https://gis.stackexchange.com/questions/119993/convert-line-shapefile-to-raster-value-total-length-of-lines-within-cell
# https://catalog.data.gov/dataset/enviroatlas-road-density-metrics-by-12-digit-huc-for-the-conterminous-united-states3
require(tigris)
options(tigris_use_cache = TRUE)

# Download road data for Alameda and Contra Costa counties in California
alameda_roads <- roads(state = "CA", county = "Alameda", year = 2020)
contra_costa_roads <- roads(state = "CA", county = "Contra Costa", year = 2020)
# Combine the road data
all_roads <- rbind(alameda_roads, contra_costa_roads)
t_all_roads =  st_transform(all_roads, st_crs(puzzles_lauren_sf))

# Find an appropriate UTM zone for your data
utm_crs <- st_crs("+proj=utm +zone=10 +datum=WGS84") # Example for California

puzzles_lauren_sf_utm <- st_transform(puzzles_lauren_sf, utm_crs)
t_all_roads_utm <- st_transform(t_all_roads, utm_crs)

puzzles_lauren_sf_utm_buffer <- st_buffer(puzzles_lauren_sf_utm,
                                                  dist = 1000)

intersected_roads <- st_intersection(puzzles_lauren_sf_utm_buffer,
                                     t_all_roads_utm)
# # Add a new column for road length (in meters, if CRS is projected)
intersected_roads$length <- st_length(intersected_roads)

# Summarize the total road length within each polygon
road_lengths_by_location <- intersected_roads %>%
  st_set_geometry(NULL) %>% # Remove geometry for summarization
  group_by(Name) %>% # Replace `location_id` with your unique identifier
  summarize(total_road_length = sum(as.numeric(length), na.rm = TRUE))


puzzles_lauren_sf_utm_buffer$area <- st_area(puzzles_lauren_sf_utm_buffer)
# Join the total road length back to the original polygons
puzzles_lauren_sf_utm_buffer <- puzzles_lauren_sf_utm_buffer %>%
  left_join(road_lengths_by_location, by = "Name")

# Calculate road density (road length per unit area, e.g., meters per square kilometer)
puzzles_lauren_sf_utm_buffer$road_density =
  puzzles_lauren_sf_utm_buffer$total_road_length / 
  puzzles_lauren_sf_utm_buffer$area

puzzles_lauren_sf_utm_buffer_df = puzzles_lauren_sf_utm_buffer |> as.tibble() |>
  dplyr::select(Name, total_road_length, road_density, area)
