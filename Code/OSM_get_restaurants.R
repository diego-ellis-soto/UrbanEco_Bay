# --- --- --- --- --- --- --- --- --- --- --- ---
# Leverage OSM for Bay Area
# Explore restaurant density
# --- --- --- --- --- --- --- --- --- --- --- ---

# https://jcoliver.github.io/learn-r/017-open-street-map.html

# Input: bbox of study area
# csv file with locations (UWIN, Lauren)
# Buffer size
# Output: Annotated .csv with restaurant density
require(mapview)
library(sf)
library(dplyr)
library(ggplot2)
require(osmdata)
require(ggplot2)
require(viridisLite)
require(viridis)
available_tags(feature = "water")
available_tags(feature = "wood")
available_tags(feature = "highway")

# East Bay
bay_area_bbox <- c(xmin = -123.1738, ymin = 37.5398, xmax = -122.0818, ymax = 37.9998)
# Larger Bay Area
# bay_area_bbox <- c(xmin = -123.00, ymin = 37.00, xmax = -121.50, ymax = 38.50)
crssa=4326
A
get_osm_restaurants = function(bbox_area){
  rest <- opq(bbox = bbox_area) %>%
    add_osm_feature(key = "amenity", value = "restaurant") %>%
    osmdata_sf()
}

get_osm_street = function(bbox_area){
  street <- opq(bbox = bbox_area) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()
}

rest_count_1km_grid = function(OSM_points_rest){
  
  # Query for water bodies in the Bay Area
  Bay_Area_water <- opq(bbox = bay_area_bbox) %>%
    add_osm_feature(key = "natural", value = "water") %>%
    osmdata_sf()
  
  # Transform to a projected CRS (e.g., UTM zone 10N for SF)
  SF_rest_proj <- st_transform(OSM_points_rest, 32610)  
  # Create a 1 km grid  
  grid <- st_make_grid(SF_rest_proj, cellsize = 1000, square = TRUE)
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  # Count restaurants per grid cell
  # Join points to the grid and count the number of points in each grid cell
  restaurants_count <- st_join(grid_sf, SF_rest_proj) %>%
    group_by(grid_id) %>%
    summarize(restaurant_count = n(), .groups = "drop")
  
  ggplot(data = restaurants_count) +
    geom_sf(aes(fill = restaurant_count)) +
    scale_fill_viridis_c(option = "plasma", na.value = "white") +
    labs(
      title = "Number of Restaurants per 1km² Grid Cell",
      fill = "Restaurant Count"
    ) +
    theme_minimal()
  
}

buffer_rest_counts = function(df_sf, rest_osm_sf, buffer_size){
  # Transform to a projected CRS for accurate distance calculations (e.g., UTM zone 10N for the Bay Area)
  df_sf_proj <- st_transform(df_sf, crs = 32610)
  rest_osm_sf_proj <- st_transform(rest_osm_sf, crs = 32610)
  
  df_buffer <- st_buffer(df_sf_proj, dist = buffer_size)
  # Count number of restaurants within buffer
  # restaurant_counts <- st_join(df_sf_proj, rest_osm_sf_proj, join = st_intersection) %>%
  restaurant_counts <- st_join(df_buffer, rest_osm_sf_proj, join = st_intersects) %>%
    group_by(Name) %>%
    summarize(restaurant_count = n(), .groups = "drop")
}


puzzles_lauren_sf = read.csv('/Users/diegoellis/Downloads/StantonPuzzleStudyLocations_11042024.csv') |>
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

rest_BA = get_osm_restaurants(bay_area_bbox)

Bay_Area_street = get_osm_street(bay_area_bbox)

puzzles_lauren_sf_buffered = buffer_rest_counts(
  df = puzzles_lauren_sf,
  rest_osm_sf = rest_BA$osm_points,
  buffer_size = 1000)

puzzles_lauren_sf_buffered |> data.frame() |> select(-geometry) |> 
  left_join(puzzles_lauren_sf, by = 'Name') |>
  write.csv(file = '/Users/diegoellis/Desktop/Projects/Postdoc/OSM_for_Ecology/puzzles_lauren_sf_buffered_restaurant_1km.csv')



mapview(df_buffer,
        col.regions = "blue",
        layer.name = "Puzzle Locations",
        legend = TRUE,
        cex = 8
        ) +
  mapview(rest_BA$osm_points,
          col.regions = "green",
          legend = TRUE,
          cex = 4,
          alpha = 0.7)

require(mapview)

# dream policy: reimagining sf + justice 40 + biodiv data + gaps identify + 
# Number of restaurants within 1km buffer of UWin Locations and within Laurens rasters
# Input:

mapview(puzzles_lauren_sf_buffered, 
        zcol = "restaurant_count", 
        layer.name = "Restaurant Count per Buffer", 
        col.regions = viridis::viridis, 
        alpha.regions = 0.8)

# Agreement with iNaturalist and eBird + movement
# -> correlation trail counter information with movement from placer AI has good correlation
# Get agreement with citizen science apps 