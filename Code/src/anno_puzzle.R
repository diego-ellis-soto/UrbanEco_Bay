# --- --- --- --- --- --- --- --- --- ---
#
#
# For Lauren Stanton, PhD, UC Berkeley
#
# Combine socio-economic variables across buzzle locations:
#
# Contact: diego.ellissoto@berkeley.edu
#
# Annotate a series of environmental and anthropogenic variables to puzzle locations
#
#
# --- --- --- --- --- --- --- --- --- ---

require(tidyverse)
require(sp)
require(sf)
library(corrr)
require(tidycensus)
require(raster)
library(reshape2)
library(units)


puzzles  = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
  mutate(states_abbrev ='CA')

puzzles_lauren_sf = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

puzzle_sp = as(puzzles_lauren_sf, 'Spatial')


# --- --- --- --- --- --- --- --- --- ---
# Load functions:
# --- --- --- --- --- --- --- --- --- ---

source('Code/Functions/fun_income_race_points.R') # Load functions to obtain census income, race, etc  based on a buffer
source('Code/get_income_popden_race.R') # Get pop density and housing density based on a buffer
source('Code/OSM_get_restaurants.R') # restaurant counts within 1000m buffer of locs

source('Code/load_roads.R') # Still needs work
source('Code/load_rs_vars.R') # Load most of the remote sensing data


# dist(puzzles[,c('Long', "Lat")])


# --- --- --- --- --- --- --- --- --- ---
# Link Socioeconomic variables: Income, Race, Median Age, Population Density, Housing Density
# --- --- --- --- --- --- --- --- --- ---

puzzle_sf_income_age = get_income_age(puzzles, 1000) |> dplyr::select(-buffer_size)

puzzle_sf_percent_white = get_percent_white(puzzles, 1000) |> dplyr::select(-buffer_size)
# puzzle_sf_percent_white = puzzle_sf_percent_white |> dplyr::select(-buffer_size)

puzzle_sf_pop_hous_dens = pop_housing_density(puzzles, 1000) |> dplyr::select(-buffer_size)
# puzzle_sf_pop_hous_dens = puzzle_sf_pop_hous_dens |> dplyr::select(-buffer_size)

# Join datasets together: percent white
puzzles_lauren_sf_anno = puzzles_lauren_sf |> 
  left_join(puzzle_sf_income_age, by='Name') |> 
  left_join(puzzle_sf_percent_white, by='Name') |>
  left_join(puzzle_sf_pop_hous_dens, by='Name')

# --- --- --- --- --- --- --- --- --- ---
# Link Restaurant counts:
# --- --- --- --- --- --- --- --- --- ---

puzzles_lauren_sf_buffered = buffer_rest_counts(
  df = puzzles_lauren_sf,
  rest_osm_sf = rest_BA$osm_points,
  buffer_size = 1000) |> data.frame() |> dplyr::select(-geometry)

puzzles_lauren_sf_anno_v2 = 
  puzzles_lauren_sf_anno |> left_join(puzzles_lauren_sf_buffered, by = 'Name')

mapview(puzzles_lauren_sf_anno,
        zcol = "restaurant_count",
        layer.name = "Restaurant Count per Buffer",
        col.regions = viridis::viridis,
        alpha.regions = 0.8)

# --- --- --- --- --- --- --- --- --- ---
# Road Density
# --- --- --- --- --- --- --- --- --- ---

# UTM transofrmation happened in the function call
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

# Get the data now out:
puzzles_lauren_sf_utm_buffer_df = puzzles_lauren_sf_utm_buffer |> as.tibble() |>
  dplyr::select(Name, total_road_length, road_density, area)

# Calculate distance to nearest road
# Find the nearest road for each puzzle point
nearest_index <- st_nearest_feature(puzzles_lauren_sf_utm_buffer, t_all_roads_utm)

# Compute the distance for each puzzle point to its corresponding nearest road
# Note: by_element = TRUE returns a vector of distances rather than a full matrix.
distances <- st_distance(puzzles_lauren_sf_utm, t_all_roads_utm[nearest_index, ], by_element = TRUE)

# Add the computed distances (converted to numeric) to your puzzles data
puzzles_lauren_sf_utm$distance_to_road <- as.numeric(distances)

# Left join with distance to nearest road:
puzzles_lauren_sf_utm_buffer_df_road_vars = puzzles_lauren_sf_utm_buffer_df |>
  left_join(as.tibble(puzzles_lauren_sf_utm)[c('Name', 'distance_to_road')]) |>
  rename(road_area = area) |>
  mutate(
    road_density      = drop_units(road_density),
    road_area              = drop_units(road_area)
  )


# Annotate back:

puzzles_lauren_sf_anno_v3 = 
  puzzles_lauren_sf_anno_v2 |> left_join(puzzles_lauren_sf_utm_buffer_df_road_vars, by = 'Name')


# --- --- --- --- --- --- --- --- --- ---
# Load remote sensing variables [Impervious surface, NDVI, Nightlight, Elev, Landcover, ]
# --- --- --- --- --- --- --- --- --- ---

# Impervious surface
puzzle_sp_tmp_sp <- as(st_transform(st_as_sf(puzzles_lauren_sf_anno_v3), crs(ncld_imp_surf_2023)),'Spatial')

puzzle_sp_tmp_sp$impervious_furface = raster::extract(ncld_imp_surf_2023, puzzle_sp_tmp_sp)

puzzle_sp_tmp_impervious_surface_percent = as.tibble(puzzle_sp_tmp_sp) |>
  dplyr::mutate(imp_surf = impervious_furface) |> 
  dplyr::select(Name, imp_surf) |> as.tibble() 

# NDVI
puzzle_sp_tmp_sp_ndvi <- as(st_transform(st_as_sf(puzzles_lauren_sf_anno_v3), crs(ndvi)),'Spatial')

puzzle_sp_tmp_sp_ndvi$ndvi = raster::extract(ndvi, puzzle_sp_tmp_sp_ndvi)

p_ndvi = as.tibble(puzzle_sp_tmp_sp_ndvi) |>
  dplyr::select(Name, ndvi) |> as.tibble() 

# Elevation
puzzle_sp_tmp_sp_elev <- as(st_transform(st_as_sf(puzzles_lauren_sf_anno_v3), crs(NED_UTM_raster)),'Spatial')

puzzle_sp_tmp_sp_elev$elev = raster::extract(NED_UTM_raster, puzzle_sp_tmp_sp_elev)

p_ndvi = as.tibble(puzzle_sp_tmp_sp_elev) |>
  dplyr::select(Name, elev) |> as.tibble() 


