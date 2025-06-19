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
library(geosphere)
require(mapview)
#puzzles  = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
 # mutate(states_abbrev ='CA')

puzzles  = read.csv('/Users/diegoellis/Downloads/NewStantonPuzzleStudyInfo_06172025.csv') |>
  mutate(states_abbrev ='CA') |> rename(Name=Site.Name)

puzzles_lauren_sf = puzzles |>
st_as_sf(coords = c("Long", "Lat"), crs = 4326)

puzzle_sp = as(puzzles_lauren_sf, 'Spatial')

# puzzles_lauren_sf = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
# mapview(puzzle_sp)
# --- --- --- --- --- --- --- --- --- ---
# Load functions:
# --- --- --- --- --- --- --- --- --- ---

source('Code/Functions/fun_income_race_points.R') # Load functions to obtain census income, race, etc  based on a buffer
source('Code/Functions/get_income_popden_race.R') # Get pop density and housing density based on a buffer
source('Code/Functions/OSM_get_restaurants.R') # restaurant counts within m buffer of locs
source('Code/Functions/load_roads.R') # Load road network to calculate road density and distance to nearest road
source('Code/Functions/get_rs_vars.R') # Load most of the remote sensing data
source('Code/Functions/prepare_daily_mobility_data.R') # Load most of the remote sensing data

#   
# --- --- --- --- --- --- --- --- --- ---
# Link Socioeconomic variables: Income, Race, Median Age, Population Density, Housing Density
# --- --- --- --- --- --- --- --- --- ---

puzzles = puzzles |> rename(Name = Site.Name)
puzzles_lauren_sf = puzzles |>
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) 

buffer_s = 1000


for(buffer_s in c(1000, 500, 250, 100)){
  print(buffer_s)
# From buffer_s in c(1000, 500, 250, 100)


# puzzles_lauren_sf_anno = puzzles_lauren_sf |> 
#   left_join(puzzle_sf_income_age, by='Name')

puzzle_sf_income_age = get_income_age(puzzles, buffer_s) |> dplyr::select(-buffer_size)

puzzle_sf_percent_white = get_percent_white(puzzles, buffer_s) |> dplyr::select(-buffer_size)
# puzzle_sf_percent_white = puzzle_sf_percent_white |> dplyr::select(-buffer_size)

puzzle_sf_pop_hous_dens = pop_housing_density(puzzles, buffer_s) |> dplyr::select(-buffer_size)
# puzzle_sf_pop_hous_dens = puzzle_sf_pop_hous_dens |> dplyr::select(-buffer_size)

# Replace Name with Site.Name

#  --- --- --- --- --- ---
# Annotate human mobility data buffered at 1km:
#   --- --- --- --- --- ---
results_v2  =  read.csv('Outdir/added_mobility_data.csv') |> rename(Name = Site.Name)

# Join datasets together: percent white
puzzles_lauren_sf_anno = puzzles_lauren_sf |> 
  left_join(puzzle_sf_income_age, by='Name') |> 
  left_join(puzzle_sf_percent_white, by='Name') |>
  left_join(puzzle_sf_pop_hous_dens, by='Name') |>
  left_join(results_v2, by='Name')
  
# --- --- --- --- --- --- --- --- --- ---
# Link Restaurant counts:
# --- --- --- --- --- --- --- --- --- ---

puzzles_lauren_sf_buffered = buffer_rest_counts(
  df = puzzles_lauren_sf,
  rest_osm_sf = rest_BA$osm_points,
  buffer_size = buffer_s) |>
  data.frame() |>
  dplyr::select(-geometry)

puzzles_lauren_sf_anno_v2 = 
  puzzles_lauren_sf_anno |> 
  left_join(puzzles_lauren_sf_buffered, by = 'Name')

# mapview(puzzles_lauren_sf_anno_v2,
#         zcol = "restaurant_count",
#         layer.name = "Restaurant Count per Buffer",
#         col.regions = viridis::viridis,
#         alpha.regions = 0.8)

# --- --- --- --- --- --- --- --- --- ---
# Road Density
# --- --- --- --- --- --- --- --- --- ---

# UTM transofrmation happened in the function call
puzzles_lauren_sf_utm_buffer <- st_buffer(puzzles_lauren_sf_utm,
                                          dist = buffer_s)

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
distances <- st_distance(puzzles_lauren_sf_utm, t_all_roads_utm[nearest_index, ],
                         by_element = TRUE)

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

# Add human mobility data (calculated with prepare_daily_mobility_data.R script)

# mobility_datA = read.csv('Outdir/added_mobility_data.csv') |>
#   mutate(Name = Site.Name)


# Annotate back:

puzzles_lauren_sf_anno_v3 = 
  puzzles_lauren_sf_anno_v2 |>
  left_join(puzzles_lauren_sf_utm_buffer_df_road_vars, by = 'Name')
# |>
#   left_join(mobility_datA, by = 'Name')

# Filter rows with NA in either mobility column and pull their Name
# missing_mobility_names <- puzzles_lauren_sf_anno_v3 %>%
#   filter(is.na(mean_daily_visits) | is.na(median_daily_visits)) %>%
#   pull(Name)

# Show the result
# missing_mobility_names


# --- --- --- --- --- --- --- --- --- ---
# Load remote sensing variables [Impervious surface, NDVI, Nightlight, Elev, Landcover, ]
# --- --- --- --- --- --- --- --- --- ---

# Impervious surface
puzzles_lauren_sf_buf = puzzles_lauren_sf |> st_buffer(buffer_s) 

puzzle_sp_tmp_sp <- as(st_transform(st_as_sf(puzzles_lauren_sf_buf),
                                    crs(ncld_imp_surf_2023)),'Spatial')

puzzle_sp_tmp_sp$impervious_furface = raster::extract(ncld_imp_surf_2023,
                                                      puzzle_sp_tmp_sp,
                                                      fun=mean, na.rm = TRUE)[,1]

puzzle_sp_tmp_impervious_surface_percent = as.tibble(puzzle_sp_tmp_sp) |>
  dplyr::mutate(imp_surf = impervious_furface) |> 
  dplyr::select(Name, imp_surf) |> as.tibble() 

# NDVI
puzzles_lauren_sf_buf = puzzles_lauren_sf |> st_buffer(buffer_s) 

puzzles_lauren_sf_buf$ndvi = raster::extract(ndvi, puzzles_lauren_sf_buf, fun=mean, na.rm = TRUE)[,1]

puzzles_lauren_sf_buf_ndvi = puzzles_lauren_sf_buf |> 
  as_tibble()  |> dplyr::select(Name, ndvi)

# puzzle_sp_tmp_sp_ndvi <- as(st_transform(st_as_sf(puzzles_lauren_sf_anno_v3), crs(ndvi)),'Spatial')
# 
# puzzle_sp_tmp_sp_ndvi$ndvi = raster::extract(ndvi, puzzle_sp_tmp_sp_ndvi)
# 
# p_ndvi = as.tibble(puzzle_sp_tmp_sp_ndvi) |>
#   dplyr::select(Name, ndvi) |> as.tibble() 

# Elevation
puzzles_lauren_sf_buf = puzzles_lauren_sf |> st_buffer(buffer_s) 

puzzles_lauren_sf_buf_elev <- as(st_transform(st_as_sf(puzzles_lauren_sf_buf), crs(NED_UTM_raster)),'Spatial')

puzzles_lauren_sf_buf_elev$elev = raster::extract(NED_UTM_raster, puzzles_lauren_sf_buf_elev, fun=mean, na.rm = TRUE)[,1]

p_elev = as.tibble(puzzles_lauren_sf_buf_elev) |>
  dplyr::select(Name, elev) |> as.tibble() 

# Night light blackmarbler not working anymore on laptop so pulling from previous successful runs:

# puzzle_nightligt = read.csv('/Users/diegoellis/Desktop/puzzles_annotate_v9.csv') |>
#   select(Name, nightlights)

# Landcover type:
puzzles_lauren_sf_buf = puzzles_lauren_sf |> st_buffer(buffer_s) 

puzzle_sp_tmp_sp_landcov <- as(st_transform(st_as_sf(puzzles_lauren_sf_buf), crs(cropped_raster_r)),'Spatial')
puzzle_sp_tmp_sp_landcov_vect = vect(puzzle_sp_tmp_sp_landcov)

puzzle_sp_tmp_sp_landcov_vect$nldc_landcover = terra::extract(cropped_raster, puzzle_sp_tmp_sp_landcov_vect)[,2]
# puzzle_sp_tmp_sp_landcov_vect$nldc_landcover = terra::extract(cropped_raster, puzzle_sp_tmp_sp_landcov_vect, fun=median, na.rm = TRUE)[,2]

p_landcover = as.tibble(puzzle_sp_tmp_sp_landcov_vect) |>
  dplyr::select(Name, nldc_landcover) |> as.tibble() 

# --- --- --- --- --- --- --- ---
# Mean Annual Temperature, Mean Annual precip
# --- --- --- --- --- --- --- ---

puzzles_lauren_sf_buf_r = puzzles_lauren_sf |> st_buffer(buffer_s) 

puzzles_lauren_sf_buf_r$human_mod = raster::extract(human_mod_americas_masked, puzzles_lauren_sf_buf_r, fun=mean, na.rm = TRUE)[,1]

puzzles_lauren_sf_buf_r$bio_1 = raster::extract(bio1_masked, puzzles_lauren_sf_buf_r, fun=mean, na.rm = TRUE)[,1]

puzzles_lauren_sf_buf_r$bio_12 = extract(bio_precip, puzzles_lauren_sf_buf_r, fun=mean, na.rm = TRUE)[,1]

hum_mod_bio_1_bio12 = puzzles_lauren_sf_buf_r |> as.tibble() |>
  dplyr::select(Name, bio_1, bio_12, human_mod) 

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Now dynamically add nightlight
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
nightlight_vals <- terra::extract(
  nightlight_raster,
  puzzles_lauren_sf_buf_r,
  fun = mean,
  na.rm = TRUE
) %>%
  mutate(
    Name = puzzles_lauren_sf_buf_r$Name,
    nightlight_avg = rowMeans(select(., t2022, t2023), na.rm = TRUE)
  )

puzzles_lauren_sf_buf_r_nightlights <- left_join(
  puzzles_lauren_sf_buf_r,
  nightlight_vals,
  by = "Name"
) |> st_drop_geometry() |>
  select(Name, nightlight_avg)

# Loop through each based on the study period and calculated mean nightlight during which the
# cameras where exposed

# --- --- --- --- --- --- --- ---
# Left join all remote sensing variables: 
# --- --- --- --- --- --- --- ---

puzzles_lauren_sf_anno_v4 = 
puzzles_lauren_sf_anno_v3 |>
  left_join(puzzle_sp_tmp_impervious_surface_percent) |>
  left_join(puzzles_lauren_sf_buf_ndvi) |>
  left_join(p_elev) |>
  left_join(puzzles_lauren_sf_buf_r_nightlights) |> # Not buffered to 1km2
  left_join(p_landcover) |>
  left_join(hum_mod_bio_1_bio12)

# --- --- --- --- --- --- --- --- --- ---
# Link Walkability Score
# --- --- --- --- --- --- --- --- --- ---

puzzles_lauren_sf_buf_r = puzzles_lauren_sf |> st_buffer(buffer_s) 

puzzle_sp_tmp_w <- st_transform(st_as_sf(puzzles_lauren_sf_buf_r), crs(walkabiltiy))
walk_lauren_locs = st_crop(walkabiltiy, st_bbox(puzzle_sp_tmp_w))
# plot(walk_lauren_locs['TotPop'])

puzzle_sp_tmp_w_walkabiltiy_scores = st_intersection(walk_lauren_locs, puzzle_sp_tmp_w) |> 
  as.tibble()  |> 
  dplyr::select(Name, NatWalkInd, Ac_Total, Ac_Land, Ac_Water, Workers)

puzzles_lauren_sf_anno_v5 =
  puzzles_lauren_sf_anno_v4 |>
  left_join(puzzle_sp_tmp_w_walkabiltiy_scores)

# puzzles_lauren_sf_anno_v5= puzzles_lauren_sf_anno_v4

# --- --- --- --- --- --- --- --- --- ---
# Correlation plot
# --- --- --- --- --- --- --- --- --- ---
require(corrplot)
quartz()
puzzles_lauren_sf_anno_v5 %>%
  as.tibble() |>
  mutate(restaurant_count_num = as.numeric(restaurant_count),
         nldc_landcover_num = as.numeric(nldc_landcover)) |>
  dplyr::select(mean_income, mean_age,mean_daily_visits,
                mean_percent_white, mean_pop_density,
                mean_housing_density, restaurant_count_num,
                human_mod, bio_1, bio_12, imp_surf,
                nldc_landcover_num, road_density,nightlight_avg, # osm_landcov_fusion_r
           #     Ac_Total, Ac_Land, Ac_Water, Workers,NatWalkInd
                elev, ndvi)  |>
  cor(use = "complete.obs") |>
  corrplot(
    method = "circle", 
    type = "lower", 
    tl.cex = 0.4,         # Text label size
    addCoef.col = "black" # Add numbers in black
  )

puzzles_estimate = 
  puzzles_lauren_sf_anno_v5 %>%
  as.tibble() |>
  mutate(restaurant_count_num = as.numeric(restaurant_count),
         nldc_landcover_num = as.numeric(nldc_landcover)) |>
  dplyr::select(mean_income, mean_age,
                mean_percent_white, mean_pop_density,mean_daily_visits,
                mean_housing_density, restaurant_count_num,
                human_mod, bio_1, bio_12, imp_surf,
                nldc_landcover_num, road_density, nightlight_avg, # osm_landcov_fusion_r
                # Ac_Total, Ac_Land, Ac_Water, Workers,NatWalkInd,
                elev, ndvi)

correlations <- correlate(puzzles_estimate, method = 'pearson')
quartz()
network_plot(correlations)

#  --- --- --- --- --- ---
# Distance between points:
#   --- --- --- --- --- ---

coords <- puzzles[, c("Long", "Lat")]

# Compute the full pairwise distance matrix (in meters) as before
dmat <- distm(coords[, c("Long", "Lat")], fun = distGeo)

# For each point, find the minimum distance to any other point (exclude self-distance of 0)
min_per_point <- apply(dmat, 1, function(row) min(row[row > 0]))

# See the results (first few points)
coords$min_distance <- min_per_point

puzzles_distmin = puzzles |> left_join(coords) |> select(Name, min_distance) |> rename(dist2nearest_camera = min_distance)

puzzles_lauren_sf_anno_v6 = puzzles_lauren_sf_anno_v5 |>
  left_join(puzzles_distmin) |>
mutate(buffer_size = buffer_s)

# Annotate to CalEnviroScreen
calenviro = read.csv('Indir/LASpollution_1000.csv')

puzzles_lauren_sf_anno_v6 = puzzles_lauren_sf_anno_v6 |>
  left_join(calenviro)


write.csv(puzzles_lauren_sf_anno_v6, file = paste0(
  'Outdir/puzzles_lauren_sf_anno_v6_', buffer_s, 'm_buffer.csv'))

paste0('Stored as ',
basename(paste0(
  'Outdir/puzzles_lauren_sf_anno_v6_', buffer_s, 'm_buffer.csv'))
)

}

# print(paste0('Written as file' ,puzzles_lauren_sf_anno_v6, file = paste0(
#   ''Outdir/puzzles_lauren_sf_anno_v6_', buffer_s, 'm_buffer.csv')''))
# # write.csv(puzzles_lauren_sf_anno_v6, file = 'Outdir/puzzles_lauren_sf_anno_v6_1000m_buffer.csv')
# }


