# --- --- --- --- --- --- --- --- --- ---
# Combine socio-economic variables across buzzle locations:
#
# Contact: diego.ellissoto@berkeley.edu
#
# To do: # add pad us
# add walkable index using osm and roads
#
#
# --- --- --- --- --- --- --- --- --- ---

require(tidyverse)
require(sp)
require(sf)
library(corrr)

puzzles  = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
  mutate(states_abbrev ='CA')

puzzles_lauren_sf = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

puzzle_sp = as(puzzles_lauren_sf, 'Spatial')

# Load functions:
source('Code/Functions/fun_income_race_points.R') # Get census income race
source('Code/get_income_popden_race.R') # Get pop density, housing density
source('Code/OSM_get_restaurants.R') # restaurant counts within 1000m buffer of locs
source('Code/load_roads.R') # Still needs work
source('Code/load_rs_vars.R') # Load most of the remote sensing data

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

puzzles_lauren_sf_anno = puzzles_lauren_sf_anno |> 
  left_join(
    data.frame(puzzles_lauren_sf_buffered)[,c('Name', 'restaurant_count')]
    )

# --- --- --- --- --- --- --- --- --- ---
# Load remote sensing variables
# --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- ---
# Impervious Surface
# --- --- --- --- --- --- --- --- --- ---

puzzles_lauren_sf_anno_sp = as(puzzles_lauren_sf_anno, 'Spatial')
puzzle_sp_tmp <- st_transform(st_as_sf(puzzles_lauren_sf_anno_sp), crs(ncld_imp_surf_2023))
puzzle_sp_tmp_sp <- as(st_transform(st_as_sf(puzzles_lauren_sf_anno_sp), crs(ncld_imp_surf_2023)),'Spatial')
cropped_raster_imp_s <- crop(ncld_imp_surf_2023, extent(st_bbox(puzzle_sp_tmp)))
puzzle_sp_tmp$impervious_furface = extract(cropped_raster_imp_s, puzzle_sp_tmp)

puzzle_sp_tmp_impervious_surface_percent = puzzle_sp_tmp |>
  dplyr::mutate(imp_surf = impervious_furface) |> 
  dplyr::select(Name, imp_surf) |> as.tibble() 

# plot(cropped_raster_imp_s, main='Impervious surface')
# points(puzzle_sp_tmp_sp,
#        col ='black',
#        fill='black',cex=1)

# --- --- --- --- --- --- --- --- --- ---
# Link Walkability Score
# --- --- --- --- --- --- --- --- --- ---
# plot(walkabiltiy$NatWalkInd)

puzzle_sp_tmp_w <- st_transform(st_as_sf(puzzles_lauren_sf_anno_sp), crs(walkabiltiy))
walk_lauren_locs = st_crop(walkabiltiy, st_bbox(puzzle_sp_tmp_w))
# plot(walk_lauren_locs['TotPop'])
# 
puzzle_sp_tmp_w_walkabiltiy_scores = st_intersection(walk_lauren_locs, puzzle_sp_tmp_w) |> 
  as.tibble()  |> 
  dplyr::select(Name, NatWalkInd, Ac_Total, Ac_Land, Ac_Water, Workers)


# --- --- --- --- --- --- ---
# Annoate remote sensing and socioeconomic data ####
# --- --- --- --- --- --- ---
puzzles_lauren_sf_anno = puzzles_lauren_sf_anno |>
  left_join(puzzle_sp_tmp_impervious_surface_percent,
            by='Name') |>
  left_join(puzzle_sp_tmp_w_walkabiltiy_scores, by='Name')

# --- --- --- --- --- --- ---
# Landcover
# --- --- --- --- --- --- ---
# puzzles_lauren_sf_anno_sp$landcover = raster::extract(bayarea, puzzles_lauren_sf_anno_sp)
# 
# puzzles_lauren_sf_anno_sp$landcover_i = as.integer(round(puzzles_lauren_sf_anno_sp$landcover))  

# df_tmp = data.frame(puzzles_lauren_sf_anno_sp)


# --- --- --- --- --- --- --- ---
# National Landcover Data:
# --- --- --- --- --- --- --- ---
# puzzles_lauren_sf

puzzles_lauren_sf_anno_sp_sf$CEC_mal_class_en = extract(CEC_map$Class_EN$Class_EN, puzzles_lauren_sf_anno_sp_sf)

# --- --- --- --- --- --- --- --- --- ---
# High res landcover data
# --- --- --- --- --- --- --- --- --- ---

bayarea_rep <- projectRaster(bayarea, crs = crs(puzzles_lauren_sf_anno_sp_sf))
puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion = extract(bayarea_rep, puzzles_lauren_sf_anno_sp_sf)
puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion_r = round(puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion)
# table(puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion_r)
# puzzles_lauren_sf_anno_sp_df = data.frame(puzzles_lauren_sf_anno_sp)
puzzles_lauren_sf_anno_sp_df = data.frame(puzzles_lauren_sf_anno_sp_sf)

df_tmp_anno = data.frame(puzzles_lauren_sf_anno_sp_df) |>
  dplyr::select(Name, CEC_mal_class_en, 
                osm_landcov_fusion, osm_landcov_fusion_r)

# Annotated 
# puzzles_lauren_sf_anno # This is the integrated spatial dataset with all the annotated variables
puzzles_lauren_sf_anno_v2 = puzzles_lauren_sf_anno |>
  left_join(df_tmp_anno,
            by='Name')

# Bio 1 now
# --- --- --- --- --- --- --- ---
# Human Footprint
# --- --- --- --- --- --- --- ---
puzzles_lauren_sf_anno_sp$human_mod = extract(human_mod_americas_masked, puzzles_lauren_sf_anno_sp)

# --- --- --- --- --- --- --- ---
# Mean Annual Temperature, Mean Annual precip
# --- --- --- --- --- --- --- ---
puzzles_lauren_sf_anno_sp$bio_1 = extract(bio1_masked, puzzles_lauren_sf_anno_sp)
puzzles_lauren_sf_anno_sp$bio_12 = extract(bio_precip_masked, puzzles_lauren_sf_anno_sp)


hum_mod_bio_1 = data.frame(puzzles_lauren_sf_anno_sp) |>
  dplyr::select(Name, bio_1, bio_12, human_mod) 

puzzles_lauren_sf_anno_v3 = puzzles_lauren_sf_anno_v2 |>
left_join(
  hum_mod_bio_1,
  by = 'Name')

# --- --- --- --- --- --- --- ---
# Road Density
# --- --- --- --- --- --- --- ---

puzzles_lauren_sf_anno_v4 = puzzles_lauren_sf_anno_v3 |>
  left_join(
    puzzles_lauren_sf_utm_buffer_df,
    by = 'Name')

puzzles_lauren_sf_anno_v4 |>
  as.tibble() |>
  dplyr::select(Name, mean_income, mean_age, mean_percent_white,
                mean_pop_density, mean_housing_density, restaurant_count, imp_surf,
                NatWalkInd, Ac_Total, Ac_Land, Ac_Water, Workers, CEC_mal_class_en,
                osm_landcov_fusion, osm_landcov_fusion_r, bio_1, bio_12,
                human_mod, total_road_length, road_density, area) |>
  write.csv(file = 'Outdir/puzzles_annotate_v5.csv')

# write.csv(puzzles_lauren_sf_anno_v4, file = 'Outdir/puzzles_annotate_v4.csv')

# --- --- --- --- --- ---
# Nightlights:
# --- --- --- --- --- ---
puzzles_lauren_sf$nightlights = extract(r_2022$t2022, puzzles_lauren_sf)
puzzles_lauren_sf$nightlights = puzzles_lauren_sf$nightlights$t2022

tmp_nightl = puzzles_lauren_sf |> as.tibble() |>
  dplyr::select(Name, nightlights)


puzzles_lauren_sf_anno_v6 = puzzles_lauren_sf_anno_v4 |>
  left_join(tmp_nightl, by ='Name')

# --- --- --- --- --- ---
# Night posts on street!:
# --- --- --- --- --- ---

puzzles_lauren_sf_anno_v7 = puzzles_lauren_sf_anno_v6 |>
  as.tibble() |>
  dplyr::select(Name, mean_income, mean_age, mean_percent_white,
                mean_pop_density, mean_housing_density, restaurant_count, imp_surf,
                NatWalkInd, Ac_Total, Ac_Land, Ac_Water, Workers, CEC_mal_class_en,
                osm_landcov_fusion, osm_landcov_fusion_r, bio_1, bio_12,
                human_mod, total_road_length, road_density, area, nightlights) 

write.csv(puzzles_lauren_sf_anno_v7, file = 'Outdir/puzzles_annotate_v7.csv')

# --- --- --- --- --- ---
# Elevation
# --- --- --- --- --- ---

puzzles_lauren_sf$elev = extract(NED_UTM$USGS_1_n38w123, puzzles_lauren_sf)
puzzles_lauren_sf$elev = puzzles_lauren_sf$elev$USGS_1_n38w123

tmp_pad_us = st_intersection(PADUS, puzzles_lauren_sf) |> dplyr::select(Name, GAP_Sts) |> as.tibble()

# puzzles_lauren_sf$PAD_GAP_Sts = 


tmp_elev = puzzles_lauren_sf |> as.tibble() |>
  dplyr::select(Name, elev)


puzzles_lauren_sf_anno_v8 = puzzles_lauren_sf_anno_v7 |>
  left_join(tmp_elev, by ='Name')

write.csv(puzzles_lauren_sf_anno_v8, file = 'Outdir/puzzles_annotate_v8.csv')

# --- --- --- --- --- ---
# NDVI # 1km buffer around it
# --- --- --- --- --- ---
puzzles_lauren_sf_anno_v8 = read.csv('Outdir/puzzles_annotate_v8.csv')

puzzles_lauren_sf = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

puzzles_lauren_sf_buf = puzzles_lauren_sf |> st_buffer(1000) 

puzzles_lauren_sf_buf$ndvi = raster::extract(ndvi, puzzles_lauren_sf_buf, fun=mean, na.rm = TRUE)

puzzles_lauren_sf_buf = puzzles_lauren_sf_buf |> 
  as_tibble() |> dplyr::mutate(ndvi = ndvi[,1]) |> dplyr::select(-geometry, Name, ndvi)


puzzles_lauren_sf_anno_v9 = puzzles_lauren_sf_anno_v8 |>
  left_join(puzzles_lauren_sf_buf)

write.csv(puzzles_lauren_sf_anno_v9, file = 'Outdir/puzzles_annotate_v9.csv')

puzzles_lauren_sf_anno_v9$road_density <- as.numeric(gsub("\\s*\\[.*\\]", "", puzzles_lauren_sf_anno_v9$road_density))

# --- --- --- --- --- ---
# Human Mobility - Daily/Weekly/Monthly locations for a subset of puzzle locations
# --- --- --- --- --- ---


# Extract geom daymet and ndvi?
# CXhcek for heatwave paper and ask carl


# Nightlights post density!

require('corrplot')
quartz()

puzzles_lauren_sf_anno_v9 %>%
  as.tibble() |>
  mutate(restaurant_count_num = as.numeric(restaurant_count)) |>
  dplyr::select(mean_income, mean_age,
                mean_percent_white, mean_pop_density,
                mean_housing_density, restaurant_count_num,
                human_mod, bio_1, bio_12, imp_surf,
                osm_landcov_fusion_r, road_density, NatWalkInd,
                Ac_Total, Ac_Land, Ac_Water, Workers, nightlights,
                elev, ndvi)  |>
  cor(use = "complete.obs") |>
  corrplot(
    method = "circle", 
    type = "lower", 
    tl.cex = 0.4,         # Text label size
    addCoef.col = "black" # Add numbers in black
  )

puzzles_estimates = puzzles_lauren_sf_anno_v9 %>%
  as.tibble() |>
  mutate(restaurant_count_num = as.numeric(restaurant_count)) |>
  dplyr::select(mean_income, mean_age,
                mean_percent_white, mean_pop_density,
                mean_housing_density, restaurant_count_num,
                human_mod, bio_1, bio_12, imp_surf,
                osm_landcov_fusion_r, road_density, NatWalkInd,
                Ac_Total, Ac_Land, Ac_Water, Workers, nightlights,
                elev, ndvi) 

correlations <- correlate(puzzles_estimates, method = 'pearson')
network_plot(correlations)

#
# --- --- --- --- --- --- ---
library(car)
# --- --- --- --- --- --- ---
  
  reclass_values <- read_csv(
    "https://raw.githubusercontent.com/tgelmi-candusso/OSM_for_Ecology/main/reclass_tables/reclass_cec_2_mcsc.csv"
  ) # Annotate the landcover classes


# --- --- --- --- --- --- --- --- --- ---
# PAD-US ####
# --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- ---
# Roads - Still needs work
# --- --- --- --- --- --- --- --- --- ---

# Greenspace
# Distance to coast: https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/

# Other variables:
# CalEnviroScreen: https://oehha.ca.gov/calenviroscreen/maps-data/download-datatidt
# California Specific variables 
# NALDC impervious surface https://www.mrlc.gov/data/nlcd-imperviousness-conus-all-years
# Distance to nearest road

# mapview(puzzle_sp_tmp_w_walkabiltiy_scores, zcol='Ac_Total')
# puzzle_sp_tmp_w_nathum_scores = st_intersection(walk_lauren_locs, puzzle_sp_tmp_w)
# mapview(puzzle_sp_tmp_w_nathum_scores, zcol='NatWalkInd')
# NatWalkInd
# Landcover type coarse
# Single Housing and vacancy housing 
# --- --- --- --- --- --- --- --- --- ---
  # https://rpubs.com/runner157/1113096
  # https://geoffboeing.com/2016/07/visualize-urban-accessibility-walkability
  # Hasta aca llegue ####
  # https://www.spatialedge.co/p/tutorial-downloading-and-processing
  # https://worldbank.github.io/blackmarbler/
  