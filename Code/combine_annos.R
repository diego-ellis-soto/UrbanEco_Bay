# Add street light in restaurant code 
# add nightlights black marble
# add ndvi ladnsaty anual

require(tidyverse)
require(sp)
require(sf)

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

plot(cropped_raster_imp_s, main='Impervious surface')
points(puzzle_sp_tmp_sp,
       col ='black',
       fill='black',cex=1)

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
  dplyr::select(Name, NatWalkInd, Ac_Total, Ac_Land, Workers)


# --- --- --- --- --- --- ---
# Annoate remote sensing and socioeconomic data ####
# --- --- --- --- --- --- ---
puzzles_lauren_sf_anno = puzzles_lauren_sf_anno |>
  left_join(puzzle_sp_tmp_impervious_surface_percent,
            puzzle_sp_tmp_w_walkabiltiy_scores,
            by='Name')






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
puzzles_lauren_sf_anno_sp_sf$CEC_mal_class_en = extract(CEC_map$Class_EN$Class_EN, puzzles_lauren_sf_anno_sp_sf)

# --- --- --- --- --- --- --- --- --- ---
# High res landcover data
# --- --- --- --- --- --- --- --- --- ---

bayarea_rep <- projectRaster(bayarea, crs = crs(puzzles_lauren_sf_anno_sp_sf))
puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion = extract(bayarea_rep, puzzles_lauren_sf_anno_sp_sf)
puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion_r = round(puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion)
# table(puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion_r)
puzzles_lauren_sf_anno_sp_df = data.frame(puzzles_lauren_sf_anno_sp)
puzzles_lauren_sf_anno_sp_df = data.frame(puzzles_lauren_sf_anno_sp_sf)

puzzles_lauren_sf_anno # This is the integrated spatial dataset with all the annotated variables

df_tmp_anno = data.frame(puzzles_lauren_sf_anno_sp_df) |>
  dplyr::select(Name, CEC_mal_class_en, 
                osm_landcov_fusion, osm_landcov_fusion_r)

# Annotated 
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


# Nightlights post density!
# nasa viirs 1km as well


reclass_values <- read_csv(
  "https://raw.githubusercontent.com/tgelmi-candusso/OSM_for_Ecology/main/reclass_tables/reclass_cec_2_mcsc.csv"
) # Annotate the landcover classes

write.csv(puzzles_lauren_sf_anno_v3, file = 'Outdir/puzzles_annotate_v3.csv')

require('corrplot')

puzzles_lauren_sf_anno_v4 %>%
  as.tibble() |>
  mutate(restaurant_count_num = as.numeric(restaurant_count)) |>
  dplyr::select(mean_income, mean_age,
                mean_percent_white, mean_pop_density,
                mean_housing_density, restaurant_count_num,
                human_mod, bio_1, bio_12, imp_surf,
                osm_landcov_fusion_r)  |>
  cor(use = "complete.obs") |>
  corrplot(
    method = "circle", 
    type = "lower", 
    tl.cex = 0.8,         # Text label size
    addCoef.col = "black" # Add numbers in black
  )
#
# --- --- --- --- --- --- ---
# Get Nightlight: ####
# https://rpubs.com/runner157/1113096
# https://geoffboeing.com/2016/07/visualize-urban-accessibility-walkability
# Python
# Hasta aca llegue ####

https://www.spatialedge.co/p/tutorial-downloading-and-processing
https://worldbank.github.io/blackmarbler/
# --- --- --- --- --- --- ---
  
  
# --- --- --- --- --- --- --- --- --- ---
# NDVI - Has problems ####
# --- --- --- --- --- --- --- --- --- ---
puzzles_lauren_sf_anno_sp = as(puzzles_lauren_sf_anno, 'Spatial')

puzzles_lauren_sf_anno_sp$ndvi = extract(ndvi, puzzles_lauren_sf_anno_sp)

# --- --- --- --- --- --- --- --- --- ---
# PAD-US ####
# --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- --- --- ---
# Roads - Still needs work
# --- --- --- --- --- --- --- --- --- ---

# Next stop: rerun NDVI
# Greenspace
# Distance to coast
# Dist2coastline
# Other variables:
# Road density
# CalEnviroScreen: https://oehha.ca.gov/calenviroscreen/maps-data/download-datatidt
# TIGRIS road density
# California Specific variables 
# Single Housing and vacancy housing 
# NALDC impervious surface https://www.mrlc.gov/data/nlcd-imperviousness-conus-all-years
# Distance to nearest road
#  puzzles_lauren_sf_anno |> left_join(puzzle_sp_tmp_impervious_surface_percent)
# Clip to only Laurens puzzles
# mapview(puzzle_sp_tmp_w_walkabiltiy_scores, zcol='Ac_Total')
# puzzle_sp_tmp_w_nathum_scores = st_intersection(walk_lauren_locs, puzzle_sp_tmp_w)
# mapview(puzzle_sp_tmp_w_nathum_scores, zcol='NatWalkInd')
# NatWalkInd
# Landcover type coarse
# puzzles_lauren_sf_anno_sp$nat_CEC_map = extract(CEC_map, st_as_sf(puzzles_lauren_sf_anno_sp))
# --- --- --- --- --- --- --- --- --- ---

# --- --- --- --- --- --- --- ---
# Mean annual precipitation
# --- --- --- --- --- --- --- ---

# Specify the version of WorldClim and resolution (10m, 5m, 2.5m, 30s)
worldclim_version <- "2.1"
resolution <- 30s  # 10 minutes (~18km)

# Extract the mean annual precipitation (BIO12)
cat("Extracting Mean Annual Precipitation (BIO12)...\n")
mean_annual_precip <- bioclim_data[[12]]

# Plot the data
plot(mean_annual_precip, main = "Mean Annual Precipitation (BIO12)")


# The one that did not work well Impervious surface"
# puzzles_lauren_sf_anno_sp$imp_surf30 = extract(imp_surf_30, puzzles_lauren_sf_anno_sp)
# Get a better metric? Why so many NA?
