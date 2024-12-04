

puzzles = read.csv('/Users/diegoellis/Downloads/StantonPuzzleStudyLocations_11042024.csv') |>
  mutate(states_abbrev ='CA')

puzzles_lauren_sf = read.csv('/Users/diegoellis/Downloads/StantonPuzzleStudyLocations_11042024.csv') |>
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)


# Load income and race
source('Code/Functions/fun_income_race_points.R')
source('Code/get_income_popden_race.R')

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

puzzles_lauren_sf_anno = puzzles_lauren_sf_anno |> left_join(data.frame(puzzles_lauren_sf_buffered)[,c('Name', 'restaurant_count')])

# --- --- --- --- --- --- --- --- --- ---
# Landcover type


puzzles_lauren_sf_anno_sp$landcover = extract(bayarea, puzzles_lauren_sf_anno_sp)

puzzles_lauren_sf_anno_sp$landcover_i = as.integer(round(puzzles_lauren_sf_anno_sp$landcover))  



puzzles_lauren_sf_anno_sp$nat_CEC_map = extract(CEC_map, st_as_sf(puzzles_lauren_sf_anno_sp))


# puzzles_lauren_sf_anno_sp_landcover_tye 



# --- --- --- --- --- --- --- --- --- ---


# --- --- --- --- --- --- --- --- --- ---
# Remote sensing variables: NDVI, NALDC, Impervious Surface, Human Footprint, Mean Annual Temperature
# --- --- --- --- --- --- --- --- --- ---

puzzles_lauren_sf_anno_sp = as(puzzles_lauren_sf_anno, 'Spatial')
# NDVI
puzzles_lauren_sf_anno_sp$ndvi = extract(ndvi, puzzles_lauren_sf_anno_sp)
# Impervious surface"
puzzles_lauren_sf_anno_sp$imp_surf30 = extract(imp_surf_30, puzzles_lauren_sf_anno_sp)
# Get a better metric? Why so many NA?
# Human Footprint
puzzles_lauren_sf_anno_sp$human_mod = extract(human_mod_americas_masked, puzzles_lauren_sf_anno_sp)
# Mean Annual Temperature
puzzles_lauren_sf_anno_sp$bio_1 = extract(bio1_masked, puzzles_lauren_sf_anno_sp)

# National Landcover Data:
puzzles_lauren_sf_anno_sp_sf$CEC_mal_class_en = extract(CEC_map$Class_EN$Class_EN, puzzles_lauren_sf_anno_sp_sf)
puzzles_lauren_sf_anno_sp_sf$CEC_mal_class_en =  puzzles_lauren_sf_anno_sp_sf$CEC_mal_class_en$Class_EN

# High res landcover data
bayarea_rep <- projectRaster(bayarea, crs = crs(puzzles_lauren_sf_anno_sp_sf))
puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion = extract(bayarea_rep, puzzles_lauren_sf_anno_sp_sf)
puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion_r = round(puzzles_lauren_sf_anno_sp_sf$osm_landcov_fusion)

puzzles_lauren_sf_anno_sp_df = data.frame(puzzles_lauren_sf_anno_sp)

write.csv(puzzles_lauren_sf_anno_sp_df, file = 'Outdir/puzzles_annotate.csv')

require('corrplot')

puzzles_lauren_sf_anno_sp_df %>%
  dplyr::select(mean_income, mean_age, mean_percent_white, mean_pop_density, mean_housing_density, restaurant_count, human_mod, bio_1, imp_surf30) %>%
  cor(use = "complete.obs") |>
corrplot(
  method = "circle", 
  type = "lower", 
  tl.cex = 0.8,         # Text label size
  addCoef.col = "black" # Add numbers in black
)












# Get a NALDC Impervious surface


# NALDC Impervious surface
# Next step: PAD US database
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