# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Get the GEOID location to send to human mobility product apps
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

require(tidyverse)
require(sp)
require(sf)
library(corrr)
require(tidycensus)


# Get BlockgroupID from San Francsico:

# Download Income data
state_income <- get_acs(
  state = "CA",
  county = c('San Francisco'), # Alameda", "Contra Costa"
   geography = "block group",
  # geography = "tract",
  variables = c(medinc = "B19013_001"),
  geometry = TRUE,
  year = 2020,
  output='wide'
)

sf_geo_id = unique(state_income$GEOID)


# Get Get BlockgroupID from Lauren's Puzzle locations



# Get locations of UWIN camera sites:
state_income_CA <- get_acs(
  state = "CA",
  # county = c('San Francisco'), # Alameda", "Contra Costa"
   geography = "block group",
  # geography = "tract",
  variables = c(medinc = "B19013_001"),
  geometry = TRUE,
  year = 2020,
  output='wide'
)

puzzles  = read.csv('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/StantonPuzzleStudyLocations_11042024.csv') |>
  mutate(states_abbrev ='CA')

puzzles_lauren_sf = puzzles |>
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) |>
  st_transform( crs(st_transform(state_income_CA)))

point_joined_income_age <- st_join(
  puzzles_lauren_sf,
  state_income_CA, 
  join = st_intersects,
  left = FALSE)

sf_lauren_geo_id = unique(point_joined_income_age$GEOID)

write.csv( unique(c(sf_lauren_geo_id, sf_geo_id)),
           file = '/Users/diegoellis/Downloads/GeoID_samples_BerkeleyStudy_WalkScore_censustract.csv',
           row.names = F)

# UWIN sites:
write.csv(sf_lauren_geo_id, file = '/Users/diegoellis/Downloads/GeoID_samples_BerkeleyStudy_Spectus.csv',
          row.names = F)



GEO_ID_all_but_UWIN = unique(c(sf_lauren_geo_id, sf_geo_id))
write.csv(GEO_ID_all_but_UWIN, file = '/Users/diegoellis/Downloads/GeoID_samples_BerkeleyStudy.csv',
          row.names = F)

# Looking at Biodiversity Access 

# Get all of those variables for California?!

# 


# Look at relationship betwen all the variables for San Francisco County

st_centroid(state_income)

# Next for all of California !

# 

require('sdm')

