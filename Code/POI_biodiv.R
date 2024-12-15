library(osmdata)
library(sf)
library(dplyr)

# Get bounding box for San Francisco
bbox_sf <- getbb("San Francisco, California")

# Define all features of interest as a character vector
all_features <- c(
  "\"leisure\"=\"park\"",
  "\"leisure\"=\"nature_reserve\"",
  "\"leisure\"=\"garden\"",
  "\"amenity\"=\"community_garden\"",
  "\"landuse\"=\"forest\"",
  "\"natural\"=\"wood\"",
  "\"natural\"=\"wetland\"",
  "\"water\"=\"lake\"",
  "\"water\"=\"pond\"",
  "\"landuse\"=\"farmland\"",
  "\"landuse\"=\"orchard\"",
  "\"landuse\"=\"vineyard\"",
  "\"boundary\"=\"protected_area\"",
  "\"highway\"=\"footway\"",
  "\"highway\"=\"path\"",
  "\"highway\"=\"cycleway\"",
  "\"tourism\"=\"viewpoint\"",
  "\"tourism\"=\"information\"",
  "\"tourism\"=\"bird_hide\"",
  "\"amenity\"=\"bench\"",
  "\"tourism\"=\"picnic_site\"",
  "\"natural\"=\"beach\"",
  "\"leisure\"=\"common\"",
  "\"landuse\"=\"greenfield\"",
  "\"landuse\"=\"cemetery\""
)

# Single command to get all features
all_data <- opq(bbox = bbox_sf) %>%
  add_osm_features(features = all_features) %>%
  osmdata_sf()

# Print and plot the results
print(all_data)
plot(st_geometry(all_data$osm_polygons), main = "All Biodiversity and Access-Related Features")




library(osmdata)
library(sf)
library(dplyr)
# 1. Get bounding box for San Francisco
bbox_sf <- getbb("San Francisco, California")

# 2. Define queries for different types of biodiversity-relevant OSM features
# Each query returns an 'osmdata' object with points, lines, and polygons.

# Parks
parks <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

# Nature reserves
nature_reserves <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "leisure", value = "nature_reserve") %>%
  osmdata_sf()

# Gardens (including community gardens)
gardens <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "leisure", value = "garden") %>%
  osmdata_sf()

community_gardens <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "amenity", value = "community_garden") %>%
  osmdata_sf()

# Forested or wooded areas
forest_areas <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "forest") %>%
  osmdata_sf()

wooded_natural <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "natural", value = "wood") %>%
  osmdata_sf()

# Wetlands
wetlands <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "natural", value = "wetland") %>%
  osmdata_sf()

# Water bodies (e.g., lakes, ponds)
lakes <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "water", value = "lake") %>%
  osmdata_sf()

ponds <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "water", value = "pond") %>%
  osmdata_sf()

# Farmland, orchards, vineyards, which can also support biodiversity
farmland <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "farmland") %>%
  osmdata_sf()

orchards <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "orchard") %>%
  osmdata_sf()

vineyards <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "vineyard") %>%
  osmdata_sf()

# 3. Extract polygonal features (most relevant for areas of biodiversity)
extract_polygons <- function(osm_data) {
  # osmdata_sf objects have $osm_points, $osm_lines, $osm_polygons, $osm_multipolygons
  # We combine polygons and multipolygons into one layer if available.
  
  poly <- osm_data$osm_polygons
  mpoly <- osm_data$osm_multipolygons
  
  # Bind them if they exist
  if (!is.null(poly) && !is.null(mpoly)) {
    return(rbind(poly, mpoly))
  } else if (!is.null(poly)) {
    return(poly)
  } else if (!is.null(mpoly)) {
    return(mpoly)
  } else {
    return(NULL)
  }
}

# Extract polygons from each dataset
parks_poly <- extract_polygons(parks)
nature_poly <- extract_polygons(nature_reserves)
gardens_poly <- extract_polygons(gardens)
community_gardens_poly <- extract_polygons(community_gardens)
forest_poly <- extract_polygons(forest_areas)
wood_poly <- extract_polygons(wooded_natural)
wetlands_poly <- extract_polygons(wetlands)
lakes_poly <- extract_polygons(lakes)
ponds_poly <- extract_polygons(ponds)
farmland_poly <- extract_polygons(farmland)
orchards_poly <- extract_polygons(orchards)
vineyards_poly <- extract_polygons(vineyards)

# 4. Combine all features into a single sf object
# Filter out NULLs
all_poly_list <- list(
  parks_poly,
  nature_poly,
  gardens_poly,
  community_gardens_poly,
  forest_poly,
  wood_poly,
  wetlands_poly,
  lakes_poly,
  ponds_poly,
  farmland_poly,
  orchards_poly,
  vineyards_poly
) %>% purrr::compact()

all_biodiversity_features <- do.call(rbind, all_poly_list)

# 5. Inspect the combined data
print(all_biodiversity_features)
plot(st_geometry(all_biodiversity_features))

# You can now analyze this combined sf object to assess biodiversity-related amenities,
# calculate distances, create summary statistics, or integrate with other layers like 
# human mobility or species observation data.

library(osmdata)
library(sf)
library(dplyr)
library(purrr)

# 1. Get bounding box for San Francisco
bbox_sf <- getbb("San Francisco, California")

# 2. Define queries for different types of biodiversity-relevant OSM features

# Existing:
# Parks
parks <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

# Nature reserves
nature_reserves <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "leisure", value = "nature_reserve") %>%
  osmdata_sf()

# Gardens (including community gardens)
gardens <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "leisure", value = "garden") %>%
  osmdata_sf()

community_gardens <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "amenity", value = "community_garden") %>%
  osmdata_sf()

# Forested or wooded areas
forest_areas <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "forest") %>%
  osmdata_sf()

wooded_natural <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "natural", value = "wood") %>%
  osmdata_sf()

# Wetlands
wetlands <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "natural", value = "wetland") %>%
  osmdata_sf()

# Water bodies
lakes <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "water", value = "lake") %>%
  osmdata_sf()

ponds <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "water", value = "pond") %>%
  osmdata_sf()

# Agricultural areas that support biodiversity
farmland <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "farmland") %>%
  osmdata_sf()

orchards <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "orchard") %>%
  osmdata_sf()

vineyards <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "vineyard") %>%
  osmdata_sf()

# Officially protected areas
protected_areas <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "boundary", value = "protected_area") %>%
  osmdata_sf()

# Additional queries to understand human access:

# 1. Paths and Trails (for pedestrian/bicycle access to biodiversity)
footways <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "highway", value = "footway") %>%
  osmdata_sf()

paths <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "highway", value = "path") %>%
  osmdata_sf()

cycleways <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "highway", value = "cycleway") %>%
  osmdata_sf()

# 2. Viewpoints and Information points
viewpoints <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "tourism", value = "viewpoint") %>%
  osmdata_sf()

info_points <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "tourism", value = "information") %>%
  osmdata_sf()

# 3. Bird Hides / Birdwatching areas (if any exist in OSM)
bird_hides <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "tourism", value = "bird_hide") %>%
  osmdata_sf()

# 4. Recreation Areas that might give humans access to biodiversity areas
beaches <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "natural", value = "beach") %>%
  osmdata_sf()

commons <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "leisure", value = "common") %>%
  osmdata_sf()

greenfields <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "landuse", value = "greenfield") %>%
  osmdata_sf()

# 5. Amenities that indicate human presence or access: benches, picnic sites
benches <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "amenity", value = "bench") %>%
  osmdata_sf()

picnic_sites <- opq(bbox = bbox_sf) %>%
  add_osm_feature(key = "tourism", value = "picnic_site") %>%
  osmdata_sf()

# Function to extract polygons
extract_polygons <- function(osm_data) {
  poly <- osm_data$osm_polygons
  mpoly <- osm_data$osm_multipolygons
  if (!is.null(poly) && !is.null(mpoly)) {
    return(rbind(poly, mpoly))
  } else if (!is.null(poly)) {
    return(poly)
  } else if (!is.null(mpoly)) {
    return(mpoly)
  } else {
    return(NULL)
  }
}

# Extract polygons from each dataset
parks_poly <- extract_polygons(parks)
nature_poly <- extract_polygons(nature_reserves)
gardens_poly <- extract_polygons(gardens)
community_gardens_poly <- extract_polygons(community_gardens)
forest_poly <- extract_polygons(forest_areas)
wood_poly <- extract_polygons(wooded_natural)
wetlands_poly <- extract_polygons(wetlands)
lakes_poly <- extract_polygons(lakes)
ponds_poly <- extract_polygons(ponds)
farmland_poly <- extract_polygons(farmland)
orchards_poly <- extract_polygons(orchards)
vineyards_poly <- extract_polygons(vineyards)
protected_areas_poly <- extract_polygons(protected_areas)
beaches_poly <- extract_polygons(beaches)
commons_poly <- extract_polygons(commons)
greenfields_poly <- extract_polygons(greenfields)

# For linear features like footways, paths, cycleways, viewpoints, info points, benches, and picnic sites,
# they might be points or lines rather than polygons. We can store them separately.
extract_lines <- function(osm_data) {
  return(osm_data$osm_lines)
}

extract_points <- function(osm_data) {
  return(osm_data$osm_points)
}

footways_line <- extract_lines(footways)
paths_line <- extract_lines(paths)
cycleways_line <- extract_lines(cycleways)
viewpoints_point <- extract_points(viewpoints)
info_points_point <- extract_points(info_points)
bird_hides_point <- extract_points(bird_hides)
benches_point <- extract_points(benches)
picnic_sites_point <- extract_points(picnic_sites)

# Combine all polygonal biodiversity features
all_poly_list <- list(
  parks_poly,
  nature_poly,
  gardens_poly,
  community_gardens_poly,
  forest_poly,
  wood_poly,
  wetlands_poly,
  lakes_poly,
  ponds_poly,
  farmland_poly,
  orchards_poly,
  vineyards_poly,
  protected_areas_poly,
  beaches_poly,
  commons_poly,
  greenfields_poly
) %>% purrr::compact()

all_biodiversity_polygons <- do.call(rbind, all_poly_list)

# Combine other features (lines and points) into separate layers if needed
all_access_lines <- do.call(rbind, purrr::compact(list(
  footways_line,
  paths_line,
  cycleways_line
)))

all_access_points <- do.call(rbind, purrr::compact(list(
  viewpoints_point,
  info_points_point,
  bird_hides_point,
  benches_point,
  picnic_sites_point
)))

# Print and plot results
print(all_biodiversity_polygons)
plot(st_geometry(all_biodiversity_polygons), main = "Biodiversity Relevant Polygons")

if (!is.null(all_access_lines)) {
  plot(st_geometry(all_access_lines), col = 'blue', add = TRUE, main = "Access Lines")
}

if (!is.null(all_access_points)) {
  plot(st_geometry(all_access_points), col = 'red', pch = 16, add = TRUE, main = "Access Points")
}

# 
# Interpreting and Using the Data:
#   
#   Biodiversity Polygons: Parks, protected areas, nature reserves, wetlands, forests, and orchards can give you an idea of where biodiversity might be high.
# 
# Access Infrastructure (Lines): Footways, paths, and cycleways help you understand how and where people can move through or into these areas, giving insight into actual accessibility.
# 
# Access Points (Points): Viewpoints, tourist information centers, bird hides, benches, and picnic sites tell you where people are encouraged to visit, rest, observe, and learn about biodiversity.
# 
# By combining these datasets, you can assess not just the spatial distribution of biodiversity-related areas but also how people might reach and experience these areas, giving a fuller picture of human-nature interactions.
