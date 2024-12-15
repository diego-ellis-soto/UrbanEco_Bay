
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
