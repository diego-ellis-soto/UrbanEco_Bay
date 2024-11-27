# Next steps: 
# NDVI sentinel
# HFI
# Impervious surface
# Pop densirt
# Income
# Percent race


#################################
#   OSM for ecology - demo   ####
#  Gelmi-Candusso and Rodriguez #
#################################

#### Notes

# Quick demo to generate an LULC map from the OSM database and integrate it into a global landcover map. 
# Cropped global landcover map is provided due to storage limitations

# In the demo we will generate an OSM-enhanced LULC map for Washington, which has is available as a stored available for the city alone, making computational requirements suitable for this demo.
# In the event of requiring a specific city, we recommend downloading the regional or continental OSM database from geofabrik and specifying a bounding box, as we describe in the manuscript, replace Step 1 below with script from "Alt_Step1_with_predownloaded_database"


## libraries

library(osmextract)
library(tidyterra)
library(dplyr)
library(terra)
library(sf)
source("OSM_to_LULC_functions.R")

#==================================
# Step 1: Download/Load OSM database
#==================================

# Load the characterized key values (Table S4) we provided in the manuscript so those features are all downloaded by the oe_get() or oe_read() orfunction

osm_kv <- read.csv("urban_features/osm_key_values.csv") #table with the key-value pairs to be extracted I nded up doing it by hand though, so we can hard code it
osm_kv <- osm_kv %>% filter(!is.na(key))
keys <- unique(osm_kv$key)

# Download database for both polygons and lines.
# This function queries and downloads a database file based on the result of that query. 
# The query can be either through a city name, a region, or a bounding box 
# If the specific name queried is not found, the function will suggest the closest result found, and suggest a database file which it will download. This result might not be the one you meant. 
# For regions queries like "us/illinois", "us/washington" work well. 
# An alternative is to predownload the database file from geofabrik, store it and call it directly using the oe_get() function, the code for this is in the "Alt_Step1_with_predownloaded_database.R" script. 
# For using bounding boxes as query I will leave here, and comment out an example to download Chicago.

#crop study area directly here using the 'clipsrc' boundary type

#Chicago
bbox_crs <- 4269

xmin = -123.1738
ymin = 37.5398
xmax = -122.0818
ymax = 37.9998 

crssa=4326

# study_area_bbox <- st_bbox(ext(c(xmin = -123.1738, ymin = 37.6398, xmax = -122.2818, ymax = 37.9298)),crs=st_crs(bbox_crs))

# study_area_bbox <- st_bbox(ext(c(-88.38422408, -87.45238124, 41.49280305, 42.34485453)), crs=st_crs(bbox_crs)) #3857
#change crs of bbox to 4326
# study_area_bbox <- study_area_bbox %>%
#   st_as_sfc() %>%
#   st_transform(crs = 4326) %>%
#   st_bbox()

study_area_bbox = st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))

pol_feat <- osmextract::oe_get(place = study_area_bbox, #can also use place = study_area_bbox to query for databases containing the study area
                               # boundary = study_area_bbox,
                               boundary_type = 'clipsrc',
                               #boundary = sf::st_bbox(c(xmin = 11.23602, ymin = 47.80478, xmax = 11.88867, ymax = 48.24261)), #must be in crs=4326 or specified in the st_bbox object
                               provider ="geofabrik",
                               layer = "multipolygons",
                               stringsAsFactors = FALSE, 
                               quiet = FALSE,
                               #OSMEXT_DOWNLOAD_DIRECTORY=/path/to/osm/data
                               extra_tags=keys)

lin_feat <- osmextract::oe_get(study_area_bbox,
                               layer = "lines", 
                               # boundary = study_area_bbox,
                               boundary_type = 'clipsrc',
                               stringsAsFactors = FALSE, 
                               extra_tags=keys)

#save rds so you dont have to download each time
saveRDS(pol_feat, "OSM_polygon_features_NorthernCali.rds")
saveRDS(lin_feat, "OSM_linear_features_NorthernCali.rds")

vlayers <- OSMtoLULC_vlayers(
  OSM_polygon_layer = pol_feat, 
  OSM_line_layer = lin_feat
)

extent <- as.vector(ext(pol_feat))

rlayers <- OSMtoLULC_rlayers(
  OSM_LULC_vlayers = vlayers,
  study_area_extent = extent
)

OSM_only_map <- merge_OSM_LULC_layers(
  OSM_raster_layers = rlayers
)

CEC_map <- rast(
  "/Users/diegoellis/Desktop/Projects/Postdoc/OSM_for_Ecology/land_cover_2020v2_30m_tif/NA_NALCMS_landcover_2020v2_30m/data/NA_NALCMS_landcover_2020v2_30m.tif"
) 

bbox_sf <- st_as_sfc(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), 
                             crs = st_crs(4326)))

bbox_reprojected <- st_transform(bbox_sf, crs = st_crs(CEC_map))
bbox_vect <- vect(bbox_reprojected)

# Clip the raster
CEC_map_clipped <- crop(CEC_map, bbox_vect)
plot(CEC_map_clipped)


bbox_reprojected_OSM <- st_transform(bbox_sf, crs = st_crs(OSM_only_map))
OSM_only_map_eastbay = crop(OSM_only_map, bbox_reprojected_OSM)
OSM_only_map_eastbay = project(OSM_only_map_eastbay, crs(CEC_map))


ggplot(data = CEC_map_clipped) +
  geom_raster(aes(x = x, y = y, fill = first)) +
  theme_void() +
  theme(legend.position = "right")+
  coord_equal() 

reclass_values <- read_csv(
  "https://raw.githubusercontent.com/tgelmi-candusso/OSM_for_Ecology/main/reclass_tables/reclass_cec_2_mcsc.csv"
)

CEC_to_OSM_table <- reclass_values %>% 
  dplyr::select(cec_value, osm_value)

OSM_enhanced_LULC_map <- integrate_OSM_to_globalLULC(
  OSM_lulc_map = OSM_only_map_eastbay,
  global_lulc_map = CEC_map, 
  reclass_table = CEC_to_OSM_table
)

ggplot(data = OSM_enhanced_LULC_map) +
  geom_raster(aes(x = x, y = y, fill = first)) +
  scale_fill_manual(values=c("#843438","#df919a",	"#F88A50", "#EC5C3B","#FEF3AC",
                             "#D4ED88","#AFDC70", "#83C966", "#51B25D","#d19c5f", "#1A9850",
                             "#088da5",
                             "#b0b0b0", "#000000",
                             "#ff580f", "#ce7e00",
                             "#ffde1a","#ffce00","#ffa700","#ff8d00", 
                             "#ff7e26", "#ff7400",
                             "#FDB768", "#783F04",
                             "#FEF3AC", "#AD6A24",
                             "#FDDB87", "#400000"),
                    breaks = c(1:28),
                    labels=c("industrial", "commercial", "institutional","residential","landuse_railway",
                             "open green", "protected area", "resourceful green area","heterogeneous green area", "barren soil","dense green area",
                             "water",
                             "parking surface", "buildings",
                             "roads (v.h. traffic)",
                             "sidewalks",
                             "roads_na",
                             "roads (v.l. traffic)",
                             "roads (l. traffic)",
                             "roads (m. traffic)",
                             "roads (h.t.l.s)",
                             "roads (h.t.h.s)",
                             "trams/streetcars",
                             "hiking trails",
                             "railways",
                             "unused linear feature",
                             "barriers",
                             "developed_na")) +
  theme_void() +
  theme(legend.position = "right")+
  coord_equal() 

terra::writeRaster(
  OSM_enhanced_LULC_map,
  "BayArea_OSM-enhanced_lcover_map.tif",
  overwrite=TRUE
)
