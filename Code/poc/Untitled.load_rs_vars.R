# Look at NDVI:
require(sf)
require(mapview)
require(raster)
library(rnaturalearth)
require("rnaturalearthdata")

# NDVI
ndvi  = raster('/Users/diegoellis/Desktop/Projects/Postdoc/Misc_proj_data/BayArea/SF_EastBay_NDVI_Sentinel_10.tif')
# Mask the water:
continents <- ne_countries(scale = "medium", returnclass = "sf")
america_continents <- continents[continents$continent %in% c("North America"), ]
america_continents <- st_transform(america_continents, crs(ndvi))
puzzles_lauren_spatial <- as(puzzles_lauren_sf_buffered_trans, "Spatial")
ndvi_masked <- mask(ndvi, america_continents)
mapview(ndvi_masked)

# Impervious surface:
imp_surf_30 =raster('/Users/diegoellis/Desktop/Projects/Postdoc/Misc_proj_data/BayArea/SF_EastBay_GISD30_Impervious_Surface_30m.tif')
mapview(imp_surf_30)


# Did not work:
imp_surf_MODIS =raster('/Users/diegoellis/Desktop/Projects/Postdoc/Misc_proj_data/BayArea/SF_EastBay_Impervious_Surface_MODIS_500m.tif')
mapview(imp_surf_MODIS)


