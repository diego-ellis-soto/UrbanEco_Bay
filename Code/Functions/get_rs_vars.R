# --- --- --- --- --- --- --- --- --- ---
#
# Load remote sensing variables: 
#
#  --- --- --- --- --- --- --- --- --- ---

# Load all variables to later be joined with puzzle locations

require(sf)
require(mapview)
require(raster)
library(rnaturalearth)
require("rnaturalearthdata")
require(terra)
require(blackmarbler) # https://worldbank.github.io/blackmarbler/
library(geodata)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(lubridate)
require(terra)
library(FedData)
require(sf)
require(tmap)
library(rstac)
library(gdalcubes)
library(stars)
library(tmap)
library(dplyr)


# --- --- --- --- --- --- --- --- --- --- --- ---
# NDVI - From Google Earth Engine
# --- --- --- --- --- --- --- --- --- --- --- ---

ndvi = raster('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/SF_EastBay_NDVI_Sentinel_10_v4.tif')

# --- --- --- --- --- --- --- --- --- --- --- ---
# Elevation - # Get the NED (USA ONLY) - USGS
# --- --- --- --- --- --- --- --- --- --- --- ---

NED <- get_ned(
  template = st_as_sfc(st_bbox(puzzles_lauren_sf)),
  label = "East Bay"
)

vect_puzzles_lauren_sf = vect(puzzles_lauren_sf)
NED_UTM = terra::project(NED, y = vect_puzzles_lauren_sf )
NED_UTM_raster = raster(NED_UTM)


# --- --- --- --- --- --- --- --- --- --- --- ---
# Landcover - From NLDC
# --- --- --- --- --- --- --- --- --- --- --- ---

xmin = -123.1738
ymin = 37.5398
xmax = -122.0818
ymax = 37.9998 

crssa=4326
study_area_bbox = st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))

CEC_map <- rast(
  "/Users/diegoellis/Desktop/Projects/Postdoc/OSM_for_Ecology/land_cover_2020v2_30m_tif/NA_NALCMS_landcover_2020v2_30m/data/NA_NALCMS_landcover_2020v2_30m.tif"
) 

bbox_sf <- st_as_sfc(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), 
                             crs = st_crs(4326)))

bbox_reprojected <- st_transform(bbox_sf, crs = st_crs(CEC_map))
bbox_vect <- vect(bbox_reprojected)

# Clip the raster
CEC_map_clipped <- terra::crop(CEC_map, bbox_vect)
plot(CEC_map_clipped)

# --- --- --- --- --- --- --- --- --- --- --- ---
# Impervious Surface - From NLDC
# --- --- --- --- --- --- --- --- --- --- --- ---

# See here: https://www.mrlc.gov/data/nlcd-imperviousness-conus-all-years

ncld_imp_surf_2023 <- raster('/Users/diegoellis/Downloads/nlcd_2021_impervious_l48_20230630/nlcd_2021_impervious_l48_20230630.img')
# ncld_imp_surf_2023 <- raster("/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/NLCD_impervious_2021_release_all_files_20230630/nlcd_2021_impervious_l48_20230630.img")

# --- --- --- --- --- --- --- --- --- --- --- ---
# National Landcover
# --- --- --- --- --- --- --- --- --- --- --- ---

CEC_map <- rast(
  "/Users/diegoellis/Desktop/Projects/Postdoc/OSM_for_Ecology/land_cover_2020v2_30m_tif/NA_NALCMS_landcover_2020v2_30m/data/NA_NALCMS_landcover_2020v2_30m.tif"
) 


# Clip landcover to smaller bounding box:
# Define bounding box coordinates in longitude and latitude (WGS84)
lon_min <- -123.0  
lon_max <- -121.0  
lat_min <- 37.0    
lat_max <- 38.5    

# Create a data frame with bounding box coordinates
bbox_df <- data.frame(
  lon = c(lon_min, lon_max, lon_max, lon_min, lon_min),
  lat = c(lat_min, lat_min, lat_max, lat_max, lat_min)
)

# Convert to an sf polygon
bbox_sf <- st_as_sf(bbox_df, coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Get the CRS of the raster
raster_crs <- crs(CEC_map)

# Transform the bounding box to match the raster's CRS
bbox_sf_proj <- st_transform(bbox_sf, crs = crs(CEC_map))

bbox_vect <- vect(bbox_sf_proj) # convert to spatvector
cropped_raster <- terra::crop(CEC_map, bbox_vect)

cropped_raster_r = raster(cropped_raster)
plot(cropped_raster)

# --- --- --- --- --- --- --- --- --- --- --- ---
# Human modification, Bio 1, Bio 12 - have not # # Mask the water:
# --- --- --- --- --- --- --- --- --- --- --- ---

continents <- ne_countries(scale = "medium", returnclass = "sf")
america_continents <- continents[continents$continent %in% c("North America"), ]
america_continents <- st_transform(america_continents, crs(ndvi))
human_mod_americas_masked = raster('/Users/diegoellis/Downloads/PressPulsePause/hmod_americas_masked.tif')
continents <- ne_countries(scale = "medium", returnclass = "sf")
bio1_masked = raster('/Users/diegoellis/Downloads/PressPulsePause/bio1_americas_masked.tif')

bio_precip = raster('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/CHELSA_pr_12_1981-2010_V.2.1.tif')

# bio_precip_masked = crop(bio_precip,bayarea)


# --- --- --- --- --- --- --- --- --- ---
# Nightlights
# --- --- --- --- --- --- --- --- --- ---
# 
# # bearer <- get_nasa_token(username = "XXX", 
# #                          password = "XXX")
# 
# bearer <- "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImRpZWdvX2VsbGlzIiwiZXhwIjoxNzM5MTE5MzY1LCJpYXQiOjE3MzM5MzUzNjUsImlzcyI6Imh0dHBzOi8vdXJzLmVhcnRoZGF0YS5uYXNhLmdvdiIsImlkZW50aXR5X3Byb3ZpZGVyIjoiZWRsX29wcyIsImFzc3VyYW5jZV9sZXZlbCI6Mn0.oV0XhM6FSwS8dwfoT0jLOJi7GXqH27DyvCqDQqBv6b_4engn5b_yEl6X7LAMwpo2GYDUYS10X7knfzXKb-C_NoPu8IRhnlu10HhKD0Eqrw_aqKOy0NC4GvbGsxWbxOxVh70USapxs8x4k27vfYNXA2ZuTsqDXH9jbeSr6PsTvFHIBBitmMKAPmhhh-voahb4w_L8uIrhe41dSIjMayRse1xrRCmhfowD4dDhnbNbEjAPvwuhwKVTlibJQ5oX77-vsa0Ep3uvWHA5ZdiPojvQk28pOB-dgAxU1mHRXI0qpe_-Y8j1vXxiL_hoOp2pRlTlb4yzACst5f6bwVqohQ1P-A"
# ### ROI
# roi_sf <- gadm(country = "USA", level=1, path = tempdir())  |> 
#   dplyr::filter(NAME_1 == 'California')
# 
# # Clip to my study extent:
# roi_sf_study_area = terra::crop(roi_sf, st_as_sf(puzzles_lauren_sf) )
# 
# 
# ### Annual data: raster for 2022
# r_2022 <- bm_raster(roi_sf = roi_sf_study_area,
#                     product_id = "VNP46A4",
#                     date = 2022,
#                     bearer = bearer)
# 
# r <- r_2022 |> terra::mask(roi_sf_study_area)
# 
# ## Distribution is skewed, so log
# r[] <- log(r[] + 1)
# 
# ##### Map
# ggplot() +
#   geom_spatraster(data = r) +
#   scale_fill_gradient2(low = "black",
#                        mid = "yellow",
#                        high = "red",
#                        midpoint = 4.5,
#                        na.value = "transparent") +
#   labs(title = "Nighttime Lights: October 2021") +
#   coord_sf() +
#   theme_void() +
#   theme(plot.title = element_text(face = "bold", hjust = 0.5),
#         legend.position = "none")

# --- --- --- --- --- ---

# # National Walkabiltiy Score:
walkabiltiy = st_read('/Users/diegoellis/Downloads/UrbanEco_EJ_Datasets/WalkabilityIndex/Natl_WI.gdb')
# # We have this, but not using OSM 

# # OSM Combined high res Landcover
# bayarea = raster('/Users/diegoellis/Desktop/Projects/Postdoc/OSM_for_Ecology/BayArea_OSM-enhanced_lcover_map.tif')
# bayarea <- projectRaster(bayarea, crs = crs(america_continents))

# Fahrer Home 

# gdalcubes::gdalcubes_options(parallel = TRUE)
# 
# 
# box <- c(xmin=-122.51, ymin=37.71, xmax=-122.36, ymax=37.81) 
# box <- c(xmin=-122.55, ymin=37.65, xmax=-122.25, ymax=37.95)
# # box <- c(xmin=-122.51027, ymin=37.59500, xmax=--122.01957, ymax=38.11594)
# 
# # Make it wider:
# start_date <- "2022-06-01"
# end_date <- "2022-12-31"
# items <-
#   stac("https://earth-search.aws.element84.com/v0/") |>
#   stac_search(collections = "sentinel-s2-l2a-cogs",
#               bbox = box,
#               datetime = paste(start_date, end_date, sep="/"),
#               limit = 100) |>
#   ext_query("eo:cloud_cover" < 20) |>
#   post_request()
# 
# col <- stac_image_collection(items$features, asset_names = c("B08", "B04", "SCL"))
# 
# cube <- cube_view(srs ="EPSG:4326",
#                   extent = list(t0 = start_date, t1 = end_date,
#                                 left = box[1], right = box[3],
#                                 top = box[4], bottom = box[2]),
#                   dx = 0.0001, dy = 0.0001, dt = "P1D",
#                   aggregation = "median", resampling = "average")
# 
# mask <- image_mask("SCL", values=c(3, 8, 9)) # mask clouds and cloud shadows
# 
# data <-  raster_cube(col, cube, mask = mask)
# 
# ndvi <- data |>
#   select_bands(c("B04", "B08")) |>
#   apply_pixel("(B08-B04)/(B08+B04)", "NDVI") |>
#   reduce_time(c("mean(NDVI)"))
# 
# pal8 <- c("#1F78B4",  "#A6CEE3",  "#FDBF6F","#B2DF8A","#33A02C")
# 
# pal_mako = viridisLite::mako(30)
# ndvi_stars <- st_as_stars(ndvi)
# 
# # mako <- tm_scale_continuous(values = viridisLite::mako(30))
# # fill <- tm_scale_continuous(values = "Greens")
# 
# di# tmap::tm_shape(ndvi_stars) + tm_raster(col.scale = mako)
# 
# # tmap::tm_shape(ndvi_stars) + tm_raster(col.scale=viridisLite::mako())
# 
# tmap::tm_shape(ndvi_stars) + tm_raster(palette = pal_mako, title='San Francisco')
# 

# poly_puzzle <- ndvi |> extract_geom(puzzles_lauren_sf_buf, FUN = mean, reduce_time = TRUE, merge=TRUE)
# poly <- ndvi |> extract_geom(puzzles_lauren_sf)



# # Now for luren:
# # For Lauren"
# require(raster)
# 
# sf <- "/vsicurl/https://dsl.richmond.edu/panorama/redlining/static/citiesData/CASanFrancisco1937/geojson.json" |>
#   st_read() |>
#   st_make_valid() |>
#   dplyr::select(-label_coords)
# 
# PADUS <- get_padus(
#   template = st_as_sfc(st_bbox(puzzles_lauren_sf)),
#   label = "East Bay"
# ) # |> dplyr::select(GAP_Sts, Loc_Nm)
# 
# PADUS = PADUS$Manager_Name |> dplyr::select(GAP_Sts, Loc_Nm)

# Not happy with this one either
# Impervious surface: from this paper: https://essd.copernicus.org/articles/14/1831/2022/
# imp_surf_30 =raster('/Users/diegoellis/Desktop/Projects/Postdoc/Misc_proj_data/BayArea/SF_EastBay_GISD30_Impervious_Surface_30m.tif')
