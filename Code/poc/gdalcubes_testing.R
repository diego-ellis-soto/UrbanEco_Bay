# --- --- --- --- --- --- --- --- --- --- --- ---
# Moving across the Bay: Wildlife
# --- --- --- --- --- --- --- --- --- --- --- ---

# Load Lauren's puzzles:


# --- --- --- --- --- --- --- --- --- --- --- ---
# Get GEOID for East Bay

# Get human mobility data for California

# Look at POI places of interest? 

# Weekly mobility patterns

# Build access score

# Lake Chabot GEO-ID

# --- --- --- --- --- --- --- --- --- --- --- ---


require(sf)
require(tmap)
library(rstac)
library(gdalcubes)
library(stars)
library(tmap)
library(dplyr)
gdalcubes::gdalcubes_options(parallel = TRUE)


box <- c(xmin=-122.51, ymin=37.71, xmax=-122.36, ymax=37.81) 
box <- c(xmin=-122.55, ymin=37.65, xmax=-122.25, ymax=37.95)

# Make it wider:
start_date <- "2022-06-01"
end_date <- "2022-12-31"
items <-
  stac("https://earth-search.aws.element84.com/v0/") |>
  stac_search(collections = "sentinel-s2-l2a-cogs",
              bbox = box,
              datetime = paste(start_date, end_date, sep="/"),
              limit = 100) |>
  ext_query("eo:cloud_cover" < 20) |>
  post_request()

col <- stac_image_collection(items$features, asset_names = c("B08", "B04", "SCL"))

cube <- cube_view(srs ="EPSG:4326",
                  extent = list(t0 = start_date, t1 = end_date,
                                left = box[1], right = box[3],
                                top = box[4], bottom = box[2]),
                  dx = 0.0001, dy = 0.0001, dt = "P1D",
                  aggregation = "median", resampling = "average")

mask <- image_mask("SCL", values=c(3, 8, 9)) # mask clouds and cloud shadows

data <-  raster_cube(col, cube, mask = mask)

ndvi <- data |>
  select_bands(c("B04", "B08")) |>
  apply_pixel("(B08-B04)/(B08+B04)", "NDVI") |>
  reduce_time(c("mean(NDVI)"))

pal8 <- c("#1F78B4",  "#A6CEE3",  "#FDBF6F","#B2DF8A","#33A02C")

pal_mako = viridisLite::mako(30)
ndvi_stars <- st_as_stars(ndvi)

mako <- tm_scale_continuous(values = viridisLite::mako(30))
fill <- tm_scale_continuous(values = "Greens")

tmap::tm_shape(ndvi_stars) + tm_raster(col.scale = mako)

tmap::tm_shape(ndvi_stars) + tm_raster(col.scale=viridisLite::mako())

tmap::tm_shape(ndvi_stars) + tm_raster(palette = pal_mako, title='San Francisco')

# Now for luren:
# For Lauren"

