# --- --- --- --- --- --- --- --- --- --- --- ---
# Exploring Earth Data Login
# --- --- --- --- --- --- --- --- --- --- --- ---
# https://brazil-data-cube.github.io/rstac/articles/rstac-02-cql2.html
# https://boettiger-lab.github.io/earthdatalogin/articles/gdalcubes-stac-cog.html

require("earthdatalogin")
library(rstac)
library(gdalcubes)
require(mapview)
library(rstac)
library(gdalcubes)
library(spData)
library(sf)
require(stars)
require('gdalcubes')
library(FedData)
require('FedData')

gdalcubes_options(parallel = TRUE) 
edl_netrc(
  username = 'diego.ellissoto@yale.edu',
  password = 'Atelopus123!'
)

with_gdalcubes()
gdalcubes_options(parallel = TRUE) 
gdal_cloud_config()

edl_s3_token(daac = "https://data.lpdaac.earthdatacloud.nasa.gov")


bbox <- c(xmin=-123, ymin=37.25, xmax=-122.0, ymax=38.25) 
start <- "2021-12-01"
end <- "2022-05-31"

# Find all assets from the desired catalog:
items <- stac("https://cmr.earthdata.nasa.gov/stac/LPCLOUD") |> 
  stac_search(collections = "HLSL30.v2.0",
              bbox = bbox,
              datetime = paste(start,end, sep = "/")) |>
  post_request() |>
  items_fetch() |>
  items_filter(properties[["eo:cloud_cover"]] < 20)






# Load FedData and magrittr
library(FedData)
library(magrittr)
library(terra)

# Install mapview if necessary
if (!require("mapview")) {
  install.packages("mapview")
}

library(mapview)
mapviewOptions(
  basemaps = c(),
  homebutton = FALSE,
  query.position = "topright",
  query.digits = 2,
  query.prefix = "",
  legend.pos = "bottomright",
  platform = "leaflet",
  fgb = TRUE,
  georaster = TRUE
)

# Create a nice mapview template
plot_map <-
  function(x, ...) {
    if (inherits(x, "SpatRaster")) {
      x %<>%
        as("Raster")
    }
    
    bounds <-
      FedData::meve %>%
      sf::st_bbox() %>%
      as.list()
    
    mapview::mapview(x, ...)@map %>%
      leaflet::removeLayersControl() %>%
      leaflet::addTiles(
        urlTemplate = "https://basemap.nationalmap.gov/ArcGIS/rest/services/USGSShadedReliefOnly/MapServer/tile/{z}/{y}/{x}"
      ) %>%
      leaflet::addTiles(
        urlTemplate = "https://tiles.stadiamaps.com/tiles/stamen_toner_lines/{z}/{x}/{y}.png"
      ) %>%
      leaflet::addTiles(
        urlTemplate = "https://tiles.stadiamaps.com/tiles/stamen_toner_labels/{z}/{x}/{y}.png"
      ) %>%
      # leaflet::addProviderTiles("Stamen.TonerLines") %>%
      # leaflet::addProviderTiles("Stamen.TonerLabels") %>%
      leaflet::addPolygons(
        data = FedData::meve,
        color = "black",
        fill = FALSE,
        options = list(pointerEvents = "none"),
        highlightOptions = list(sendToBack = TRUE)
      ) %>%
      leaflet::fitBounds(
        lng1 = bounds$xmin,
        lng2 = bounds$xmax,
        lat1 = bounds$ymin,
        lat2 = bounds$ymax
      )
  }


# FedData comes loaded with the boundary of Mesa Verde National Park, for testing
# FedData::meve

plot_map(FedData::meve,
         legend = FALSE
)

