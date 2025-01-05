
# Send plot of relationship to Carl

# HFI and income


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Rasterize Income layer - SocioEcoSDM
# Target Group Background Sampling
#
#
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Use the entire distirbution of GBIF records to make a target group background sampling - for Income
# And then for greenspace?

require(terra)
require(sp)
require(sf)
require(tidycensus)
require(tigris)
require(tidyverse)
library(earthdatalogin)
library(rstac)
library(gdalcubes)
library(spData)
library(sf)
gdalcubes_options(parallel = TRUE) 

# Download Income data
state_income <- get_acs(
  state = "CA",
  county = c('San Francisco'), # Alameda", "Contra Costa"
  geography = "block group",
  variables = c(medinc = "B19013_001"),
  geometry = TRUE,
  year = 2020,
  output='wide'
)

# GBIF DB: Download all necessary bodiversity sources




# Load Nightlight Raster
# MMonarch Butterfly
load('/Users/diegoellis/Downloads/Danaus plexippus (Linnaeus, 1758).Rdata')

species_sf = gbif_data_d |> filter(stateProvince =='California') %>% st_as_sf(coords = c('decimalLongitude', 'decimalLatitude'), crs = st_crs(4326))  %>% st_transform(st_crs(state_income))

gbif_state_inters <- st_filter(species_sf, state_income)


imp_surf_30m = raster("~/Desktop/Projects/Postdoc/Misc_proj_data/BayArea/SF_EastBay_GISD30_Impervious_Surface_30m.tif")
imp_surf_30m = projectRaster(imp_surf_30m, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
bio_stack = rast(imp_surf_30m)

gbif_state_USCENSUS = st_join(gbif_state_inters, state_income)

state_income %>%
  ggplot(aes(fill = medincM)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "viridis", direction = -1) + # Reversed 'viridis' palette (dark = high, light = low)
  geom_sf(data = gbif_state_USCENSUS, aes(fill = medincM), size = 0.01, color = 'black', alpha = 0.7) + # Adding transparency to points
  theme_bw() + 
  ggtitle(paste0(unique(species_sf$states_abbrev), ' species records \n across income of census tracts')) +
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16)) +
  theme(legend.key.size = unit(2, "lines")) # Adjusting legend size

cbg_income_vect <- vect(state_income)
# Dexter ask problem most locations are inside green areas - help with that?
# Water mask


# Reproject cbg_income_vect to match bio_stack's CRS
cbg_income_vect_reproj <- terra::project(cbg_income_vect, crs(bio_stack) )

# Rasterize median household income
income_raster <- terra::rasterize(cbg_income_vect_reproj, bio_stack, field = "medincE") 

# Handle NAs if necessary
income_raster[is.na(income_raster)] <- median(income_raster, na.rm = TRUE)  # Optionally replace NAs with median income

# NAs are likely greenspace and blue space ####

# Normalize income raster between 0 and 1
rescale01 <- function(x) {
  val <- values(x)
  values(x) <- (val - min(val, na.rm = TRUE)) / (max(val, na.rm = TRUE) - min(val, na.rm = TRUE))
  return(x)
}

normalized_income <- rescale01(income_raster)

# Invert if lower income should have higher weight
normalized_income_inverted <- 1 - normalized_income

# No entiendo este ####
# Combine with Species KDE Bias Raster
# Assuming you have a species-specific bias raster (normalized_raster), you can combine it with the income-based raster to create a composite bias

composite_bias_normalized = normalized_income_inverted # Can update ####
 
# Sample background points using the composite bias raster
background_points <- terra::spatSample(x = composite_bias_normalized, size = 2000, method = "weight", xy = TRUE)

# Convert to sf object
back_g_p <- st_as_sf(background_points, coords = c("x", "y"),
                     crs = st_crs(composite_bias_normalized))

back_g_p_trans = st_transform(back_g_p, st_crs(cbg_income_vect))

# Plot to visualize
plot(cbg_income_vect, main = "Composite Bias Raster (Species + Income)")
plot(back_g_p_trans$geometry, cex = 1, add = TRUE)


state_income %>%
  ggplot(aes(fill = medincE)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c(option = "viridis", direction = -1) + # Reversed 'viridis' palette (dark = high, light = low)
  geom_sf(data = back_g_p_trans, aes(fill = medincE), size = 0.01, color = 'white', alpha = 0.7) + # Adding transparency to points
  theme_bw() + 
  ggtitle(paste0(unique(species_sf$states_abbrev), ' species records \n across income of census tracts')) +
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16)) +
  theme(legend.key.size = unit(2, "lines")) +
  geom_sf(data = gbif_state_USCENSUS, aes(fill = medincE), size = 0.01, color = 'red', alpha = 0.7)

