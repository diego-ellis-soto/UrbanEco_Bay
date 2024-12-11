# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load roads
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

require(tigris)
options(tigris_use_cache = TRUE)

# Download road data for Alameda and Contra Costa counties in California
alameda_roads <- roads(state = "CA", county = "Alameda", year = 2020)
contra_costa_roads <- roads(state = "CA", county = "Contra Costa", year = 2020)
# Combine the road data
all_roads <- rbind(alameda_roads, contra_costa_roads)
t_all_roads =  st_transform(all_roads, st_crs(puzzles_lauren_sf_anno_v3))

intersected_roads <- st_intersection(puzzles_lauren_sf_anno_v3, t_all_roads)

t_all_roads_v2 = st_crop(t_all_roads, st_bbox(puzzles_lauren_sf_anno_v3))

mapview(t_all_roads_v2)

# spatstat::pixellate.psp()
# https://gis.stackexchange.com/questions/138861/calculating-road-density-in-r-using-kernel-density