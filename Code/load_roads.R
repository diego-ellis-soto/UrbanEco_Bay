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

