# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Income_race_houlsehold_density
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

library(tidycensus)
library(dplyr)
library(sf)


pop_housing_density = function(df, buffer_size){

  df_sf = st_as_sf(SpatialPointsDataFrame(df,
                                          coords = df[,c('Long', 'Lat')],
                                          proj4string =CRS("+proj=longlat +datum=WGS84")
  ))
  
  
# Download Income, Age, Population, and Housing Data
state_pop_age <- get_acs(
  state = unique(df_sf$states_abbrev),
  county = c("Alameda", "Contra Costa"),
  geography = "block group",
  variables = c(
    pop = "B01003_001",       # Total Population
    housing = "B25001_001"    # Total Housing Units
  ),
  geometry = TRUE,
  year = 2020,
  output = "wide"
)

# Calculate Population and Housing Density
state_pop_age_density <- state_pop_age %>%
  mutate(
    area_sqkm = st_area(geometry) / 1e6,          # Convert area to square kilometers
    pop_density = as.numeric(popE / area_sqkm),              # Population density (people per sq km)
    housing_density = as.numeric(housingE / area_sqkm)       # Housing density (units per sq km)
  )

p_sf = df %>% st_as_sf(coords = c('Long', 'Lat'), crs = st_crs(4326))  %>% st_transform(st_crs(state_pop_age_density))
# Spatial join point locations and income
p_sf_pop_age_density = st_join(p_sf, state_pop_age_density)


if(
  is.na(buffer_size)
){
  
  col_pal = c('#046C9A', 'bisque3')
  
  ggplot_housing_density = ggplot() +
    geom_density(aes(housing_density,
                     fill = "Housing density across puzzles"),
                 alpha = .2,
                 data = p_sf_pop_age_density, linewidth = 0.8)  +
    geom_density(aes(housing_density, fill = "Background Housing density"), alpha = .2, data = state_pop_age_density, linewidth = 0.8) +
    ggtitle('Puzzles \n across housing density of census tracts ') +
    scale_fill_manual(values = col_pal) + theme_classic() + ylab('Sampling density') + xlab('Housing density') +
    theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
          axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
          axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
          axis.title.y = element_text(face = "bold", size = 16 ,color='black')) # +
  # theme(legend.position="none") # Remove legend
  
  print(ggplot_housing_density)
  
  
  return(p_sf_pop_age_density)
}


if(
  !is.na(buffer_size)
){
  df_sf_buffer <- p_sf %>%
    st_buffer(dist = buffer_size) # For example 1000m buffer
  
  buffered_point_joined_pop_age_density <- st_join(
    df_sf_buffer,
    state_pop_age_density, 
    join = st_intersects,
    left = FALSE)

  
  # Calculate the Mean Median Household Income Within Each Buffer
  mean_pop_house_d_buff_p <- buffered_point_joined_pop_age_density %>%
    group_by(Name) %>%
    summarize(mean_housing_density = mean(housing_density, na.rm = TRUE),
              mean_pop_density = mean(pop_density, na.rm = TRUE))
  
  df_sf_w_mean_inc_age_buf <- df %>%
    left_join(mean_pop_house_d_buff_p, by = "Name") |>
    # Now to get it back to our original data framer annotate the mean income age and provide the size of the buffer
    dplyr::select(Name, mean_pop_density, mean_housing_density) |> 
    mutate(buffer_size = paste0(buffer_size))
  
  col_pal = c('#046C9A', 'bisque3')
  
  ggplot_housing_density = ggplot() +
    geom_density(aes(mean_housing_density,
                     fill = "Housing density across puzzles"),
                 alpha = .2,
                 data = df_sf_w_mean_inc_age_buf, linewidth = 0.8, fill = '#046C9A')  +
    geom_density(aes(housing_density, fill = "Background Housing density"), alpha = .2, fill = 'bisque3', data = state_pop_age_density, linewidth = 0.8) +
    ggtitle(paste0('Puzzles \n across housing density of census tracts , buffer size = ', buffer_size)) +
    scale_fill_manual(values = col_pal) + theme_classic() + ylab('Sampling density') + xlab('Housing density') +
    theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
          axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
          axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
          axis.title.y = element_text(face = "bold", size = 16 ,color='black')) # +
  # theme(legend.position="none") # Remove legend
  
  print(ggplot_housing_density)
  
  return(df_sf_w_mean_inc_age_buf)
}

}

