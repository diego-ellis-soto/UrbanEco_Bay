# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# East Bay Region: UWIN sites
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Ask Dextrer: What to do about polygons 500m. Distance2 water and Distance2 green.
# Ask Dexter about buffers! 
# Load Locations
# require(rgdal)
require(sp)
require(sf)
require(tidycensus)
require(tigris)
require(tidyverse)
options(tigris_use_cache = TRUE)
census_api_key('88f3fbb46861a0190c673e39c99bd3c067f15072', install = TRUE, overwrite = TRUE)
# census_api_key('88f3fbb46861a0190c673e39c99bd3c067f15072', install = TRUE)
# puzzles = read.csv('/Users/diegoellis/Downloads/StantonPuzzleStudyLocations_11012024.csv') |>
#   mutate(states_abbrev ='CA')
puzzles = read.csv('/Users/diegoellis/Downloads/StantonPuzzleStudyLocations_11042024.csv') |>
  mutate(states_abbrev ='CA')

get_income_age=function(df, buffer_size=NA){
  
  df_sf = st_as_sf(SpatialPointsDataFrame(df,
                                               coords = df[,c('Long', 'Lat')],
                                               proj4string =CRS("+proj=longlat +datum=WGS84")
  ))
  # Download Income data
  state_income_age <- get_acs(
    state = unique(df_sf$states_abbrev),
    county = c("Alameda", "Contra Costa"),
    geography = "block group",
    variables = c(medinc = "B19013_001",
                  medage = 'B01002_001'),
    geometry = TRUE,
    year = 2020,
    output='wide'
  )

  
  p_sf = df %>% st_as_sf(coords = c('Long', 'Lat'), crs = st_crs(4326))  %>% st_transform(st_crs(state_income_age))
  # Spatial join point locations and income
  p_sf_income_age = st_join(p_sf, state_income_age)
   # Get buffer
  if(
  is.na(buffer_size)
  ){
    return(p_sf_income_age)
  }
  
  if(
    !is.na(buffer_size)
  ){
    df_sf_buffer <- p_sf %>%
      st_buffer(dist = buffer_size) # For example 1000m buffer
    
    buffered_point_joined_income_age <- st_join(
      df_sf_buffer,
      state_income_age, 
      join = st_intersects,
      left = FALSE)
    
    # Calculate the Mean Median Household Income Within Each Buffer
    mean_income_buff_p <- buffered_point_joined_income_age %>%
      group_by(Name) %>%
      summarize(mean_income = mean(medincE, na.rm = TRUE),
                mean_age = mean(medageE, na.rm = TRUE))
    
    df_sf_w_mean_inc_age_buf <- df %>%
      left_join(mean_income_buff_p, by = "Name") |>
      # Now to get it back to our original data framer annotate the mean income age and provide the size of the buffer
      dplyr::select(Name, mean_income, mean_age) |> 
      mutate(buffer_size = paste0(buffer_size))
    
    col_pal = c('#046C9A', 'bisque3')
    
    ggplot_income = ggplot() +
      geom_density(aes(mean_income/1000,
                       fill = "Income across puzzles"),
                   alpha = .2,
                   data = df_sf_w_mean_inc_age_buf, linewidth = 0.8)  +
      geom_density(aes(medincE/1000, fill = "State background income"), alpha = .2, data = state_income_age, linewidth = 0.8) +
      ggtitle('Puzzles \n across income of census tracts ') +
      scale_fill_manual(values = col_pal) + theme_classic() + ylab('Sampling density') + xlab('Median household income in $') +
      theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
            axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
            axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
            axis.title.y = element_text(face = "bold", size = 16 ,color='black'))+xlim(0, 300) # +
      # theme(legend.position="none") # Remove legend
    
    print(ggplot_income)
    
    return(df_sf_w_mean_inc_age_buf)
  }
  
  
  
    
  }

get_percent_white=function(df, buffer_size=NA){

  df_sf = st_as_sf(SpatialPointsDataFrame(df,
                                          coords = df[,c('Long', 'Lat')],
                                          proj4string =CRS("+proj=longlat +datum=WGS84")))
  
  racevars <- c(White = "P2_005N", 
                Black = "P2_006N", 
                Asian = "P2_008N", 
                Hispanic = "P2_002N")
  
  state_race <- get_decennial(
    geography = "block group",
    variables = racevars,
    state = 'CA',
    county = c("Alameda", "Contra Costa"),
    geometry = TRUE,
    summary_var = "P2_001N",
    year = 2020
  ) 
  
  df_sf_transf = st_transform(df_sf, st_crs(state_race))
  
  df_state_race = st_join(df_sf_transf, state_race)
  
  state_race = state_race  %>%
    mutate(percent = 100 * (value / summary_value))
  
  df_racial_comp = df_state_race  %>%
    mutate(percent = 100 * (value / summary_value)) %>% 
    drop_na(variable) 
  
    if(
    is.na(buffer_size)
  ){
      
      df_racial_comp_comp_tmp = df_racial_comp %>% as_tibble() %>% dplyr::select(variable, percent) %>% mutate(type='Puzzle')
      
      state_race_tmp = state_race %>% as_tibble() %>% dplyr::select(variable, percent)  %>% mutate(type='Alameda & Contra Costa')
      
      tmp_race = rbind(df_racial_comp_comp_tmp, state_race_tmp)
      
      tmp_race_white = tmp_race[tmp_race$variable =='White',] |> drop_na()
      
      tmp_race_white$type = as.factor(tmp_race_white$type)
      
      
      # col_pal = c('#046C9A', 'bisque3')
      col_pal <- c('Puzzle' = '#046C9A', 'Alameda & Contra Costa' = 'bisque3')
      
      
      plot_racial_makeup_white = ggplot(tmp_race[tmp_race$variable %in% 'White',], aes(percent)) +
        geom_density(data = tmp_race[tmp_race$variable %in% 'White',], aes(fill = type), alpha = 0.4, linewidth = 0.8) +
        ggtitle(' Puzzle locations across demographics ') +
        scale_fill_manual(values = col_pal) + theme_classic() + ylab('Density') + xlab('Percentage (in %) of census block group \n identifying as white') +
        theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
              axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
              axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
              axis.title.y = element_text(face = "bold", size = 16 ,color='black')) # +
        # theme(legend.position="none") # Remove legend
      
      print(plot_racial_makeup_white)
      
      
    return(p_sf_income_age)
  }
  # Now if not buffered:
  if(
    !is.na(buffer_size)
  ){
  
    df_sf_transf_buffer <- df_sf_transf %>%
      st_buffer(dist = buffer_size) # For example 1000m buffer
    
    buffered_point_joined_race <- st_join(
      df_sf_transf_buffer,
      state_race, 
      join = st_intersects,
      left = FALSE)
    
    df_racial_comp_buffered = buffered_point_joined_race  %>%
      mutate(percent = 100 * (value / summary_value)) %>% 
      drop_na(variable) 
    
    # mean_income_buff_p <- buffered_point_joined_income_age %>%
    #   group_by(Name) %>%
    #   summarize(mean_income = mean(medincE, na.rm = TRUE),
    #             mean_age = mean(medageE, na.rm = TRUE))
    
    
    # Calculate the Mean Median Household Income Within Each Buffer
    mean_percent_white_buffered = 
      buffered_point_joined_race %>% # filter(variable=='White') |> 
      group_by(Name) # %>%
      # summarize(mean_percent_white = mean(percent, na.rm = TRUE))
                
    
    mean_percent_white_buffered_white = mean_percent_white_buffered |> dplyr::filter(variable=='White')
    
    annotated_df_race <- df %>%
      left_join(mean_percent_white_buffered_white, by = "Name") |>
      # Now to get it back to our original data framer annotate the mean income age and provide the size of the buffer
      dplyr::select(Name, percent) |> 
      mutate(buffer_size = paste0(buffer_size),
             mean_percent_white = percent) |> dplyr::select(-percent)
    
    
    # annotated_df_race_buf = mean_percent_white_buffered_white  %>% as_tibble() |> dplyr::select(variable, percent) |> mutate(type='Puzzle')
    
    return_buffered_percent_white = mean_percent_white_buffered |> dplyr::filter(variable=='White') |>
      group_by(Name) |>
      summarize(mean_percent_white = mean(percent, na.rm = TRUE)) |> mutate(percent = mean_percent_white, variable= 'White', type = 'Puzzle')
    
    return_buffered_percent_white = return_buffered_percent_white |> as.tibble()  |> dplyr::select(Name,percent, variable, type)

    state_race_tmp = state_race %>% as_tibble() %>% dplyr::select(variable, percent)  %>% mutate(type='Alameda & Contra Costa') |> filter(variable=='White')
    
    tmp_race = rbind(return_buffered_percent_white[,c('percent','variable','type')],
                     state_race_tmp) |> drop_na()
    
    tmp_race$type = as.factor(tmp_race$type)
    
    
    col_pal <- c('Puzzle' = '#046C9A', 'Alameda & Contra Costa' = 'bisque3')
    plot_racial_makeup_white_buf = ggplot(tmp_race[tmp_race$variable %in% 'White',], aes(percent)) +
      geom_density(data = tmp_race[tmp_race$variable %in% 'White',], aes(fill = type), alpha = 0.4, linewidth = 0.8) +
      ggtitle(paste0(' Puzzle locations across demographics , buffer = ', buffer_size)) +
      scale_fill_manual(values = col_pal) + theme_classic() + ylab('Density') + xlab('Percentage (in %) of census block group \n identifying as white') +
      theme(axis.text.x = element_text(face = "bold", size = 16 ,color='black'),
            axis.title.x = element_text(face = "bold", size = 16 ,color='black'),
            axis.text.y = element_text(face = "bold", size = 16 ,color='black'),
            axis.title.y = element_text(face = "bold", size = 16 ,color='black')) # +
      # theme(legend.position="none") # Remove legend
    
    print(plot_racial_makeup_white_buf)
    
    return_buffered_percent_white_merged = return_buffered_percent_white |> mutate(mean_percent_white = percent) |> dplyr::select(Name, mean_percent_white) |> mutate(buffer_size = paste0(buffer_size))
    
    return(return_buffered_percent_white_merged)
  }
  
}
