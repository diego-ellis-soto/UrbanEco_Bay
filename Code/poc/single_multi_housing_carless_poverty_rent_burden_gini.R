# Answer Cory Callaghan !!!! 

# Ask Lauren and Chris about these

# We can use the CDC and California Vulnerability scores: 

library(tidycensus)
library(dplyr)
library(sf)

# Fetch housing and socioeconomic variables
data <- get_acs(
  geography = "block group",
  variables = c(
    single_unit = "B25024_002",
    multi_unit = "B25024_003",
    vacancy = "B25004_001",
    poverty = "B17001_002",
    carless = "B08201_002",
    rent_burden = "B25070_010",
    gini_index = "B19083_001"
  ),
  output='wide',
  state = "CA",
  county = c("Alameda", "Contra Costa"),
  geometry = TRUE,
  year = 2020
)

# Calculate metrics
data <- data %>%
  mutate(
    single_unit_rate = single_unitE / (single_unitE + multi_unitE + vacancyE),  # Proportion single unit
    vacancy_rate = vacancyE / (single_unitE + multi_unitE + vacancyE) #, # Vacancy rate
    # carless_rate = carlessE / totalE  # Households without vehicles
  )

# How many people use a car



