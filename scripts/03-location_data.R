fs::dir_walk(here::here("code"), source)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### LOCATION DATA  ###

# Load 2019 Census population estimates for each locality.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


##### CENSUS POPULATIONS

# This df preps our df of locations to match easily with Census demographics. 
location_df <- counties %>%
  select(location) %>%
  separate(location, into = c("city", "state.abb"), sep = ", ", remove = T) %>%
  left_join(tibble(state = state.name, state.abb = state.abb)) %>%
  mutate(state = if_else(city == "Washington", "District of Columbia", state))

# Use tidycensus to grab the 2019 Census estimate populations for the year 2019.
# At the "place" level. 
# Need census API key in config file for this to work. 
census_places <- tidycensus::get_estimates(geography = "place", product = "population", year = 2019, cache = T) %>%
  filter(variable == "POP") %>%
  mutate(NAME = case_when(
    str_detect(NAME, "Nashville-Davidson") ~ "Nashville, Tennessee",
    str_detect(NAME, "Louisville.Jefferson") ~ "Louisville, Kentucky",
    TRUE ~ NAME
  )) %>%
  separate(NAME, into = c("city", "state"), sep = ", ", remove = T) %>%
  mutate(city = str_remove(city, " city"))

# This grabs those same estimates but at the county level, and combines with the other "place" level
# dataset to complete the demographic info for all places. 
census_full <- tidycensus::get_estimates(geography = "county", product = "population", year = 2019, cache = T) %>%
  filter(variable == "POP") %>%
  filter(str_detect(NAME, "San Diego|Pinellas")) %>%
  bind_rows(census_places) %>%
  mutate(city = case_when(
    NAME == "San Diego County, California" ~ "San Diego County",
    NAME == "Pinellas County, Florida" ~ "Pinellas County",
    TRUE ~ city
  )) %>%
  mutate(state = case_when(
    city == "San Diego County" ~ "California",
    city == "Pinellas County" ~ "Florida",
    TRUE ~ state
  ))

pop_data <- left_join(location_df, census_full) %>%
  # We combine these together to easily join to crimes data later with "location".
  mutate(location = paste0(city, ", ", state.abb)) %>%
  select(city, location, state, pop = value)
