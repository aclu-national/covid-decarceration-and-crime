fs::dir_walk(here::here("code"), source)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### ANALYSIS & POPULATION DATA  ###

# After the data has been processed, unify it for further visualization and analysis

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

source(here::here("scripts/01b-crime_categorization_assumptions.R"), echo = FALSE)
source(here::here("scripts/02-decarceration_data.R"), echo = FALSE)
source(here::here("scripts/03-location_data.R"), echo = FALSE)

##### COMBINE ALL CRIME DATA ---------------

# - Add all Part One incident level datasets together

all_part_one_crimes_incident_level <- bind_rows(part1_dataset_list) %>% 
  filter(month >= as.Date("2018-01-01") & month <= as.Date("2020-06-01"))

# - Add all month-level datasets together. 

all_part_one_crimes_month_level <- bind_rows(monthly_dataset_list) %>% 
  filter(month >= as.Date("2018-01-01") & month <= as.Date("2020-06-01"))

# Investigate incident and monthly data to ensure: 
# 1) No unusual gaps. All locations should have 30 months of data except Houston (through May), Fort Worth (begins in 2019) and Portland (only up until April).
# 2) The first and last days of crime data available are Jan 1 2018 and May 31 2020 or later. 

# Notes : 
# - No incomplete months past May except in Portland (only goes through April)
# - Many cities don't have complete June data yet. 
all_part_one_crimes_incident_level %>%
  group_by(location) %>%
  summarise(
    n_months = n_distinct(month),
    first = min(occur_date, na.rm = T),
    last = max(occur_date, na.rm = T)
  ) %>%
  View()

all_part_one_crimes_month_level %>%
  group_by(location) %>%
  summarise(
    n_months = n_distinct(month),
    first = min(month),
    last = max(month)
  ) %>%
  View()

# Combine incident level and month level data by summarizing incident level to monthly 
# (by counting unique incident IDs per month) then binding

all_part_one_crimes <- all_part_one_crimes_incident_level %>%
  group_by(location, month) %>%
  summarise(n = n_distinct(id)) %>%
  bind_rows(all_part_one_crimes_month_level) %>%
  ungroup() %>%
  select(location, month, n)


##### CALCULATE PRE COVID AVERAGES AND RANGE  --------------------------------

# Get the average, min and max crimes for the pre-period
# Assumptions : 
# 1. The pre-period is inclusive of 2018-01-01 - 2020-02-01
# 2. Mean is a reasonable effective metric for average (vs median)

pre_monthly_avg_crimes <- all_part_one_crimes %>%
  filter(month < as.Date("2020-03-01") & month >= as.Date("2018-01-01")) %>%
  group_by(location) %>%
  summarise(
    avg_monthly_crimes_pre = mean(n),
    low_range_pre = min(n),
    high_range_pre = max(n)
  )

# Calculate crime rate & number of crimes per month, then add in pre-period metrics 
# Notes :
# 1.Rate is effectively calculated based on total population as of 2019 Census estimates, so denominator does not change.
# 2.Because we don't have 2020 population data and pop. doesn't change much between 2018 and 2019, we use raw crime numbers for viz throughout instead of rates.

monthly_crimes_with_pre <- all_part_one_crimes %>%
  group_by(location, month) %>%
  left_join(pop_data) %>%
  # Calculating rate per 10k people.
  mutate(monthly_crime_rate = (n / pop) * 10000) %>%
  ungroup() %>%
  # Adding in the pre-covid period averages and stats
  left_join(pre_monthly_avg_crimes) %>%
  # Adding in jail pop. data
  left_join(jail_pop_by_change) %>% 
  select(location, month, n, city, state, pop, monthly_crime_rate, avg_monthly_crimes_pre, low_range_pre, high_range_pre, percent_change_jail = percent_change, clean_percent_change_jail) %>% 
  # For cleaner annotating in charts where we need location name and the % jail pop. change.
  mutate(location_annotate = paste(location, clean_percent_change_jail, sep = " "))


##### CALCULATE COMPARISONS BETWEEN MAR-MAY 2019 and 2020  --------------------------------

# Calculate comparison crime rates and average # of crimes for during COVID-19 period compared to 
# same three months in 2018 and 2019

# Assumptions : 
# 1. COVID period dates are 2020-03-01 to 2020-05-01
# 2. Crime is cyclical -- it is reasonable to compare these three months to the same 3 months in 2019
# 3. Avg crime rate over 3 month period well characterized by a straight average/mean

# Notes : 
# - For rates, we only use 2019 pop data
# - Portland data only runs through April, so analyzed separately

comparison_crimes <- all_part_one_crimes %>%
  group_by(location, month) %>%
  left_join(pop_data) %>%
  ungroup() %>%
  filter(month %in% as.Date(c(
    "2018-03-01", "2018-04-01", "2018-05-01",
    "2019-03-01", "2019-04-01", "2019-05-01",
    "2020-03-01", "2020-04-01", "2020-05-01"
  ))) %>%
  mutate(year = year(month)) %>%
  group_by(location, year) %>%
  # Calculate average crime rate between mar-may, by year
  # Calculate average number of crimes between mar-may, by year
  summarise(
    crime_rate_over_3_months = sum(n) / sum(pop),
    avg_crime_over_3_months = sum(n) / 3
  ) %>%
  ungroup() %>%
  # Portland goes through April, not May.
  filter(location != "Portland, OR")

portland_comparison_crimes <- all_part_one_crimes %>%
  filter(location == "Portland, OR") %>%
  left_join(pop_data) %>%
  filter(month %in% as.Date(c(
    "2018-03-01", "2018-04-01",
    "2019-03-01", "2019-04-01",
    "2020-03-01", "2020-04-01"
  ))) %>%
  mutate(year = year(month)) %>%
  group_by(location, year) %>%
  # Calculate average crime rate between mar-apr, by year
  # Calculate average number of crimes between mar-may, by year
  summarise(
    crime_rate_over_3_months = sum(n) / sum(pop),
    avg_crime_over_3_months = sum(n) / 2
  ) %>%
  ungroup()

# Combine
comparison_crimes %<>%
  bind_rows(portland_comparison_crimes)

# Calculate the percent change in avg crime between mar-may from 2019 to 2020.
# Add jail data in. 

# Notes :
# - 2018 excluded from analysis for 1:1 comparison purposes

percent_change_crime <- comparison_crimes %>%
  filter(year != 2018) %>%
  # We use raw crimes and not rates in our visualizations.
  select(-crime_rate_over_3_months) %>% 
  group_by(location) %>%
  # Average crime for months of mar-may 2020 and 2019 are side by side
  spread(year, avg_crime_over_3_months, sep = "_") %>% 
  mutate(
    perc_change_avg_crime = (year_2020 - year_2019) / year_2019
  ) %>%
  # Adding jail population information
  left_join(jail_pop_by_change) %>% 
  # For better annotation in the graphics.
  mutate(location_annotate = paste(location, clean_percent_change_jail, sep = " "))





