fs::dir_walk(here::here("code"), source)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# In this section, we look at trends of jail populations in each of these counties, primarily using Vera
# data. https://github.com/vera-institute/jail-population-data

# We choose the dates Feb 29 2020 and April 30 2020 as our "before/during" given COVID had not fully hit
# prior to the end of Feb and we are going to look at crimes up until end of April.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### IMPORT & TRANSFORM DATA -------------------------------------------
# Read in the Vera data -- "counties" df was made in code/global_variables.R
# The Vera data is often updated at this link. 
jail_pops <- counties %>% 
  left_join(read_csv("https://raw.githubusercontent.com/vera-institute/jail-population-data/master/jail_population.csv"), 
            counties, by = c("county" = "county_name", 'fips' = 'fips')) %>% 
  mutate(
    month = floor_date(date, "month"),
    year = floor_date(date, "year")
  ) 

# Double checking that these are the counties we want
jail_pops %>%
  distinct(county, state_name)

# There are 6 without jail data at all in Vera. Boston, Denver, Detroit, Tucson, & Phoenix (Suffolk, Denver, Wayne, Pima and Maricopa Counties) 
jail_pops %>% filter(is.na(jail_population)) %>% distinct(county)


# Look for first and last days of jail data available to see which need other data points 
# These counties have Vera jail data, but not on either the exact before or after date we are using.
jail_pops %>%
  group_by(county) %>%
  summarise(
    first_day = min(date),
    last_day = max(date)
  ) %>%
  # We choose to use Feb 29 and April 30 as our pre and during COVID-19 dates to calculate % change in jail pop.
  # DC, Douglas County and Multnomah County have first or last dates that are outside of this window. 
  filter(first_day > as.Date("2020-02-29") | last_day < as.Date("2020-04-30"))

### MANUAL NEWS/EXTERNAL BASED DATA ADDITIONS -----------

# Washington DC: We use Vera's data here, despite the earliest date being Mar 18. Mar 18 #: 1854. After #: 1351
## Additional data source: (references about 1850 in March) https://www.washingtonpost.com/local/legal-issues/dc-jail-inmates-with-coronavirus-barred-from-access-to-lawyers-family-showers-changes-of-clothing-inspectors-say/2020/04/15/69a86c9e-7f36-11ea-9040-68981f488eed_story.html

# Multnomah County (Portland, OR): We use Vera's data, despite the earliest date being Mar. 9. 

# Wayne County (Detroit, MI): Doesn't exist in Vera. We use data from Mar 10 (1381) and May 4 (830) found in article below.
## Additional data source: https://www.freep.com/story/news/local/michigan/2020/05/05/wayne-county-jail-coronavirus-lawsuit-medically-vulnerable-inmates/3076656001/

# Pima County (Tucson, AZ): Doesn't exist in Vera. We use data from "early March" (2000 inmates) and early May (1300) 
## Additional data source: https://www.kold.com/2020/05/04/hundreds-prisoners-released-early-due-covid-concerns/  
## Additional data source: https://tucson.com/news/local/countywide-effort-reduces-jail-population-in-bid-to-reduce-coronavirus-spread-in-tucson/article_939b67cc-d91c-5ff2-a204-8c6d0061d1d5.html

# Douglas County (Omaha, NE): Vera doesn't have an early enough date. We use Mar 10 (1200) from below, and the Vera number from May 1. 
## Additional data source: https://www.omaha.com/news/crime/douglas-county-jail-making-room-in-case-it-needs-coronavirus-quarantine-unit/article_130ded2b-74bb-532d-a1d9-3205e4fe5e53.html#1

# Maricopa County (Phoenix, AZ): Doesn't exist in Vera. The article below has vague dates, but states they "reduced our daily population from approximately 7,100 to 5,785." 
## Additional data source: https://www.abc15.com/news/coronavirus/advocates-push-to-release-low-level-inmates-from-county-jails

# Denver, CO: Doesn't exist in Vera -- Use the article below for Mar 1 (1057 + 749 = 1806) and April 15 (1057)
## Additional data source: https://www.google.com/url?q=https://denverite.com/2020/04/20/denvers-jail-population-is-drastically-shrinking-but-inmates-and-deputies-are-far-from-immune-to-coronavirus/&sa=D&ust=1594324380178000&usg=AFQjCNEYkJJ4yOzOoTSXQfhdcAX_UgltVw

# Suffolk County (Boston, MA): Doesn't exist in Vera. We use the article below to estimate 536 on Feb 24, and 397 on April 27 
## Additional data source: https://www.mass.gov/lists/weekly-inmate-count-2020

# Make a df that shows percent change from 2/29 to 4/30, where possible. Others use earliest and latest dates possible, as defined above.
# These are all the manual ones, where there's either no good start or end date, so we those fill in using the sources above.
jail_pop_additions <- data_frame(
  location = c("Boston, MA", "Washington, DC", "Detroit, MI", "Portland, OR", "Tucson, AZ", "Omaha, NE", "Phoenix, AZ", "Pittsburgh, PA", "Denver, CO"),
  county = c("Suffolk County", "District of Columbia", "Wayne County", "Multnomah County", "Pima County", "Douglas County", "Maricopa County", "Allegheny County", "Denver County"),
  state_name = c("Massachusetts", "", "Michigan", "Oregon", "Arizona", "Nebraska", "Arizona", "Pennsylvania", "Colorado"),
  before = c(536, 1854, 1381, 1145, 2000, 1200, 7100, 2488, 1806),
  before_date = c("2020-02-24", "2020-03-18", "2020-03-10", "2020-03-09", "2020-03-01", "2020-03-10", NA, "2020-02-29", "2020-03-01"),
  after = c(397, 1351, 830, 715, 1300, 1134, 5785, 1714, 1057),
  after_date = c("2020-04-27", "2020-04-30", "2020-05-04", "2020-04-30", "2020-05-04", "2020-05-01", "2020-04-10", "2020-04-08", "2020-04-30")
) %>%
  mutate_at(vars(before_date, after_date), as.Date)


### COMBINE ---------------------

# A df with all locations containing before data, after data, and percent change.
jail_pop_by_change <- jail_pops %>%
  # First remove the counties with manual data
  filter(!county %in% jail_pop_additions$county) %>%
  group_by(location, county, state_name, fips) %>%
  # These are our dates of interest to calculate change
  filter(date == as.Date("2020-02-29") | date == as.Date("2020-04-30")) %>%
  # Arrange by first date
  arrange(date) %>%
  # Summarize based on these dates
  summarise(
    before = first(jail_population),
    before_date = min(date),
    after = last(jail_population),
    after_date = max(date)
  ) %>%
  # Add in the manual locations
  bind_rows(jail_pop_additions) %>%
  # Calculate percent change
  mutate(percent_change = (after - before) / before,
         clean_percent_change_jail = scales::percent(percent_change))


# All of these places have decarcerated, between 5% and 40% between Feb 29 and April 30 (where those dates existed)
# except for Atlanta where the jail population has risen. 
jail_pop_by_change %>%
  ggplot() +
  geom_histogram(aes(x = percent_change), binwidth = 0.10, color = "white") +
  scale_y_continuous(limits = c(0, 13))

# A line chart of the places that have enough daily data, in case useful.
jail_pops %>%
  filter(month < "2020-06-01" & month > "2020-02-01") %>%
  ggplot(aes(x = date, y = jail_population)) +
  geom_line(size = 0.8, aes(group = 1), na.rm = F) +
  geom_point(aes(x = as.Date("2020-03-01"), y = 0), col = "white") +
  facet_wrap(~location, scales = "free_y", nrow = 6) +
  scale_color_manual(values = c("#3c3532", "#fabeaf"), guide = FALSE) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0)) +
  labs(title = "Jail Population Over Time", x = "Month", y = "")
