
## CRIME VARIABLES -------------

##  ---------
#### PART ONE UCR CRIMES
# 1. Criminal Homicide
# 2. Forcible Rape
# 3. Robbery
# 4. Aggravated Assault
# 5. Burglary
# 6. Larceny-theft (except motor vehicle theft)
# 7. Motor Vehicle Theft
# 8. Arson

# Definitions of Part One crimes
# https://ucr.fbi.gov/crime-in-the-u.s/2011/crime-in-the-u.s.-2011/offense-definitions
# https://www.ucrdatatool.gov/offenses.cfm


# The FBI explains that the reporting of crime statistics is limited to these
# "because they are the crimes most likely to be reported and most likely to occur with
# sufficient frequency to provide an adequate basis for comparison."

part1 <- c(
  "AGGRAVATED", "AGG", "HOMICIDE", "MURDER", "ROBBERY", "LARCENY", "ARSON",
  "BURGLARY", "RAPE", "THEFT", "SHOPLIFTING", "PICKPOCKET"
)


## GEOGRAPHY VARIABLES ----------------
# Source : Google
counties <- tibble::tibble(
  county = c(
    "Fulton County", "Suffolk County",
    "Cook County", "Denver County",
    "Wayne County", "Broward County", "Harris County",
    "Los Angeles County", "Shelby County", "Milwaukee County",
    "Philadelphia County", "Pinellas County", "Multnomah County",
    "Riverside County", "San Diego County", "King County", "San Bernardino County",
    "Jefferson County", "District of Columbia", "San Francisco County", "Travis County", "Pima County",
    "Tarrant County", "Wake County", "Douglas County", "Davidson County", "Maricopa County", "Allegheny County", 
    "Hamilton County"
  ),
  location = c(
    "Atlanta, GA", "Boston, MA", "Chicago, IL",
    "Denver, CO", "Detroit, MI", "Fort Lauderdale, FL",
    "Houston, TX", "Los Angeles, CA", "Memphis, TN", "Milwaukee, WI",
    "Philadelphia, PA", "Pinellas County, FL", "Portland, OR", "Riverside, CA",
    "San Diego County, CA", "Seattle, WA", "San Bernardino, CA", "Louisville, KY",
    "Washington, DC", "San Francisco, CA", "Austin, TX", "Tucson, AZ", "Fort Worth, TX",
    "Raleigh, NC", "Omaha, NE", "Nashville, TN", "Phoenix, AZ", "Pittsburgh, PA", "Cincinnati, OH"
  ),
  fips = c(
    13121,  25025, 17031, 08031, 29223, 12011,
    48201, 06037, 47157, 55079, 42101, 12103, 41051, 06065, 06073,
    53033, 06071, 21111, 11001, 06075, 48453, 04019, 48439, 37183,
    31055, 47037, 04013, 42003, 39061
  )
)

# Checks above — should have 0 rows
tidycensus::fips_codes %>%
  unite("fips", state_code, county_code, sep = "") %>%
  mutate(fips = as.numeric(fips)) %>%
  filter(fips %in% counties$fips) %>%
  left_join(counties, by = "fips") %>%
  filter(county.x != county.y) %>%
  verify(nrow(.) == 0) %>%
  invisible()
