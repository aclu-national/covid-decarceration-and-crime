#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### CRIME DATA ETL ###

# Load location data for all cities and validate for later use
# Data comes in varying forms from cities ranging from month level pdfs to well-formatted incident level datasets
## see https://docs.google.com/spreadsheets/d/1zEAuJ4HB7f4SxodJF33Ja5qVnpquSXpAC0gpvwblxvs/edit#gid=0
## for all data sources hyperlinked

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

fs::dir_walk(here::here("code"), source)

crimes_dataset_list <-list()

#### ------------------------- ATLANTA ------------------------------------####
info_line("Atlanta")

atlanta_2019 <- s3read_using(
  FUN = read_csv3,
  object = "atlanta/2019/COBRA-2009-2019.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  select(offense_id = report_number, occur_date, ucr_literal)


atlanta_2020 <- s3read_using(
  FUN = read_csv3,
  object = "atlanta/2020/COBRA-2020.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  select(offense_id, occur_date, ucr_literal = uc2_literal) %>%
  mutate_at(vars(offense_id), as.character) %>%
  mutate(occur_date = as.Date(occur_date, "%m/%d/%Y"))

crimes_dataset_list[['atlanta']] <- atlanta_2019 %>%
  bind_rows(atlanta_2020) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  # Atlanta 2019 has a few duplicate report numbers. Atlanta 2020 has none. 
  # The distinct() only eliminates a few rows. These look like duplicate offenses with the same id and same offense type.
  distinct() %>%
  mutate(
    year = floor_date(occur_date, "year"),
    month = floor_date(occur_date, "month"),
    location = "Atlanta, GA"
  ) %>%
  select(
    id = offense_id,
    occur_date,
    year,
    month,
    category = ucr_literal,
    location
  ) %>%
  mutate(id = as.character(id)) %>%
  verify_crime_dataset(
    type = "offense_level",
    id_warning = "Atlanta : Contains some duplicate report numbers. It appears there can be multiple offenses per report, but very few reports had multiple
    offenses. May have switched from UCR to NIBRS reporting."
  )

remove_ls(pattern = "atlanta_")
quick_line(crimes_dataset_list[['atlanta']])

#### ------------------------- AUSTIN ------------------------------------####
info_line("Austin")

crimes_dataset_list[['austin']] <- s3read_using(
  FUN = read_csv3,
  object = "austin/Crime_Reports.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(occurred_date, "%m/%d/%Y"),
    year = floor_date(occur_date, "year"),
    month = floor_date(occur_date, "month"),
    location = "Austin, TX"
  ) %>%
  select(
    id = incident_number,
    occur_date,
    year,
    month,
    category = category_description,
    category_num = ucr_category,
    description = highest_offense_description,
    location
  ) %>%
  mutate(id = as.character(id)) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  verify_crime_dataset(type = "incident_level")


remove_ls(pattern = "austin_")


#### ------------------------- BOSTON ------------------------------------####
info_line("Boston")
crimes_dataset_list[['boston']] <- s3read_using(
  FUN = read_csv3,
  object = "boston/tmplswgdmy8.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(occurred_on_date, "%Y-%m-%d"),
    year = floor_date(occur_date, "year"),
    month = floor_date(occur_date, "month"),
    location = "Boston, MA"
  ) %>%
  select(
    id = incident_number,
    occur_date,
    year,
    month,
    category = ucr_part,
    category_num = offense_code,
    description = offense_description,
    location
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  mutate(id = as.character(id)) %>%
  verify_crime_dataset(
    type = "offense_level",
    id_warning = "Incident ids can have multiple rows/offenses" 
    )

remove_ls(pattern = "boston_")

#### ------------------------- CHICAGO ------------------------------------####
info_line("Chicago")
crimes_dataset_list[['chicago']] <- s3read_using(
  FUN = read_csv3,
  object = "chicago/Crimes_-_2001_to_present.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  # The dataset is large so cutting it earlier on. 
  filter(year > 2017) %>%
  mutate(
    occur_date = parse_date_time(date, "mdy hms"),
    occur_date = as.Date(occur_date),
    year = floor_date(occur_date, "year"),
    month = floor_date(occur_date, "month"),
    location = "Chicago, IL",
    id = as.character(id)
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = primary_type,
    category_num = iucr,
    description = description,
    location
  ) %>%
  verify_crime_dataset(type = "incident_level")

remove_ls(pattern = "chicago_")

#### ------------------------- CINCINATTI ------------------------------------####
info_line("Cincinatti")
crimes_dataset_list[['cincinnati']] <- s3read_using(
  FUN = read_csv3,
  object = "cincinnati/PDI__Police_Data_Initiative__Crime_Incidents.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  # Dictionary says "date_from" is the estimated date/time of start of crime.
  mutate(
    occur_date = as.Date(date_from, "%m/%d/%Y"),
    year = floor_date(occur_date, "year"),
    month = floor_date(occur_date, "month"),
    location = "Cincinnati, OH",
    # "Incident No: The unique identifier for this dataset. Each id represents a victim of a crime."
    id = as.character(incident_no)
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  # There are over 1,000 missing ids. adding an estimated unique identifier where there is none. 
  # These four columns should be the same for each incident.
  mutate(id = if_else(is.na(id), paste0(address_x, date_reported, date_from, victim_age), id)) %>% 
  select(id,
    occur_date,
    year,
    month,
    category = ucr_group,
    description = offense,
    location
  ) %>%
  # Offense level data, multiple rows per incident
  verify_crime_dataset(type = "offense_level")

remove_ls(pattern = "cincinnati_")


#### ------------------------- DC ------------------------------------####
info_line("DC")
crimes_dataset_list[['dc']] <- s3read_using(
  FUN = read_csv3,
  object = "dc/Crime_Incidents_in_2019.csv",
  bucket = "aclu-covid-crimes"
) %>%
  mutate(CCN = as.character(CCN)) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "dc/Crime_Incidents_in_2020.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "dc/Crime_Incidents_in_2018.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(start_date, "%Y/%m/%d"),
    occur_date = as.Date(occur_date),
    year = floor_date(occur_date, "year"),
    month = floor_date(occur_date, "month"),
    location = "Washington, DC",
    id = as.character(ccn)
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  # Object ID looks to be unique rows. The CCN is unique except in 50ish cases. Will just count 
  # unique Ids when aggregating to the month level.
  select(id,
    occur_date,
    year,
    month,
    category = offense,
    description = method,
    location
  ) %>%
  verify_crime_dataset(type = "incident_level",
                       id_warning = "There are 49 incidents where id is not unique.")

remove_ls(pattern = "dc_")

#### ------------------------- DENVER ------------------------------------####

info_line("Denver")

crimes_dataset_list[['denver']] <- s3read_using(
  FUN = read_csv3,
  object = "denver/crime.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = parse_date_time(first_occurrence_date, "mdy hms"),
    occur_date = as.Date(occur_date),
    year = floor_date(occur_date, "year"),
    month = floor_date(occur_date, "month"),
    location = "Denver, CO",
    id = as.character(incident_id)
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = offense_category_id,
    category_num = offense_code,
    description = offense_type_id,
    location
  ) %>%
  verify_crime_dataset(type = "offense_level")

remove_ls(pattern = "denver_")

#### ------------------------- DETROIT ------------------------------------####
info_line("Detroit")
crimes_dataset_list[['detroit']] <- s3read_using(
  FUN = read_csv3,
  object = "detroit/RMS_Crime_Incidents.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(incident_timestamp, "%Y/%m/%d"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    location = "Detroit, MI",
    id = as.character(crime_id)
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = offense_category,
    category_num = state_offense_code,
    description = offense_description,
    location
  ) %>%
  verify_crime_dataset(type = "offense_level")

remove_ls(pattern = "detroit_")

#### ------------------------- FT LAUDERDALE ------------------------------------####
info_line("Ft Lauderdale")

crimes_dataset_list[['ft_lauderdale']] <- s3read_using(
  FUN = read_csv3,
  object = "ftlauderdale/Incident.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = parse_date_time(date_time_occurred, "mdy hms"),
    occur_date = as.Date(occur_date),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(case_number),
    location = "Fort Lauderdale, FL"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  # 
  select(id,
    occur_date,
    year,
    month,
    category = offense,
    category_num = incident_disposition_code,
    description = reported_as,
    location
  ) %>%
  verify_crime_dataset(
    type = "incident_level",
    id_warning = "There are few duplicate ids that are mainly child abuse related - these cases potentially had repeat abuses."
  )

remove_ls(pattern = "ft_lauderdale_")

#### ------------------------- FORT WORTH ------------------------------------####
info_line("Fort Worth")
crimes_dataset_list[['fort_worth']] <- s3read_using(
  FUN = read_csv3,
  object = "fortworth/Crime_Data.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(from_date, "%m/%d/%Y"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(case_number),
    location = "Fort Worth, TX"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = nature_of_call,
    category_num = offense,
    description,
    location
  ) %>%
  # Note - only 6 rows with this issue -- parsing failures
  filter(!is.na(id)) %>%
  # Offense level, so multiple rows per incident 
  verify_crime_dataset(type = "offense_level")

remove_ls(pattern = "fort_worth_")

#### ------------------------- HOUSTON ------------------------------------####
info_line("Houston")
houston_objects <- data.table::rbindlist(
  get_bucket("aclu-covid-crimes", prefix = "houston/2018")) %>%
  filter(!Key %in% c("houston/2018/")) %>%
  filter(Key %in% c("houston/2018/jan18.xls", "houston/2018/feb18.xls", "houston/2018/mar18.xls", "houston/2018/apr18.xls", "houston/2018/may18.xls")) %>%
  pull(Key) %>%
  unique()

houston_2018_1 <- map_dfr(
  houston_objects,
  ~ s3read_using(FUN = read_excel, object = .x, bucket = "aclu-covid-crimes")
) %>%
  clean_names()

houston_2018_1 <- houston_2018_1 %>%
  mutate(
    occur_date = as.Date(date, "%m/%d/%Y"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(sample(1:47015, replace = FALSE)),
    location = "Houston, TX"
  ) %>%
  # Each row is supposed to be at incident level, but 2018 doesn't come with any form of unique identifier.
  select(id,
    occur_date,
    year,
    month,
    category = offense_type,
    offense_count = offenses,
    location
  )

# These files are differently reported. 
houston_objects2 <- data.table::rbindlist(
  get_bucket("aclu-covid-crimes", prefix = "houston/2018")) %>%
  filter(!Key %in% c("houston/2018/")) %>%
  filter(!Key %in% c("houston/2018/jan18.xls", "houston/2018/feb18.xls", "houston/2018/mar18.xls", "houston/2018/apr18.xls", "houston/2018/may18.xls")) %>%
  pull(Key) %>%
  unique()

houston_2018_2 <- map_dfr(
  houston_objects2,
  ~ s3read_using(FUN = read_excel, object = .x, bucket = "aclu-covid-crimes", skip = 9)
) %>%
  clean_names() %>%
  select(-starts_with("x"))

houston_2018_2 <- houston_2018_2 %>%
  mutate(
    occur_date = as.Date(occurrence_date, "%Y-%m-%d"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    # Created unique ids here -- assumption is that this is at the incident level. 
    id = as.character(sample(47016:192728, replace = FALSE)),
    location = "Houston, TX"
  ) %>%
  select(id,
    occur_date,
    year,
    month,
    category = nibrs_description,
    offense_count,
    location
  ) %>%
  verify_crime_dataset(type = "incident_level")

houston_2020 <- s3read_using(
  FUN = read_excel,
  object = "houston/2020/NIBRSPublicViewJan1-May31-2020-2.xlsx",
  bucket = "aclu-covid-crimes"
) %>%
  mutate(`ZIP Code` = as.character(`ZIP Code`)) %>%
  clean_names() %>%
  mutate(occur_date = as.Date(occurrence_date))

houston_2019 <- s3read_using(
  FUN = read_excel,
  object = "houston/2019/2019_NIBRSPublicView.Jan1-Dec31 (1).xlsx",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(occur_date = as.Date(occurrence_date))

crimes_dataset_list[['houston']] <- houston_2020 %>%
  bind_rows(houston_2019) %>%
  mutate(
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(incident),
    location = "Houston, TX"
  ) %>%
  select(id,
    occur_date,
    year,
    month,
    category = nibrs_description,
    offense_count,
    location
  ) %>%
  bind_rows(houston_2018_1, houston_2018_2) %>%
  verify_crime_dataset(type = "offense_level")


remove_ls(pattern = "houston_")

#### ------------------------- LOS ANGELES ------------------------------------####
info_line("Los Angeles")
crimes_dataset_list[['los_angeles']] <- s3read_using(
  FUN = read_csv3,
  object = "la/2019/Crime_Data_from_2010_to_2019 (1).csv",
  bucket = "aclu-covid-crimes"
) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "la/2020/Crime_Data_from_2020_to_Present.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(date_occ, "%m/%d/%Y"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(dr_no),
    category_num = as.character(crm_cd),
    location = "Los Angeles, CA"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = crm_cd_desc,
    category_num,
    location
  ) %>%
  verify_crime_dataset(type = "incident_level")

remove_ls(pattern = "los_angeles_")

#### ------------------------- LOUISVILLE ------------------------------------####
info_line("Louisville")
louisville_location <- "https://www.louisville-police.org/ArchiveCenter/ViewFile/Item/85"

# do interactively the first time
# areas <- locate_areas(location, pages = 5)
louisville_areas <- list(c(top = 35.07692, left = 6.36499, bottom = 422.76923, right = 1014.21114))
louisville_out <- extract_tables(louisville_location, pages = 5, area = louisville_areas)
crimes_dataset_list[['louisville']] <- as_tibble(louisville_out[[1]]) %>%
  rename(
    month_1 = V1, monthly_avg = V2, trend = V3, `2016` = V4,
    `2017` = V5, `2018` = V6, `2019` = V7, `2020` = V8
  ) %>%
  select(-c(V9, V10)) %>%
  filter(!month_1 %in% c("UCR Part I Crime Stats", "", "Yearly Total:", "YTD Average:")) %>%
  select(month_1, `2018`:`2020`) %>%
  filter(!month_1 == "T") %>% 
  mutate_at(vars(`2018`:`2020`), as.numeric) %>%
  pivot_longer(cols = c(`2018`:`2020`), names_to = "year") %>%
  mutate(month_yr = paste(month_1, year, sep = ", ")) %>%
  # turning into a date object
  mutate(
    month = parse_date(month_yr, format = "%B, %Y"),
    crime = "Part 1",
    location = "Louisville, KY"
  ) %>%
  select(crime, month, n = value, location) %>%
  # All the months of 2020 are in here but want to get rid of months that haven't occurred.
  filter(n > 0, !is.na(month)) %>%
  verify_crime_dataset(type = "month_level")

remove_ls(pattern = "louisville_")

#### ------------------------- MEMPHIS ------------------------------------####
info_line("Memphis")
crimes_dataset_list[['memphis']] <- s3read_using(
  FUN = read_csv3,
  object = "memphis/Memphis_Police_Department__Public_Safety_Incidents.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = parse_date_time(offense_date, "mdy hms"),
    occur_date = as.Date(occur_date),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    location = "Memphis, TN"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(
    id = crime_id,
    occur_date,
    year,
    month,
    category,
    description = agency_crimetype_id,
    location
  ) %>%
  verify_crime_dataset(type = "incident_level",
                       id_warning = "There is one id that has a duplicate.")

remove_ls(pattern = "memphis_")



#### ------------------------- MILWAUKEE ------------------------------------####
info_line("Milwaukee")

crimes_dataset_list[['milwaukee']] <- s3read_using(
  FUN = read_csv3,
  object = "milwaukee/2020/wibr.csv",
  bucket = "aclu-covid-crimes"
) %>%
  mutate(IncidentNum = as.character(IncidentNum)) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "milwaukee/2019/wibrarchive.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(reported_date_time),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    location = "Milwaukee, WI"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(
    id = incident_num,
    occur_date,
    year,
    month,
    location,
    arson:vehicle_theft # Keeping these columns to keep the dataset at the incident level
  ) %>%
  # Only 1 missing ID
  filter(!is.na(id)) %>%
  verify_crime_dataset(type = "incident_level",
                       id_warning = "There are four ids duplicated in dataset.")

remove_ls(pattern = "milwaukee_")


#### ------------------------- NASHVILLE ------------------------------------####
info_line("Nashville")
crimes_dataset_list[['nashville']] <- s3read_using(
  FUN = read_csv3,
  object = "nashville/Metro_Nashville_Police_Department_Incidents__2020_.csv",
  bucket = "aclu-covid-crimes"
) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "nashville/Metro_Nashville_Police_Department_Incidents__2019_.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "nashville/Metro_Nashville_Police_Department_Incidents__2018_.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(incident_occurred, "%m/%d/%Y"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(incident_number),
    location = "Nashville, TN"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = offense_description,
    category_num = offense_nibrs,
    location
  ) %>%
  verify_crime_dataset(
    type = "offense_level"
  )

remove_ls(pattern = "nashville_")

#### ------------------------- OMAHA ------------------------------------####
info_line("Omaha")
crimes_dataset_list[['omaha']] <- s3read_using(
  FUN = read_csv3,
  object = "omaha/Incidents_2018.csv",
  bucket = "aclu-covid-crimes"
) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "omaha/Incidents_2019.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "omaha/Incidents_2020.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(reported_date, "%m/%d/%Y"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(rb_number),
    location = "Omaha, NE"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = statute_ordinance_description,
    location
  ) %>%
  verify_crime_dataset(
    type = "offense_level"
  )

remove_ls(pattern = "omaha_")

#### ------------------------- PHILADELPHIA ------------------------------------####
info_line("Philadelphia")
crimes_dataset_list[['philadelphia']] <- s3read_using(
  FUN = read_csv3,
  object = "philadelphia/2019/incidents_part1_part2_2019.csv",
  bucket = "aclu-covid-crimes"
) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "philadelphia/2018/incidents_part1_part2.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "philadelphia/2020/incidents_part1_part2.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  clean_names() %>%
  mutate(
    month = floor_date(dispatch_date, "month"),
    year = floor_date(dispatch_date, "year"),
    id = as.character(objectid),
    category_num = as.character(ucr_general),
    location = "Philadelphia, PA"
  ) %>%
  select(id,
    occur_date = dispatch_date,
    year,
    month,
    category = text_general_code,
    category_num,
    location
  ) %>%
  verify_crime_dataset(type = "incident")

remove_ls(pattern = "philadelphia_")

#### ------------------------- PHOENIX ------------------------------------####
info_line("Phoenix")
crimes_dataset_list[['phoenix']] <- s3read_using(
  FUN = read_csv3,
  object = "phoenix/crimestat.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(occurred_on, "%m/%d/%Y"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(inc_number),
    location = "Phoenix, AZ"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = ucr_crime_category,
    location
  ) %>%
  verify_crime_dataset(type = "incident")

remove_ls(pattern = "phoenix_")

#### ------------------------- PINELLAS ------------------------------------####
# Only able to download 5k at a time, hence many files

info_line("Pinellas")

pinellas_objects <- data.table::rbindlist(
  get_bucket("aclu-covid-crimes", prefix = "pinellas")) %>%
  filter(!Key %in% c("pinellas/.DS_Store")) %>%
  pull(Key)

crimes_dataset_list[['pinellas']] <- map_dfr(
  pinellas_objects,
  ~ s3read_using(FUN = read_csv3, object = .x, bucket = "aclu-covid-crimes")
) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(report_date, "%m/%d/%Y"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(report_key),
    location = "Pinellas County, FL"
  ) %>%
  # there are some 
  select(id,
    occur_date,
    year,
    month,
    category,
    description = report_code,
    location
  ) %>%
  # there are some seemingly straight duplicates 
  distinct() %>% 
  verify_crime_dataset(type = "offense_level")



remove_ls(pattern = "pinellas_")

#### ------------------------- PITTSBURGH  ------------------------------------####
info_line("Pittsburgh")

crimes_dataset_list[['pittsburgh']] <- s3read_using(
  FUN = read_csv3,
  object = "pittsburgh/044f2016-1dfd-4ab0-bc1e-065da05fca2e.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(incidenttime),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(ccr),
    category_num = as.character(hierarchy),
    location = "Pittsburgh, PA"
  ) %>%
  select(id,
    occur_date,
    year,
    month,
    category = incidenthierarchydesc,
    category_num,
    description = offenses,
    location
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  # there were 30 dupes, and they looked like duplicate rows
  unique() %>%
  verify_crime_dataset(
    type = "incident",
    id_warning = "There were 30 duplicate ids that looked like fully duplicated rows.
    A few of these were not duplicated rows."
  )

remove_ls(pattern = "pittsburgh_")

#### ------------------------- PORTLAND  ------------------------------------####
info_line("Portland")

crimes_dataset_list[['portland']] <- s3read_using(
  FUN = read_csv3,
  object = "portland/CrimeData-2018.csv",
  bucket = "aclu-covid-crimes"
) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "portland/CrimeData-2019.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "portland/CrimeData-2020.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(occur_date, "%m/%d/%Y"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(case_number),
    location = "Portland, OR"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = offense_category,
    description = offense_type,
    location,
    offense_count
  ) %>%
  verify_crime_dataset(id_warning = "Addresses seem to parse incorrectly",
                       type = "offense_level")

remove_ls(pattern = "portland_")

#### ------------------------- RALEIGH  ------------------------------------####
info_line("Raleigh")

crimes_dataset_list[['raleigh']] <- s3read_using(
  FUN = read_csv3,
  object = "raleigh/Raleigh_Police_Incidents__NIBRS_.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(reported_date, "%Y/%m/%d"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(case_number),
    location = "Raleigh, NC"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  # There are many missing IDs, so we estimate incidents by the duplication of these four cols.
  mutate(id = if_else(is.na(id), paste0(reported_block_address, reported_date, latitude, longitude), id)) %>% 
  select(id,
    occur_date,
    year,
    month,
    category = crime_category,
    category_num = crime_code,
    description = crime_description,
    location
  ) %>%
  verify_crime_dataset(type = "offense_level")


remove_ls(pattern = "raleigh_")

#### ------------------------- RIVERSIDE  ------------------------------------####
info_line("Riverside")

crimes_dataset_list[['riverside']] <- s3read_using(
  FUN = read_csv3,
  object = "riverside/Crime_Reports.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    month = floor_date(offense_date, "month"),
    year = floor_date(offense_date, "year"),
    id = as.character(case_number),
    location = "Riverside, CA"
  ) %>%
  # The column previously titled "id" seemed to be unique per row. "Case number" is at the 
  # incident level with multiple rows of offenses. 
  select(id,
    occur_date = offense_date,
    year,
    month,
    category = crime_type,
    location
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  distinct() %>%
  verify_crime_dataset(type = "offense_level")

remove_ls(pattern = "riverside_")

#### ------------------------- SAN BERNARDINO ------------------------------------####
info_line("San Bernardino")

san_bernardino_location <- "http://www.ci.san-bernardino.ca.us/civicax/filebank/blobdload.aspx?BlobID=28475"
san_bernardino_out <- extract_tables(san_bernardino_location)

san_bernardino_location2 <- "http://www.ci.san-bernardino.ca.us/civicax/filebank/blobdload.aspx?BlobID=28041"

san_bernardino_location3 <- "http://www.ci.san-bernardino.ca.us/civicax/filebank/blobdload.aspx?BlobID=26799"

san_bernardino_2020 <- as.data.frame(san_bernardino_out[[1]]) %>%
  rename(
    crime = V2, `January, 2020` = V3, `February, 2020` = V4, `March, 2020` = V5,
    `April, 2020` = V6, `May, 2020` = V7
  ) %>%
  select(`January, 2020`:`May, 2020`) %>%
  head(1) %>%
  gather(month_yr, n) %>%
  mutate(n = as.numeric(n))

san_bernardino_2019 <- data_frame(
  month_yr = c(
    "January, 2019", "February, 2019", "March, 2019", "April, 2019", "May, 2019",
    "June, 2019", "July, 2019", "August, 2019", "September, 2019", "October, 2019",
    "November, 2019", "December, 2019"
  ),
  # These are the totals of Part One crime from the link "location2" above. The pdf reading was messy on this one. 
  n = c(1019, 877, 1030, 946, 928, 970, 999, 1010, 967, 1195, 973, 1041)
)

# This is from the link "location 3" and is also just the total Part One crimes per month
san_bernardino_2018 <- data_frame(
  month_yr = c(
    "January, 2018", "February, 2018", "March, 2018", "April, 2018", "May, 2018",
    "June, 2018", "July, 2018", "August, 2018", "September, 2018", "October, 2018",
    "November, 2018", "December, 2018"
  ),
  n = c(1070, 876, 972, 943, 1010, 997, 1032, 1017, 969, 1125, 960, 949)
)

crimes_dataset_list[['san_bernardino']] <- san_bernardino_2019 %>%
  bind_rows(san_bernardino_2020) %>%
  bind_rows(san_bernardino_2018) %>%
  mutate(
    location = "San Bernardino, CA",
    month = parse_date(month_yr, "%B, %Y")
  ) %>%
  verify_crime_dataset(type = "month_level")

remove_ls(pattern = "san_bernardino_")

#### ------------------------- SAN FRANCISCO -------------------------------------####
info_line("San Francisco")

crimes_dataset_list[['san_francisco']] <- s3read_using(
  FUN = read_csv3,
  object = "sanfrancisco/Police_Department_Incident_Reports__2018_to_Present.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  mutate(
    occur_date = incident_date,
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    # According to the dictionary, this is sometimes interchangeable with case number
    id = as.character(incident_number),
    location = "San Francisco, CA"
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  select(id,
    occur_date,
    year,
    month,
    category = incident_subcategory,
    category_num = incident_code,
    description = incident_description,
    location,
    incident_category
  ) %>%
  verify_crime_dataset(type = "offense_level")

remove_ls(pattern = "san_francisco_")



#### ------------------------- SAN DIEGO ------------------------------------####
info_line("San Diego")

san_diego_out <- s3read_using(
  FUN = read_csv3,
  object = "sandiego/crimes.csv",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  select(-sort_order) %>%
  gather(month_char, n, -crime)

crimes_dataset_list[['san_diego']] <- san_diego_out %>%
  filter(crime == "Crime Index" & month_char != "total") %>%
  mutate(
    month_char = str_replace(month_char, "_", " "),
    month = parse_date(month_char, "%b %y"),
    location = "San Diego County, CA"
  ) %>%
  verify_crime_dataset(type = "month_level")

remove_ls(pattern = "san_diego_")

#### ------------------------- SEATTLE ------------------------------------####
info_line("Seattle")

crimes_dataset_list[['seattle']] <- s3read_using(
  FUN = read_excel,
  object = "seattle/Line_crosstab.xls",
  bucket = "aclu-covid-crimes"
) %>%
  clean_names() %>%
  select(-incomplete_month) %>%
  gather(month, n, -year_of_offense_start_date_time) %>%
  mutate(month_yr = paste(month, year_of_offense_start_date_time)) %>%
  mutate(
    month = parse_date(month_yr, "%b %Y"),
    location = "Seattle, WA"
  ) %>%
  verify_crime_dataset(type = "month_level")

remove_ls(pattern = "seattle_")

#### ------------------------- TUCSON ------------------------------------####
info_line("Tucson")

crimes_dataset_list[['tucson']] <- s3read_using(
  FUN = read_csv3,
  object = "tuscon/Tucson_Police_Incidents_-_2018_-_Open_Data.csv",
  bucket = "aclu-covid-crimes"
) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "tuscon/Tucson_Police_Incidents_-_2019_-_Open_Data (1).csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  select(-DOW_REPT) %>%
  bind_rows(s3read_using(
    FUN = read_csv3,
    object = "tuscon/Tucson_Police_Incidents_-_2020_-_Open_Data.csv",
    bucket = "aclu-covid-crimes"
  )) %>%
  clean_names() %>%
  mutate(
    occur_date = as.Date(date_occu, "%Y/%m/%d"),
    month = floor_date(occur_date, "month"),
    year = floor_date(occur_date, "year"),
    id = as.character(inci_id),
    location = "Tucson, AZ"
  ) %>%
  select(id,
    occur_date,
    year,
    month,
    category = statutdesc,
    category_num = offense,
    description = reportedas,
    location
  ) %>%
  filter(occur_date >= as.Date("2018-01-01")) %>%
  verify_crime_dataset(
    type = "incident_level",
    id_warning = "Limited duplicates (97) ; we can use unique IDs as an incident indicator"
  )

remove_ls(pattern = "tucson_")
