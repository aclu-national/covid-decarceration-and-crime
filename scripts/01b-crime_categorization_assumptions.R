fs::dir_walk(here::here("code"), source)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### CATEGORIZATION OF PART 1 CRIMES ###

## This script attempts to gather just Part 1 UCR crimes 
## from datasets that don't explicitly exclude other types.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# This reads in the crime data. No other datasets required for this script.
source(here::here("scripts/01a-crime_data.R"))

part1_dataset_list <- list()
monthly_dataset_list <- list()

#### ------------------------- ATLANTA ------------------------------------####

# All are Part One, but does not report rape
crimes_dataset_list[['atlanta']] %>%
  tabyl(category)

part1_dataset_list[['atlanta']] <- crimes_dataset_list[['atlanta']]

remove_ls(pattern = "atlanta_")


#### ------------------------- AUSTIN ------------------------------------####
part1_dataset_list[['austin']] <- crimes_dataset_list[['austin']] %>%
  # Most without a category is not Part One, and everything with a category is Part One
  # Exception: some offenses of arson do not have a category. 
  filter(!is.na(category) | description == "ARSON")

part1_dataset_list[['austin']] %>%
  tabyl(category)

part1_dataset_list[['austin']] %>%
  tabyl(description)

# Checking rows without a category
austin_tst <- crimes_dataset_list[['austin']] %>%
  filter(is.na(category)) %>%
  mutate(maybes = str_detect(description, paste(part1, collapse = "|"))) %>%
  filter(maybes == TRUE)

austin_tst %>%
  tabyl(description)

remove_ls(pattern = "austin_")


#### ------------------------- BOSTON ------------------------------------####

part1_dataset_list[['boston']] <- crimes_dataset_list[['boston']] %>%
  mutate(description = trimws(description)) %>%
  # These completely drop off later in the year. We estimate Part One categories where there is no category below. 
  filter(category == "Part One")

# Many categories missing
crimes_dataset_list[['boston']] %>%
  tabyl(category)

# Estimating Part One categories based on description 
boston_more_part_ones <- crimes_dataset_list[['boston']] %>%
  filter(is.na(category)) %>%
  # These have same description as ones with the category Part One
  mutate(category = if_else(description %in% part1_dataset_list[['boston']]$description, "Part One", category)) %>%
  filter(category == "Part One")

boston_even_more_part_ones <- crimes_dataset_list[['boston']] %>%
  filter(is.na(category)) %>%
  filter(!id %in% boston_more_part_ones$id) %>%
  mutate(category_num = str_pad(category_num, 5, "0", side = "left")) %>%
  mutate(category = case_when(
    category_num %in% part1_dataset_list[['boston']]$category_num ~ "Part One"
  )) %>%
  mutate(word_detection = str_detect(description, paste(part1, collapse = "|"))) %>%
  mutate(category = ifelse(word_detection == TRUE, "Part One", category)) %>%
  filter(category == "Part One")


part1_dataset_list[['boston']] <- part1_dataset_list[['boston']] %>%
  bind_rows(boston_more_part_ones) %>%
  bind_rows(boston_even_more_part_ones)


boston_more_part_ones %>% tabyl(description)


remove_ls(pattern = "boston_")

#### ------------------------- CHICAGO ------------------------------------####

crimes_dataset_list[['chicago']] %>%
  tabyl(category)

part1_dataset_list[['chicago']] <- crimes_dataset_list[['chicago']] %>%
  mutate(
    word_detection = str_detect(category, paste(part1, collapse = "|")),
    # Looking for words that are captured in the "description" and not in the "category"
    word_detection2 = str_detect(description, paste(part1, collapse = "|"))
  )

part1_dataset_list[['chicago']] %>%
  filter(word_detection| word_detection2) %>%
  tabyl(description)

# Aggravated assaults should be included and are not
part1_dataset_list[['chicago']] %>%
  filter(!word_detection) %>%
  tabyl(description)

part1_dataset_list[['chicago']] <- part1_dataset_list[['chicago']] %>%
  filter(word_detection| word_detection2) %>%
  # These are not Part One
  filter(!category %in% c("DECEPTIVE PRACTICE", "STALKING", "PUBLIC PEACE VIOLATION", "KIDNAPPING", "OTHER OFFENSE", "OFFENSE INVOLVING CHILDREN"))
  # Noting that "sex offense" is potentially over-inclusive but all are categorized as "agg" sexual offenses in description. 

tabyl(part1_dataset_list[['chicago']]$category)
remove_ls(pattern = "chicago_")

#### ------------------------- CINCINATTI ------------------------------------####

crimes_dataset_list[['cincinnati']] %>%
  tabyl(category)

crimes_dataset_list[['cincinnati']] %>% 
  tabyl(description)

part1_dataset_list[['cincinnati']] <- crimes_dataset_list[['cincinnati']] %>%
  filter(!category %in% c("PART 2 MINOR", "UNAUTHORIZED USE") &
        !is.na(category) &
        !(description %in% 
          c("ETHNIC INTIMIDATION", 
            "NEGLIGENT ASSAULT", 
            "TAMPERING WITH COIN MACHINES", 
            "UNAUTHORIZED USE OF PROPERTY"))
        )

remove_ls(pattern = "cincinnati_")


#### ------------------------- DC ------------------------------------####
# Assumption: DC only includes Part One crimes.
# Note: In the documentation, DC says: "Sex Assault Data Availability: In an effort to provide more clear
# information about the most serious sex assaults that are most closely aligned with the public's perception
# of rape and attempted rape, the most serious sex abuse categories are included in the reports of DC Code
# Index Violent Crimes: Sex Assault. The figures reported in this category include First Degree Sex Abuse, Second Degree 
# Sex Abuse, Attempted First Degree Sex Abuse and Assault with Intent to Commit First Degree
# Sex Abuse against adults."

crimes_dataset_list[['dc']] %>%
  tabyl(category)

part1_dataset_list[['dc']] <- crimes_dataset_list[['dc']]

remove_ls(pattern = "dc_")

#### ------------------------- DENVER ------------------------------------####

part1_dataset_list[['denver']] <- crimes_dataset_list[['denver']] %>%
  mutate(
    category = toupper(category),
    description = toupper(description)
  ) %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

# checking
part1_dataset_list[['denver']] %>%
  filter(word_detection) %>%
  tabyl(description, category)

part1_dataset_list[['denver']] %>%
  filter(!word_detection) %>%
  tabyl(description)

# Add in rape and aggravated assaults
part1_dataset_list[['denver']] <- part1_dataset_list[['denver']] %>%
  filter(word_detection | description %in% c(
    "SEX-ASLT-RAPE", "SEX-ASLT-RAPE-POT", "AGG-ASLT-POLICE-WEAPON",
    "ASLT-AGG-POLICE-GUN"
  ))

part1_dataset_list[['denver']] %>%
  tabyl(category)

part1_dataset_list[['denver']] %>%
  quick_line()

remove_ls(pattern = "denver_")

#### ------------------------- DETROIT ------------------------------------####

part1_dataset_list[['detroit']] <- crimes_dataset_list[['detroit']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|"))) %>%
  # CSC stands for criminal sexual contact, and 1st degree counts as Part One:  https://www.nysenate.gov/legislation/laws/PEN/130.65
  mutate(
    rape = str_detect(description, "CSC 1ST DEGREE"),
    second_degree = str_detect(description, "2ND DEGREE|3RD DEGREE|4TH DEGREE")
  )

# Some CSC 1st degree descriptions are matched with (seemingly) wrong categories. PROPERTY/OBSTRUCTING THE POLICE comes up as a CSC 1st degree, 
# for instnace. We leave these in and trust the CSC description. 

# # detroit_part1 %>%
# #   filter(str_detect(category, "OBSTRU")) %>%
# #   tabyl(description)


part1_dataset_list[['detroit']] %>%
  filter(word_detection) %>%
  tabyl(category, description)


part1_dataset_list[['detroit']] %>%
  filter(word_detection) %>%
  tabyl(description)

part1_dataset_list[['detroit']] <- part1_dataset_list[['detroit']] %>%
  filter((word_detection| rape) & category != "JUSTIFIABLE HOMICIDE" &
    second_degree == FALSE &
    !description %in% c(
      "KIDNAPPING / ABDUCTION", "INTIMIDATION / STALKING", "POSSESSION OF BURGLARY TOOLS", "PARENTAL KIDNAPPING",
      "FRAUD - FALSE PRETENSE / SWINDLE / CONFIDENCE GAME", "EXTORTION", "ENTRY WITHOUT PERMISSION (NO INTENT)"
    ))


tabyl(part1_dataset_list[['detroit']]$description)

remove_ls(pattern = "detroit_")

#### ------------------------- FT LAUDERDALE ------------------------------------####

crimes_dataset_list[['ft_lauderdale']] %>% tabyl(category)

part1_dataset_list[['ft_lauderdale']] <- crimes_dataset_list[['ft_lauderdale']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

# These all seem like Part One crimes but am not sure about the ones beginning with "warrant". There are very few of these.
part1_dataset_list[['ft_lauderdale']] %>%
  filter(word_detection) %>%
  tabyl(category)

# Leaves in all of Ft Lauderdale
part1_dataset_list[['ft_lauderdale']] <- part1_dataset_list[['ft_lauderdale']] %>%
  filter(word_detection == TRUE)

remove_ls(pattern = "ft_lauderdale_")

#### ------------------------- FORT WORTH ------------------------------------####

crimes_dataset_list[['fort_worth']] %>%
  tabyl(category)

part1_dataset_list[['fort_worth']] <- crimes_dataset_list[['fort_worth']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['fort_worth']] %>%
  filter(word_detection) %>%
  tabyl(category)

part1_dataset_list[['fort_worth']] %>%
  filter(!word_detection) %>%
  tabyl(category)

part1_dataset_list[['fort_worth']] <- part1_dataset_list[['fort_worth']] %>%
  filter(word_detection | category == "AG ASSAULT") %>%
  # Filtering Ft Worth for just data after 2019 -- anything before this looks untrustworthy. 
  filter(occur_date >= as.Date("2019-01-01"))

remove_ls(pattern = "fort_worth_")
#### ------------------------- HOUSTON ------------------------------------####

part1_dataset_list[['houston']] <- crimes_dataset_list[['houston']] %>%
  mutate(category = toupper(category)) %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['houston']] %>%
  filter(word_detection) %>%
  tabyl(category)

part1_dataset_list[['houston']] %>%
  filter(!word_detection) %>%
  tabyl(category)

part1_dataset_list[['houston']] <- part1_dataset_list[['houston']] %>%
  filter(word_detection == TRUE | category %in% c(
    "PURSE-SNATCHING", "POCKET PICKING" )) %>%
  # These categories aren't counted as Part One
  filter(!category %in% c("IDENTIFY THEFT", "STATUTORY RAPE"))

tabyl(part1_dataset_list[['houston']]$category)

remove_ls(pattern = "houston_")
#### ------------------------- LOS ANGELES ------------------------------------####

crimes_dataset_list[['los_angeles']] %>%
  tabyl(category)

part1_dataset_list[['los_angeles']] <- crimes_dataset_list[['los_angeles']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['los_angeles']] %>%
  filter(word_detection) %>%
  tabyl(category)

part1_dataset_list[['los_angeles']] %>%
  filter(!word_detection) %>%
  tabyl(category)

part1_dataset_list[['los_angeles']] <- part1_dataset_list[['los_angeles']] %>%
  mutate(not_part_one = str_detect(category, "EMBEZZLEMENT|THEFT OF IDENTITY")) %>%
  # Including "VEHICLE - STOLEN" as auto theft and thus a Part One offense. 
  filter((word_detection == TRUE | category %in%
    c(
      "VEHICLE - STOLEN", "VEHICLE - ATTEMPT STOLEN",
      "PURSE SNATCHING", "PURSE SNATCHING - ATTEMPT",
      "LYNCHING",
      "LYNCHING - ATTEMPT",
      "SHOTS FIRED AT MOVING VEHICLE, TRAIN OR AIRCRAFT",
      "SHOTS FIRED AT INHABITED DWELLING"
    )) & !not_part_one)

part1_dataset_list[['los_angeles']] %>%
  tabyl(category)

remove_ls(pattern = "los_angeles_")
#### ------------------------- LOUISVILLE ------------------------------------####

monthly_dataset_list[['louisville']] <- crimes_dataset_list[['louisville']]

#### ------------------------- MEMPHIS ------------------------------------#### 

part1_dataset_list[['memphis']] <- crimes_dataset_list[['memphis']] %>%
  mutate(category = toupper(category)) %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['memphis']] %>%
  filter(word_detection) %>%
  tabyl(description)

part1_dataset_list[['memphis']] %>%
  filter(!word_detection) %>%
  tabyl(description)

part1_dataset_list[['memphis']] <- part1_dataset_list[['memphis']] %>%
  filter((word_detection == TRUE | category %in% c("PROPERTY CRIME", "BREAKING & ENTERING") |
    description %in% c("Aggravated Assault", "Aggravated Assault/DV")) &
    !description %in% c(
      "Embezzlement", "Identity Theft",
      # According to the handbook, negligent vehicular manslaughter is not classified as neg. homicide
      "Negligent Vehicular Manslaughter", "Stolen Property",
      "Stolen Property Offense", "Vandalism/Misdemeanor",
      "Vandalism/Felony"
    ))

part1_dataset_list[['memphis']] %>%
  tabyl(description)

part1_dataset_list[['memphis']] %>%
  quick_line()

remove_ls(pattern = "memphis_")

#### ------------------------- MILWAUKEE ------------------------------------####

# Including "locked vehicle" as auto theft/attempted auto theft -- Part One. 

part1_dataset_list[['milwaukee']] <- crimes_dataset_list[['milwaukee']]

remove_ls(pattern = "milwaukee_")

#### ------------------------- NASHVILLE ------------------------------------####

crimes_dataset_list[['nashville']] %>%
  tabyl(category)

part1_dataset_list[['nashville']] <- crimes_dataset_list[['nashville']] %>%
  mutate(
    category = toupper(category),
    word_detection = str_detect(category, paste(part1, collapse = "|"))
  )

# Stalking should not be part 1, nor should photography, animal cruelty, identity theft, criminal trepass, child
part1_dataset_list[['nashville']] %>%
  filter(!word_detection) %>%
  tabyl(category)

part1_dataset_list[['nashville']] <- part1_dataset_list[['nashville']] %>%
  mutate(not_part1 = str_detect(category, "PHOTOGRAPHY|ANIMAL|CHILD|IDENTITY|TRESPASS|PANHANDLING|CAREGIVER. AGG. NEGLECT")) %>%
  # Larceny (shortened to LARC) shoud be included. 
  mutate(larceny = str_detect(category, "LARC")) %>%
  filter((word_detection | larceny) & !not_part1)

tabyl(part1_dataset_list[['nashville']]$category)
remove_ls(pattern = "nashville_")


#### ------------------------- OMAHA ------------------------------------####

crimes_dataset_list[['omaha']] %>%
  tabyl(category)

part1_dataset_list[['omaha']] <- crimes_dataset_list[['omaha']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['omaha']] %>%
  filter(word_detection) %>%
  tabyl(category)

# Categorizing SEXUAL ASSAULT-PENETRATION as rape. Identity theft and extortion are not Part One. 
part1_dataset_list[['omaha']] %>%
  filter(!word_detection) %>%
  tabyl(category)

part1_dataset_list[['omaha']] <- part1_dataset_list[['omaha']] %>%
  mutate(exclude = str_detect(category, "IDENTITY THEFT|EXTORTION")) %>%
  filter((word_detection & !exclude) | category %in% c("SEXUAL ASSAULT-ATT PENETRATION", "SEXUAL ASSAULT-PENETRATION"))

tabyl(part1_dataset_list[['omaha']] $category)
remove_ls(pattern = "omaha_")

#### ------------------------- PHILADELPHIA ------------------------------------####

part1_dataset_list[['philadelphia']] <- crimes_dataset_list[['philadelphia']] %>%
  mutate(category = toupper(category)) %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['philadelphia']] %>%
  filter(word_detection) %>%
  tabyl(category)

part1_dataset_list[['philadelphia']] %>%
  filter(!word_detection) %>%
  tabyl(category)

part1_dataset_list[['philadelphia']] <- part1_dataset_list[['philadelphia']] %>%
  filter(word_detection == TRUE)

remove_ls(pattern = "philadelphia_")


#### ------------------------- PHOENIX ------------------------------------####

crimes_dataset_list[['phoenix']] %>%
  tabyl(category)

part1_dataset_list[['phoenix']] <- crimes_dataset_list[['phoenix']] %>%
  filter(category != "DRUG OFFENSE")

remove_ls(pattern = "phoenix_")

#### ------------------------- PINELLAS ------------------------------------####
crimes_dataset_list[['pinellas']] %>%
  tabyl(category)

part1_dataset_list[['pinellas']] <- crimes_dataset_list[['pinellas']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['pinellas']] %>%
  filter(word_detection) %>%
  tabyl(category)

part1_dataset_list[['pinellas']] %>%
  filter(!word_detection) %>%
  tabyl(category)

# On the Pinellas site, "stolen vehicle" means "auto theft" which is Part One. 
part1_dataset_list[['pinellas']] <- part1_dataset_list[['pinellas']] %>%
  filter(word_detection == TRUE | category %in% c("STOLEN VEHICLE"))

remove_ls(pattern = "pinellas_")

#### ------------------------- PITTSBURGH  ------------------------------------####

crimes_dataset_list[['pittsburgh']] %>%
  tabyl(category)

part1_dataset_list[['pittsburgh']] <- crimes_dataset_list[['pittsburgh']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['pittsburgh']] %>%
  filter(!word_detection) %>%
  tabyl(description) %>%
  View()

part1_dataset_list[['pittsburgh']]  <- part1_dataset_list[['pittsburgh']] %>%
  mutate(
    mtr_theft = str_detect(category, "MTR"),
    rape = str_detect(description, "Rape")
  ) %>%
  filter((word_detection | mtr_theft | rape) & !category %in% c("IDENTITY THEFT"))

part1_dataset_list[['pittsburgh']] %>%
  tabyl(category)

remove_ls(pattern = "pittsburgh_")

#### ------------------------- PORTLAND  ------------------------------------####

crimes_dataset_list[['portland']] %>%
  tabyl(category)

part1_dataset_list[['portland']] <- crimes_dataset_list[['portland']] %>%
  mutate(
    category = toupper(category),
    description = toupper(description)
  ) %>%
  mutate(
    word_detection = str_detect(category, paste(part1, collapse = "|")),
    # More 
    word_detection2 = str_detect(description, paste(part1, collapse = "|"))
  )

part1_dataset_list[['portland']] %>%
  filter(word_detection | word_detection2) %>%
  tabyl(description)

part1_dataset_list[['portland']] %>%
  filter(!word_detection & !word_detection2) %>%
  tabyl(description)

part1_dataset_list[['portland']] <- part1_dataset_list[['portland']] %>%
  filter(word_detection | word_detection2 ) %>%
  filter(!description %in% c("STATUTORY RAPE", "IDENTITY THEFT"))

part1_dataset_list[['portland']] %>%
  tabyl(category)

remove_ls(pattern = "portland_")

#### ------------------------- RALEIGH  ------------------------------------####
crimes_dataset_list[['raleigh']] %>%
  tabyl(category)

part1_dataset_list[['raleigh']] <- crimes_dataset_list[['raleigh']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

part1_dataset_list[['raleigh']] %>%
  filter(word_detection) %>%
  tabyl(category)

# From the UCR manual: "13. Stolen Property: Buying, Receiving, Possessing " is a Part Two offence
part1_dataset_list[['raleigh']] %>%
  filter(!word_detection) %>%
  tabyl(description)

part1_dataset_list[['raleigh']] <- part1_dataset_list[['raleigh']] %>%
  filter(word_detection | description %in% c("Assault/Aggravated", "Sex Offense/Forcible Rape", "Child Abuse/Aggravated"))

part1_dataset_list[['raleigh']] %>%
  tabyl(description)

part1_dataset_list[['raleigh']]%>%
  quick_line()

remove_ls(pattern = "raleigh_")

#### ------------------------- RIVERSIDE  ------------------------------------####

crimes_dataset_list[['riverside']] %>%
  tabyl(category)

part1_dataset_list[['riverside']] <- crimes_dataset_list[['riverside']] %>%
  mutate(
    category = toupper(category),
    word_detection = str_detect(category, paste(part1, collapse = "|"))
  )

part1_dataset_list[['riverside']] %>%
  filter(word_detection) %>%
  tabyl(category)


part1_dataset_list[['riverside']] %>%
  filter(!word_detection) %>%
  tabyl(category)

part1_dataset_list[['riverside']] <- part1_dataset_list[['riverside']] %>%
  # Counting the below as aggravated assault. 
  filter(word_detection == TRUE | category %in% c(
    "ASSAULT: FIREARM", "ASSAULT: KNIFE OR CUTTING INSTRUMENT",
    "ASSAULT: OTHER DANGEROUS WEAPON", "ASSAULT: STRONG-ARM"
  ))

remove_ls(pattern = "riverside_")

#### ------------------------- SAN BERNARDINO ------------------------------------####

monthly_dataset_list[['san_bernardino']] <- crimes_dataset_list[['san_bernardino']]

#### ------------------------- SAN FRANCISCO -------------------------------------####
crimes_dataset_list[['san_francisco']] %>%
  tabyl(category)

part1_dataset_list[['san_francisco']] <- crimes_dataset_list[['san_francisco']] %>%
  mutate(
    category = toupper(category),
    word_detection = str_detect(category, paste(part1, collapse = "|"))
  )

part1_dataset_list[['san_francisco']] %>%
  filter(word_detection) %>%
  tabyl(category)

part1_dataset_list[['san_francisco']] %>%
  filter(!word_detection) %>%
  tabyl(description)

part1_dataset_list[['san_francisco']]  <- part1_dataset_list[['san_francisco']] %>%
  filter(word_detection)

remove_ls(pattern = "san_francisco_")


#### ------------------------- SAN DIEGO ------------------------------------####

monthly_dataset_list[['san_diego']] <- crimes_dataset_list[['san_diego']]

#### ------------------------- SEATTLE ------------------------------------####

monthly_dataset_list[['seattle']] <- crimes_dataset_list[['seattle']]

#### ------------------------- TUCSON ------------------------------------####

crimes_dataset_list[['tucson']] %>%
  tabyl(category)

part1_dataset_list[['tucson']] <- crimes_dataset_list[['tucson']] %>%
  mutate(word_detection = str_detect(category, paste(part1, collapse = "|")))

# Descriptions are very messy. Utilize mainly "category" instead.
part1_dataset_list[['tucson']] %>%
  filter(word_detection) %>%
  tabyl(category) 

part1_dataset_list[['tucson']] %>%
  filter(word_detection) %>%
  tabyl(description)

part1_dataset_list[['tucson']] <- part1_dataset_list[['tucson']] %>%
  mutate(not_arson = str_detect(category, "NOT ARSON")) %>%
  # Rape is covered by other categories, so excluding the "sex/offenses/other" category. 
  filter(!not_arson & word_detection & !category %in% c("SEX OFFENSES/OTHER (ADULTRY,INCEST,STAT RAPE,ETC)"))

remove_ls(pattern = "tucson_")
