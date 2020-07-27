suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(here)
  library(janitor)
  library(magrittr)
  library(lubridate)
  library(readxl)
  library(tabulizer)
  library(LibLabTemplates)
  library(aws.s3)
  library(glue)
  library(ggplot2) # requires version >= 3.3 for custom theme to work
  library(extrafont)
  library(tidycensus) # requires version >= 0.9.9.5 ; errors from older versions
  library(fs)
  library(assertr)
  library(scales)
  library(conflicted)
  library(data.table) # requires version >= 1.12.4; errors from older versions
})

# Conflict preferences
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("month", "lubridate", quiet = TRUE)
conflict_prefer("first", "dplyr", quiet = TRUE)
conflict_prefer("last", "dplyr", quiet = TRUE)
conflict_prefer("year", "lubridate", quiet = TRUE)

# setup of tidycensus and other config parameters
my_config <- LibLabTemplates::get_my_config_file()


Sys.setenv(
  "AWS_ACCESS_KEY_ID" = my_config$aws_access$AWS_ACCESS_KEY_ID,
  "AWS_SECRET_ACCESS_KEY" = my_config$aws_access$AWS_SECRET_ACCESS_KEY,
  "AWS_DEFAULT_REGION" = "us-east-1",
  "CENSUS_API_KEY" = my_config$census$CENSUS_API_KEY
)
