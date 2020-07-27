# Crimes During COVID-19

COVID-19 presents an enormous risk to those in carceral facilities and their surrounding communities. The ACLU Analytics team demonstrated just how dangerous the COVID-19 pandemic could be for incarcerated people in April after building an [epidemiological model](https://www.aclu.org/news/smart-justice/new-model-shows-reducing-jail-population-will-lower-covid-19-death-toll-for-all-of-us/) that illustrated the deadly threat of COVID-19 in jails. Since then, some governors, sheriffs, and judges made the decision to shift detention policies to prioritize protecting the lives of those who live and work in jails and prisons.

The release of incarcerated people from jails has saved lives in both jails and in communities. To quell concerns that decarceration has had any negative effect on the surrounding communities, the ACLU Analytics team analyzed crime trends and jail populations in 29 localities. Nearly every county jail examined reduced their population during the COVID-19 pandemic, and the size of the reduction in jail population was functionally unrelated to crime trends in the following months (March - May 2020). The team found no evidence of spikes in crime in areas that reduced their jail population, even when comparing monthly trends over the past two years. The release of incarcerated people from jails has saved lives, all while monthly crime trends remained within or below average ranges in every city. 

### Data Sources 

See [here](https://docs.google.com/spreadsheets/d/1zEAuJ4HB7f4SxodJF33Ja5qVnpquSXpAC0gpvwblxvs/edit?usp=sharing) for sources of crime and jail population data. This analysis was conducted in July of 2020 — because police data is dynamic and sometimes subsequently updated based on new reports, incident counts may have changed since the time of analysis. 

### Repository Structure 

The repository is structured into several folders as follows:

- **:open_file_folder: scripts** : Core scripts, run in sequential order, used as part of the cleaning and analysis process
  - :page_with_curl: `01a-crime-data.R` : Reads in 29 cities worth of crime data from AWS S3 bucket. Cleans all of them such that they have the relevant required columns depending on type (incident level, offense level or month level).
  - :page_with_curl: `01b-crime_categorization_assumptions.R`: Categorizes all crime cities data such that we only look at [UCR's "Part One"](https://ucr.fbi.gov/crime-in-the-u.s/2011/crime-in-the-u.s.-2011/offense-definitions) crime types. This file sources in `01a-crime-data.R`.
  - :page_with_curl: `02-decarceration_data.R`: Reads in jail population data from [Vera](https://github.com/vera-institute/jail-population-data). Where counties don't have jail population data, we cite different news sources for estimates of jail pop. 
  - :page_with_curl: `03-location_data.R` : Reads in relevant census data from the cities covered in the analyses.
  - :page_with_curl: `04_analysis.R` : Bind, joins and shapes data across cities in right format to visualize. Calculates crime rates/raw crime numbers per month as well as percent changes in crime from same time period last year. This file sources all prior files (`01* - 03`)
  - :page_with_curl: `05-viz.R` : Visualizations created for publication. This file sources all prior files (`01* - 04`).
- **:open_file_folder: code** : Functions which are sourced into :file_folder: scripts
  - :page_facing_up: `aaa_onLoad.R` : Package and credentials loading for all files. Package dependencies can be found here.
  - :page_facing_up: `custom_theme.R` : Set up custom theme for plots, based on our internal custom themes
  - :page_facing_up: `global_variables.R` : Useful pre-assigned variables used in later scripts
  - :page_facing_up: `utils-cli.R` : Functions for printing to the cli.
  - :page_facing_up: `utils-diagnostics.R` : Quick informal plot/tests for datasets
  - :page_facing_up: `utils-misc.R` : Random misceallaneous convenience functions
  - :page_facing_up: `utils-validation.R` : Formal dataset tests for the core crimes datasets
- **:file_folder: graphics** : Outputs produced by the visualizations files in scripts


### Instructions for Non-ACLU Staff

We use an internal package called `LibLabTemplates` that we've unwound from this repository in advance of public release. The only remaining component is the use of `config` files, which is essentially a simple use of the [config](https://github.com/rstudio/config) package to store credentials. 

To run this code, you'll need to change the following [code block](https://github.com/aclu-national/covid-crimes/blob/e1544be9bdf641636ab302ba3a214eb9f302a1c1/code/aaa_onLoad.R#L30-L39) to your own code:
```r
# setup of tidycensus and other config parameters
my_config <- LibLabTemplates::get_my_config_file()

### NOTE YOU WILL NEED TO USE YOUR OWN KEYS BELOW
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = my_config$aws_access$AWS_ACCESS_KEY_ID,
  "AWS_SECRET_ACCESS_KEY" = my_config$aws_access$AWS_SECRET_ACCESS_KEY,
  "AWS_DEFAULT_REGION" = "us-east-1",
  "CENSUS_API_KEY" = my_config$census$CENSUS_API_KEY
)
```
