# Visualising World Development Indicators
# *************************************************************************
# General Functions and Constants
# *************************************************************************
#
# Contents:
# - Defining a set of functions and constants that will be used by other scripts
# 
# Inputs:
# - None
# 
# Outputs:
# - load_packages()
# - indicator_index_loc path
# - wdi_extract_loc path
# 
# Notes:
# energy data extracted from: http://www.iea.org/etp/explore/
# bilateral trade data extracted from: http://www.economicswebinstitute.org/data/bilateraltrade.zip


# Loading libraries -------------------------------------------------------

# get R version
R.version

# Load libraries
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)


# Constants ---------------------------------------------------------------

indicator_index_fl      <- "data/indicator_index_full.csv"    # full list of indicators
indicator_testing_loc   <- "data/indicator_index_testing.csv" # API for testing (used to see whether a variable of interest is populated)
indicator_api_index_loc <- "data/indicator_index_api.csv"     # indicators dim table (for data pulled from API)
indicator_csv_index_loc <- "data/indicator_index_csv.csv"     # indicators dim table (for data pulled as a CSV file)
wdi_extract_loc         <- "data/wdi_extract.Rdata"           # WDI raw data extract
country_list_loc        <- "data/country_list.csv"            # list of countries
dim_country_loc         <- "data/dim_country.Rdata"           # country dimension table

country_data_link       <- "https://raw.githubusercontent.com/mledoze/countries/master/countries.json" # extra country attributes
country_gj              <- "https://raw.githubusercontent.com/datasets/geo-boundaries-world-110m/master/countries.geojson" # country map

trade_data_path         <- "data/dyadic-compact.xls" # bilateral trade data
energy_data             <- "data/ETP2016_flows-download.xlsx" # energy data

census_gov_codes        <- "http://www.census.gov/population/international/data/idb/informationGateway.php"

# colour scale dataset to be used by the map in VIZ:
color_df <- data.frame(
  var = c(
    "life_expect",
    "birth_rate_p1000",
    "mort_neo_p1000",
    "fertility_rate_pw",
    "cotracept_prev",
    "tuber_drp1000",
    "health_expend_priv_pogdp",
    "health_expend_publ_pogdp"
  ),
  col = c(
    "RdYlGn",
    "BuGn",
    "Reds",
    "Greens",
    "Greens",
    "Reds",
    "Blues",
    "Blues"
  ),
  stringsAsFactors = FALSE
)

# Functions ---------------------------------------------------------------

# function designed to load libraries and report on their status
load_packages <- function(pkg){

  # load
  loaded_pkg  <- unlist(lapply(pkg, require, character.only = TRUE))
  
  release_pkg <- lapply(pkg,function(x) packageDescription(x)$Date)
  release_pkg[sapply(release_pkg, is.null)] <- NA
  release_pkg <- unlist(release_pkg)

  version_pkg <- unlist(lapply(pkg,function(x) as.character(packageVersion(x))))
  version_pkg[sapply(version_pkg, is.null)] <- NA
  version_pkg <- unlist(version_pkg)
  
  # get a list of available updates
  old_pkg <- old.packages()[,c("Package","ReposVer")] %>% 
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::rename("package"     = Package,
           "new_version" = ReposVer)
  # put it all together
  data.frame(package = pkg, 
             loaded  = loaded_pkg,
             release = release_pkg,
             version = version_pkg,
             stringsAsFactors = FALSE) %>%
    left_join(old_pkg, by = "package")
  
}
