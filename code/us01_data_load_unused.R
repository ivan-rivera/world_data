# Visualising World Development Indicators
# *************************************************************************
# Extracting data for visualisation (CSV download)
# *************************************************************************
#
# Contents:
# - Downloading data from WDI website
# - retaining a subset of variable
# - formating
# 
# Inputs:
# - s00_init.R
# - data/indicator_index.csv
# 
# Outputs:
# - wdi_extract_loc file
# 
# Notes:
# Refer to info.Rmd for information about the selected indicators
#
# Preamble ----------------------------------------------------------------

##########################################################################################################
# *** Usng API instead of CSV extract. Use s01_api_extract ***
##########################################################################################################


# system("source code/wdi_csv_extract.sh") # refresh data extract (updated annually)

# source in code
source("code/s00_init.R")

# load indicator reference data
indicator_index <- read.csv(indicator_csv_index_loc,stringsAsFactors = FALSE)

# load packages
load_packages(c("data.table",
                "tidyr",
                "ggplot2"))

# load data
wdi_full <-
  # get wdi_folder(s)
  list.files("data/") %>% .[grep("wdi_2",.)] %>%
  # isolate the date
  gsub("\\D","",.) %>% as.numeric() %>% 
  # pick the most recent
  sort(decreasing=T) %>% .[1] %>% as.character() %>%
  # convert back to file path
  gsub("(\\d{4})(\\d{2})(\\d{2})","\\1_\\2_\\3",.) %>%
  paste0("wdi_",.) %>% paste0("data/",.,"/WDI_Data.csv") %>%
  fread(stringsAsFactors=F, header=T) %>% as.data.frame() # read file

# fix headers
colnames(wdi_full) %<>% gsub("(^\\d+$)","year_\\1",.)
colnames(wdi_full) %<>% tolower() %>% gsub("\\s+","_",.)

# loading indicator index
wdi_full %>% 
  select(indicator_code,indicator_name) %>%
  distinct() %>% 
  write.csv(file="data/indicator_index_full.csv",row.names = F)

# testing availability of variables
available_vars <-
  indicator_index %>% left_join(
    wdi_full %>% 
      rename("indicator" = indicator_code) %>%
      mutate(in_csv = 1) %>%
      select(indicator,in_csv) %>%
      distinct(),
    by = "indicator"
  ) %>% distinct()

available_vars %>% filter(is.na(in_csv)) # see what isn't in there

# validating data quality
wdi_full %>% filter(
  indicator_code %in% 
    (
      available_vars %>% 
        filter(!is.na(in_csv)) 
    )$indicator
) %>% select(indicator_name,
             starts_with("year")) %>% 
  group_by(indicator_name) %>%
  # for each variable, calculate % of non missing values
  summarise_each(funs(sum(!is.na(.))/n())) %>% 
  ungroup() %>% gather(year,nas,-indicator_name) %>%
  rename(
    "indicator" = indicator_name,
    "year_old"  = year
    ) %>%
  mutate(year = as.numeric(gsub("\\D","",year_old))) %>%
  ggplot(aes(
    x = year,
    y = nas
  )) + geom_line() +
  facet_wrap(~indicator)
