# Visualising World Development Indicators
# *************************************************************************
# Extracting data for visualisation via WDI API
# *************************************************************************
#
# Contents:
# - Connecting to the World Development Indicators API
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

# source init
source("code/s00_init.R")
# load packages
# pacman::p_load(
#   WDI,
#   ggplot2
# )

library(WDI)
library(ggplot2)

#full list of indicators
# indicator_index_full <- WDIcache()$series
# write.csv(as.data.frame(indicator_index_full), file = indicator_index_fl, row.names = F)

# load indicator reference data
indicator_index <- read.csv(
  indicator_api_index_loc, # set to test mode if required
  stringsAsFactors = FALSE)


# Data Extract ------------------------------------------------------------

t1 <- Sys.time()
wdi_data <- WDI(
  country   = "all",
  indicator = indicator_index$indicator,
  start     = "1960",
  end       = "2016",
  extra     = TRUE
)
Sys.time()-t1 # run time: 9 mins
dim(wdi_data) # dimensions: 14160 75 

# cleaning column names
colnames(wdi_data) <- 
  (
    data.frame(indicator = colnames(wdi_data), stringsAsFactors = FALSE) %>%
    left_join(indicator_index %>% 
                select(indicator,label),
              by = "indicator") %>%
    mutate(newcol = ifelse(is.na(label),indicator,label))
  )$newcol

country_attriutes <- c(
  "iso2c",
  "iso3c",
  "capital",
  "lending",
  "income",
  "latitude",
  "longitude"
)

# cleaning character vars
for(txt_var in c("country",country_attriutes))
{
  wdi_data[,txt_var] %<>% iconv("latin1", "ASCII", sub = "")
}

# validate data quality
# png(filename="outputs/data_extract_validation.png")
# wdi_data[,unlist(lapply(wdi_data, class)) == "numeric"] %>% # retain numeric only 
#   gather(var, val, -year) %>%
#   mutate(val = as.numeric(val)) %>%
#   group_by(year,var) %>%
#   summarise(nas = sum(ifelse(is.na(val),0,1))/n()) %>% # calc proportion of non-missings over time
#   ungroup() %>% left_join(
#     indicator_index %>% 
#       select(label, description) %>% 
#       rename("var" = label),
#     by = "var"
#   ) %>%
#   ggplot(aes(
#     x = year,
#     y = nas
#   )) + geom_line() +
#   facet_wrap(~description)
# dev.off()


# convert lat / longs into numeric
wdi_data %<>% mutate(
  latitude  = as.numeric(latitude),
  longitude = as.numeric(longitude)
)

# create a dimension table with country attributes
dim_country <- wdi_data[,colnames(wdi_data) %in% c("country",country_attriutes)] %>% distinct()

# separate country dimensions and reshape
wdi_data %<>% .[,!(colnames(.) %in% country_attriutes)] %>% 
  gather(var, val, -year, -country) %>%
  mutate(val = as.numeric(val)) %>%
  filter(!is.na(val)) # remove missings

# Saving Oputputs ---------------------------------------------------------

save(wdi_data, file = wdi_extract_loc)
#save(dim_country, file = dim_country_loc)
write.csv(dim_country, file = "data/dim_country.csv", row.names=F)

# the below part is only done once, we then need to manually split countries from general groupings
# list of countries (some are not countries, need to pick those out)
# country_list <- unique(wdi_data$country) # list of countries (some are not countries, need to pick those out)
# write.csv(data.frame(country = country_list, stringsAsFactors = F), 
#           file = country_list_loc, row.names=F)

