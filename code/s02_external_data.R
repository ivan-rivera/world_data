# Visualising World Development Indicators
# *************************************************************************
# Extracting external data for additional visualisation
# *************************************************************************
#
# Contents:
# - Extracting languages, currencies to be used in force directed graph
# - Extracting bilateral trade to be used in chord diagram
# - Extracting energy consumption data to be used in sankey plot
# - Attempting to construct a hive plot in R
# 
# Inputs:
# - s00_init.R
# - data/dim_country.Rdata
# - data/wdi_data.Rdata
# - country_data_link (link - source)
# - country_list_loc (link - csv)
# 
# Outputs:
# - data/country_attr_df.Rdata # additional country attributes (languages and currencies)
# - data/dim_country.Rdata # extra attributes added to this table
# - data/country_geo.Rdata # map of countries
# - data/bilateral_trade.Rdata # bilateral trade (exports from SOURCE to TARGET)
# - data/sankey_data.Rdata # energy usage data
#
# Notes:
# 

# Preamble ----------------------------------------------------------------

# source init
source("code/s00_init.R")
# load libraries
# pacman::p_load(
#   readxl,
#   rjson,
#   rgdal,
#   Matrix,
#   igraph,
#   xml2,
#   XML
# )

library(readxl)
library(rjson)
library(rgdal)
library(Matrix)
library(igraph)
library(xml2)
library(XML)


#load("data/dim_country.Rdata") # country dimension
load("data/wdi_data.Rdata") # WDI data
dim_country <- read.csv("data/dim_country.csv", stringsAsFactors = FALSE)



# Load country data -------------------------------------------------------

# read raw data
country_info <- fromJSON(paste(readLines(country_data_link), collapse=""))

# create a dataset with attributes
country_attr_df <- data.frame(
  country   = iconv(country_info %>% sapply(function(x) unlist(x$name$common)),"latin1","ASCII"),
  iso3c     = country_info %>% sapply(function(x) unlist(x$cca3)),
  region    = country_info %>% sapply(function(x) unlist(x$region)),
  subregion = country_info %>% sapply(function(x) unlist(x$subregion)),
  currency  = iconv(country_info %>% sapply(function(x) paste(unlist(x$currency),collapse=",")),"latin1","ASCII"),
  languages = iconv(country_info %>% sapply(function(x) paste(unlist(x$languages),collapse=",")),"latin1","ASCII"),
  stringsAsFactors = FALSE
) %>% left_join(
  wdi_data %>% # attach population and GDP
    filter(var %in% c("pop_total","gdp_value")) %>% 
    arrange(country,  var, desc(year)) %>%
    group_by(country, var) %>%
    filter(!is.na(val), # keep the first available value
           row_number(-year) == 1) %>%
    select(-year) %>% ungroup() %>%
    mutate(val = as.numeric(val)) %>%
    spread(var, val) %>% 
    left_join(dim_country %>% 
                select(country, iso3c), 
              by = "country") %>% select(-country)
  , by = "iso3c") %>%
  rowwise() %>%
  mutate(
    # deal with situations where country and language/currency are equal
    languages = gsub(paste0("^",country,"$"),paste(country,"Language"),languages),
    currency  = gsub(paste0("^",country,"$"),paste(country,"Currency"),currency)
  ) %>% ungroup() %>% filter(country != "Tonga") # problem with Tonga

# impute missing values (hot deck)
country_attr_df %<>% left_join(
  country_attr_df %>% 
    group_by(subregion) %>%
    summarise(imp_pop = mean(pop_total,na.rm=T),
              imp_gdp = mean(gdp_value,na.rm=T)),
  by = "subregion"
) %>% mutate(
  pop_total = ifelse(is.na(pop_total),imp_pop,pop_total),
  gdp_value = ifelse(is.na(gdp_value),imp_gdp,gdp_value)
) %>% select(-starts_with("imp")) %>% rename(
  "Population" = pop_total,
  "GDP" = gdp_value
)
# fill the remaining blanks
country_attr_df$Population <- with(country_attr_df,{ifelse(is.na(Population),mean(Population,na.rm=T),Population)})
country_attr_df$GDP <- with(country_attr_df,{ifelse(is.na(GDP),mean(GDP,na.rm=T),GDP)})

# fill in NZ data
country_attr_df[country_attr_df$country == "New Zealand","languages"] = "English"

# add the above attributes to dim_country
dim_country %<>% dplyr::select(country,iso3c,iso2c,capital,longitude,latitude,income,lending)
dim_country %<>% left_join(
  country_attr_df %>% 
    select(-currency,-languages,-country, Population, GDP),
  by = "iso3c"
  )
# adding non-country flag
dim_country <- dim_country[,!(colnames(dim_country) %in% "non_country")]
dim_country %<>% inner_join(read.csv(country_list_loc, stringsAsFactors = FALSE), by = "country")


# Country Geo -------------------------------------------------------------

# load country GeoJSON (no longer used -- going with Plotly's internal maps instead)
# country_geo <- readOGR(paste(readLines(country_gj), collapse=""), "OGRGeoJSON")


# Load Bilateral trade data -----------------------------------------------

bilateral_trade <- data.frame(
  year = integer(),
  country_a = character(),
  country_b = character(),
  trade = integer(),
  stringsAsFactors = FALSE
)

# get file sheet names
file_sheets <- excel_sheets(trade_data_path)

# loop over each year of data
for(yr in file_sheets[-c(1:2)]){
  
  # set up data
  active_data <- read_excel(trade_data_path,sheet = yr)
  row_labels <- as.data.frame(active_data)[,4] # set rownames
  active_data %<>% .[,-c(1:4)] # drop first 3 columns
  active_data %<>% sapply(as.numeric) %>% as.data.frame() # convert to numeric
  active_data$country_a <- row_labels
  
  # trade data
  trade_data <-
    active_data %>% 
    gather(country_b, trade, -country_a) %>%
    select(country_a,country_b,trade) %>%
    filter(country_a != country_b) %>%
    mutate(year  = as.integer(yr)) %>%
    select(year,country_a,country_b,trade) %>%
    filter(trade > 0) # if trade is zero, then remove
  
  # stack
  bilateral_trade <- rbind(bilateral_trade, trade_data)
}

bt_desc <- read_excel(trade_data_path,sheet = "Country codes")

# attributes
bt_attributes <- data.frame(bt_code = with(bilateral_trade,{unique(c(country_a,country_b))}),stringsAsFactors = FALSE) %>%
  # add given country codes
  left_join(
    bt_desc %>% rename("bt_code" = Acronym,
                       "country" = Name),
    by = "bt_code"
    # add regions / countries based on ISO3C
  ) %>% left_join(
    dim_country %>% select(iso3c,region,subregion) %>%
      rename("bt_code"  = iso3c,
             "region1"    = region,
             "subregion1" = subregion),
    by = "bt_code"
    # if cannot match ISO3C, use country name
  ) %>% left_join(
    dim_country %>% select(country,region,subregion) %>%
      rename("region2" = region,
             "subregion2" = subregion),
    by = "country"
  ) %>% mutate(
    region = ifelse(is.na(region1),region2,region1),
    subregion = ifelse(is.na(subregion1), subregion2, subregion1)
  ) %>% select(bt_code, country, subregion, region) # consider dropping everything that is unmatched


bilateral_trade %<>% left_join(
  bt_attributes %>% 
    filter(!is.na(region)) %>%
    mutate(dummy_a = 1) %>%
    select(bt_code, dummy_a) %>% 
    rename("country_a" = bt_code),
  by = "country_a"
) %>% left_join(
  bt_attributes %>% 
    filter(!is.na(region)) %>%
    mutate(dummy_b = 1) %>%
    select(bt_code, dummy_b) %>% 
    rename("country_b" = bt_code),
  by = "country_b"
) %>% na.omit() %>% select(-starts_with("dummy_"))


# Hive Plot ---------------------------------------------------------------
#
# NOTE:
# After investigating how to construct a hive plot in R as well as the quality of output, I decided to leave this for later
# and to construct it in D3. Gephi is another alternative, but it won't integrate into an interactive web application
# 
# create a graph for hive plot (regions = axis, subregions = points and trade = edges)
#
# # aggregate all years
# agg_bt <- bilateral_trade %>%
#   left_join(
#     bt_attributes %>% 
#       rename("country_a"   = bt_code,
#              "subregion_a" = subregion) %>% 
#       select(country_a, subregion_a),
#     by = "country_a"
#   ) %>% left_join(
#     bt_attributes %>% 
#       rename("country_b"   = bt_code,
#              "subregion_b" = subregion) %>% 
#       select(country_b, subregion_b),
#     by = "country_b"
#   ) %>%
#   group_by(subregion_a,subregion_b) %>%
#   summarise(trade = sum(trade,na.rm=T)) %>%
#   ungroup()
# 
# # take the sum of trades both ways
# agg_bt %<>% rename(
#   "a" = subregion_a,
#   "b" = subregion_b
# ) %>% left_join(
#   agg_bt %>% rename(
#     "a" = subregion_b,
#     "b" = subregion_a
#   ), by = c("a","b")
# ) %>% mutate(trade = trade.x+trade.y) %>%
#   select(a,b,trade) %>% # remove dups
#   .[!duplicated(t(apply(.[,c("a","b")], 1, sort))),]
# 
# # create nodes
# trade_agg_nodes <- agg_bt %>% 
#   group_by(a) %>% 
#   summarise(trade = sum(trade)) %>% 
#   ungroup() %>% 
#   rename("subregion" = a) %>%
#   left_join(
#     agg_bt %>% 
#       group_by(b) %>% 
#       summarise(trade = sum(trade)) %>% 
#       ungroup() %>% 
#       rename("subregion" = b),
#     by = "subregion"
#   ) %>% mutate(trade = ifelse(is.na(trade.x),0,trade.x)+ifelse(is.na(trade.y),0,trade.y)) %>%
#   left_join(bt_attributes %>% select(subregion,region) %>% distinct(), by = "subregion") %>%
#   select(region,subregion,trade) %>% filter(trade>0)


# Load energy data --------------------------------------------------------

# define table
sankey_data <- data.frame(stringsAsFactors = FALSE)
for(sheet_name in excel_sheets(energy_data)[-1]){
  # sheet_name <- "flows-overview" # testing
  tmp <- read_excel(energy_data, sheet = sheet_name) # read in
  tmp <- tmp[,colnames(tmp) != ""] # remove trailing blank variable
  reshape_tmp <- data.frame(stringsAsFactors = FALSE)
  max_class_var <- max(which(sapply(tmp,class) %in% "character"))
  var_set <- 1:max_class_var
  for(i in 1:(max_class_var-1)){
    tmp_remove_vars <- setdiff(var_set,c(var_set[c(i,i+1)]))
    if(length(tmp_remove_vars)>0){
      reshape_tmp <- tmp[,-tmp_remove_vars]
    } else {reshape_tmp <- tmp}
    node_group <- colnames(reshape_tmp)[c(1,2)]
    colnames(reshape_tmp)[c(1,2)] <- c("source","target")
    reshape_tmp %<>% gather(year,value,-source,-target)
    reshape_tmp %<>% group_by(source,target,year) %>% summarise(value = sum(value)) %>% ungroup()
    reshape_tmp %<>% filter(source != "Final Energy Consumption",
                            target != "Final Energy Consumption",
                            source != target)
    reshape_tmp$source_group <- node_group[1]
    reshape_tmp$target_group <- node_group[2]
    reshape_tmp <- cbind(data.frame(domain = gsub("flows-","",sheet_name), stringsAsFactors = FALSE), reshape_tmp)
    reshape_tmp %<>% na.omit()
    sankey_data <- rbind(sankey_data,reshape_tmp)
  }
}
sankey_data$year <- as.numeric(sankey_data$year)


# Alternative Population Source -------------------------------------------

# scrape country / area codes (they are not iso2/3c unfortunately
country_code_xml <- read_html(census_gov_codes) %>% xml_find_all('//*[@id="r_countries_0"]/option')
dim_country %<>% 
  mutate(
    country_match = gsub("\\s+","_",gsub("[[:punct:]]","",tolower(country)))
  ) %>% left_join(
    data.frame(
      country     = country_code_xml %>% xml_text(),
      census_code = unlist(country_code_xml %>% xml_attrs("value")),
      stringsAsFactors = FALSE
    ) %>% mutate(
      country = ifelse(country == "Russia","Russian Federation",country),
      country_match = gsub("\\s+","_",gsub("[[:punct:]]","",tolower(country)))
    ) %>% select(country_match, census_code),
    by = "country_match"
  ) %>% select(-country_match)

# get forecasted population data (this will take a few minutes...)
census_codes <- unique(dim_country$census_code) %>% na.omit()
year_span <- seq(1950,2050,10)
census_df <- data.frame(stringsAsFactors = FALSE)
for(i in census_codes){
  tmp_df <- data.frame(
    readHTMLTable(paste0(
        "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y=" ,
        gsub(" ", "", toString(year_span)), "&R=-1&C=",i
      )))
  nms <- c("Year", "Age", "total", "Male", "Female", "percent", "pctMale", "pctFemale", "sexratio")  
  names(tmp_df) <- nms  
  cols <- c(1, 3:9)
  tmp_df[,cols] <- apply(tmp_df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))
  tmp_df <- tmp_df[tmp_df$Age != 'Total',]  
  tmp_df <- cbind(tmp_df, ord = 1:nrow(tmp_df))
  tmp_df$country_code <- i
  census_df <- rbind(census_df,tmp_df)
  # Sys.sleep(1) # consider adding this
} # expect warnings - ignore them
# unique(warnings())
gc()

census_df %<>% left_join(
  dim_country %>% 
    select(country,iso3c,census_code) %>% 
    rename("country_code" = census_code),
  by = "country_code"
)

# append any extra data
wdi_pyramid_data <- rbind(
  wdi_data %>% 
    left_join(
      dim_country %>% select(country,iso3c),
      by = "country"
    ) %>%
    filter(!(iso3c %in% unique(census_df$iso3c)), # exclude those that we get from census
           grepl("^pop_(fe)?male_\\d+",var)
    ) %>% mutate(
      var2 = gsub("^pop_","",var),
      gender = paste0(gsub("(?<=(?<=[a-z]))_.*","",var2,perl=T),"s"),
      gencode = ifelse(gender == "males",1,2),
      age = gsub("_","-",gsub("^(fe)?male_","",var2)),
      age = gsub("^0-4$","00-04",age),
      age = gsub("^5-9$","05-09",age),
      age = gsub("p","-",age),
      val = round(val*1000,0),
      val = ifelse(gender == "males",-1*val,val),
      gender = as.factor(ifelse(gender == "males","Male","Female"))
    ) %>% dplyr::select(country,year,val,gender,gencode,age) %>%
    arrange(country,year,gencode,age) %>%
    group_by(country,gencode) %>% 
    mutate(ord = as.integer(row_number())) %>% 
    ungroup() %>% rename(
      "Age" = age,
      "Year" = year,
      "Gender" = gender,
      "Population" = val
    ) %>% mutate(
      Age = gsub("00-04","0-4",Age),
      Age = gsub("05-09","5-9",Age),
      Age = as.factor(Age)
    ) %>%
    select(country,Age,ord,Year,Gender,Population,gencode)
  ,
  census_df %>% mutate(
    Male = -1*Male
  ) %>% select(country,Year, Age, Male, Female) %>%
    gather(Gender, Population, -Year, -Age, -country) %>% 
    group_by(country,Gender) %>% mutate(ord = row_number()) %>% ungroup() %>% 
    mutate(gencode = ifelse(Gender == "Male",1,2), Gender = as.factor(Gender)) %>%
    select(country, Age, ord, Year, Gender, Population, gencode)
)

# Save Outputs ------------------------------------------------------------

save(country_attr_df, file = "data/country_attr_df.Rdata")
save(dim_country, file = "data/dim_country.Rdata")
# save(census_df, file = "data/census_df.Rdata")
save(wdi_pyramid_data, file = "data/wdi_pyramid_data.Rdata")
# save(country_geo, file = "data/country_geo.Rdata")
save(bilateral_trade, file = "data/bilateral_trade.Rdata")
save(bt_attributes, file = "data/bt_attributes.Rdata")
save(sankey_data, file = "data/sankey_data.Rdata")
