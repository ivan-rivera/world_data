# Visualising World Development Indicators
# *************************************************************************
# Producing visualisations that will be consumed by Shiny
# *************************************************************************
#
# Contents:
# - Creating visualisations as input functions for Rshiny
#   * forced directed network
#   * chord diagram
#   * sankey plot
# 
# Inputs:
# - s00_init.R
# 
# Outputs:
# - a set of functions (see levels in R Studio)
# 
# Notes:
# R resource:
# * HTML Widgets: http://www.htmlwidgets.org/showcase_leaflet.html
# * Sparklines: https://github.com/htmlwidgets/sparkline
# * rCharts: http://ramnathv.github.io/rCharts/
# Leaflet layers
# * http://leaflet-extras.github.io/leaflet-providers/preview/
# * https://github.com/leaflet-extras/leaflet-providers
# WDI viz:
# * WorldBank visualiser: http://devdata.worldbank.org/DataVisualizer/
# * Google WDI Viz: http://www.google.com/publicdata/explore?ds=d5bncppjof8f9_
# Sankey:
# * http://www.iea.org/etp/explore/
# Stacked charts:
# * http://stackoverflow.com/questions/35371828/plotly-labels-in-r-stacked-area-chart

# Preamble ----------------------------------------------------------------

source("code/s00_init.R") # source init file

# if(!require(chorddiag)){
#   pacman::p_load(devtools)
#   devtools::install_github("mattflor/chorddiag")
# }
# 
# pacman::p_load(
#   viridis,    # * https://github.com/sjmgarnier/viridis
#   ggthemes,   # * https://github.com/jrnold/ggthemes
#   ggplot2,    # * https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#   chorddiag,  # * https://github.com/mattflor/chorddiag
#   networkD3,  # * https://christophergandrud.github.io/networkD3/
#   d3heatmap,  # * https://github.com/rstudio/d3heatmap
#   plotly      # * https://plot.ly/r/shiny-tutorial/
# )             # * http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

library(viridis)
library(ggthemes)
library(ggplot2)
library(chorddiag)
library(networkD3)
library(d3heatmap)
library(plotly)


# load the below data for testing
load("data/dim_country.Rdata") # dim country
load("data/wdi_extract.Rdata") # WDI data
load("data/country_attr_df.Rdata") # country attributes
load("data/bilateral_trade.Rdata") # bilateral trade matrix
load("data/bt_attributes.Rdata") # BT attributes
load("data/sankey_data.Rdata") # energy use network
load("data/wdi_pyramid_data.Rdata") # census - population forecast for pyramid chart

indicator_index <- read.csv(
  indicator_api_index_loc, # set to test mode if required
  stringsAsFactors = FALSE)


# capitaliser
capitaliser <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


# Tab 1: General Overview -------------------------------------------------

# world map
plot_map <- function(
  map_decade = 2000,
  map_var = "life_expect",
  map_type = "flat"
){
  if(!(map_var %in% c(
    "life_expect",
    "birth_rate_p1000",
    "mort_neo_p1000",
    "fertility_rate_pw",
    "cotracept_prev",
    "tuber_drp1000",
    "health_expend_priv_pogdp",
    "health_expend_publ_pogdp"
  ))){stop(
    paste0("Illegal variable choice. Values allowed: ",
           paste0(color_df$var,collapse=", "))
  )}
  if(!(map_type %in% c("flat","3d"))){stop("map_type must be either 'flat' or '3d'.")}
  if(map_type == "flat"){
    geo_list <- c(
      showframe = FALSE,
      showland = TRUE,
      landcolor = toRGB("grey90"),
      bgcolor = 'rgba(0,0,0,0)',
      showcoastlines = FALSE,
      list(lonaxis = list(range = c(-170, 180))), # zoom in a little
      list(lataxis = list(range = c(-50, 85)))
    )
  } else{
    geo_list <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      bgcolor = 'rgba(0,0,0,0)',
      projection = list(type = 'orthographic')
    )
  }
  map_data <- wdi_data %>% mutate(
    decade = round_any(year,10,floor)
  ) %>% group_by(country,var,decade) %>% 
    summarise(val = mean(val,na.rm=T)) %>% ungroup() %>%
    filter(decade == map_decade, 
           var  == map_var) %>% 
    #mutate(val = as.integer(val)) %>%
    dplyr::select(country,val) %>%
    inner_join( # get ISO3C values
      dim_country %>% 
        filter(!is.na(iso3c), non_country == 0)  %>%
        dplyr::select(country,iso3c),
      by = "country"
    ) %>% inner_join(
      wdi_data %>% mutate(decade = round_any(year,10,floor)) %>%
        filter(var == "pop_total", !is.na(val), decade == map_decade) %>% group_by(country) %>% 
        summarise(val = mean(val, na.rm=T)) %>% ungroup() %>% filter(val > 2*10^6) %>% select(country),
      by = "country"
    )
  
  map_title <- paste("World Map:",indicator_index[indicator_index$label==map_var,"description"])
  map_attr_show_scale <- TRUE
  
  if(nrow(map_data) == 0){
    map_data <- dim_country %>% 
      filter(!is.na(iso3c), non_country == 0) %>% 
      select(country, iso3c) %>% 
      mutate(val = 0)
    map_title <- "No data"
    map_attr_show_scale <- FALSE
  }
  return(list(
    "data" = map_data,
    "plot" = plot_ly(map_data,
                     z = val,
                     text = country,
                     #locationmode = 'country names',
                     locations = iso3c,
                     type = "choropleth",
                     color = val,
                     colorbar = list(title = "Scale"),
                     colors = color_df[color_df$var == map_var,"col"],
                     marker = list(line = list(color = toRGB("grey"), width = 0.5)),
                     inherit = FALSE,
                     showscale = map_attr_show_scale,
                     source = "map"
    )  %>% layout(
      title = map_title,
      geo   = geo_list,
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
  ))
}

# example of use: (allowable values: life_expect,birth_rate_p1000,mort_neo_p1000,fertility_rate_pw,cotracept_prev
#                  tuber_drp1000,health_expend_priv_pogdp,health_expend_publ_pogdp)
# plot_map(map_decade = 1960, map_var = "tuber_drp1000", map_type = "flat")



# Age Pyramid (Animated)
age_pyramid <- function(country_age = "Australia"){
  p_dat <- wdi_pyramid_data %>% filter(country == "New Zealand") %>% select(-country) %>% filter(!is.na(Population))
  if(nrow(p_dat) == 0){stop("Unavailable information")}
  d1 <- dPlot(
    x = "Population", 
    y = "Age", 
    groups = "Gender", 
    data = p_dat, 
    type = 'bar')
  d1$yAxis(type = "addCategoryAxis", orderRule = "ord")
  d1$xAxis(type = "addMeasureAxis")
  d1$legend( x = 60, y = 10, width = 700, height = 20, horizontalAlign = "right")
  d1$colorAxis(
    type = "addColorAxis", 
    colorSeries = "gencode", 
    palette = c("skyblue","salmon")
  )
  d1$set(storyboard = "Year")
  max_x <- round_any(max(p_dat$Population), 10000, f = ceiling)
  min_x <- round_any(min(p_dat$Population), 10000, f = floor)
  d1$xAxis(overrideMax = max_x, overrideMin = min_x)
  if (max(p_dat$Population >= 1000000)) {
    d1$setTemplate( afterScript = 
                      "
                    <script>
                    x._getFormat = function () {
                    return function(d) {
                    return d3.format(',.1f')(Math.abs(d) / 1000000) + 'm';
                    };
                    };
                    myChart.draw()
                    </script>
                    ")
  } else {
    d1$setTemplate( afterScript = 
                      "
                    <script>
                    x._getFormat = function () {
                    return function(d) {
                    return d3.format(',.0f')(Math.abs(d) / 1000) + 'k';
                    };
                    };
                    myChart.draw()
                    </script>
                    ")
  }
  return(d1)
}

# example of use:
# age_pyramid("New Zealand")




# Age Pyramid (Plotly)
pyramid_pl <- function(p_country,p_decade,pyramid_mode="absolute"){
  p_dat <- 
    wdi_pyramid_data %>%
    mutate(decade = round_any(Year,10,floor),
           Population = Population / 1000,
           country = tolower(country)
    ) %>% filter(country == p_country, decade == p_decade) %>%
    group_by(decade,Age, Gender) %>% 
    summarise(Population = sum(Population)) %>% ungroup() %>%
    mutate(order_var = formatC(as.numeric(gsub("(\\d{1,3}).*","\\1",Age)),2,flag=0)) %>%
    arrange(order_var)
  if(nrow(p_dat)==0){stop("No data")}
  # get maximum value, divide in half and set ticks
  plot_title <- ": Age Pyramid (in 1000s)"
  
  if(pyramid_mode == "percent"){
    plot_title <- ": Age Pyramid - Gender Imbalance"
    p_dat %<>% mutate(Population = abs(Population)) %>% 
      select(-order_var, -decade) %>% 
      spread(Gender,Population) %>%
      mutate(Total = Female+Male,
             Female = Female/Total - 0.5,
             Male = Male/Total - 0.5) %>%
      select(-Total,-Female) %>% 
      mutate(Gender = ifelse(Male > 0, "Male","Female")) %>%
      rename("Population" = Male) %>%
      mutate(Population = round(Population*100,2))
  }
  
  pop_max <- round(max(abs(p_dat$Population)))
  top_tick <- round_any(pop_max,f=floor, 10^(nchar(pop_max)-1))
  mid_tick <- top_tick/2
  tick_values <- c(-top_tick, -mid_tick, 0, mid_tick, top_tick)
  tick_labs <- c(as.character(top_tick), as.character(mid_tick), "0", as.character(mid_tick), as.character(top_tick))
  if(pyramid_mode == "percent"){
    tick_labs <- paste0(tick_labs,"%")
  }
  
  plot_ly(p_dat,
          x = Population,
          y = Age,
          group = Gender,
          type = "bar",
          orientation = "h",
          hoverinfo = "y+text+name",
          text = abs(Population)
  ) %>% layout(
    title = paste0(capitaliser(p_country),plot_title),
    bargap = 0.1,
    barmode = "overlay",
    paper_bgcolor = 'rgba(0,0,0,0)',
    plot_bgcolor = 'rgba(0,0,0,0)',
    xaxis = list(
      tickmode = "array",
      tickvals = tick_values,
      ticktext = tick_labs
    )
  )
}

# example of use:
# pyramid_pl("Japan",2000,"absolute")



# urban / rural population split

pop_split_bar <- function(bar_country){
  bar_data <- wdi_data %>% mutate(
      country = tolower(country)
    ) %>% filter(
      country == bar_country,
      var %in% c("pop_total","pop_rural_pot")
    )
  if(nrow(bar_data) == 0){stop("No data")}
  bar_data %>% mutate(
    decade = round_any(year,10,floor)
  ) %>% group_by(decade,var) %>% 
    summarise(val = mean(val)) %>% ungroup() %>%
    spread(var,val) %>% mutate(
      pop_rural_p = pop_rural_pot/100,
      Rural = as.integer(pop_rural_p*pop_total),
      Urban = pop_total-Rural,
      Unknown = ifelse(is.na(Rural),pop_total,0)
    ) %>% select(decade,Rural,Urban,Unknown) %>% 
    gather(Type,Population,-decade) %>%
    filter(!is.na(Population))  %>% 
    rename("Decade" = decade) %>% plot_ly(
      x = Decade,
      y = Population,
      color = Type,
      colors = c("bisque3","burlywood1","grey90"),
      type = "bar"
    ) %>% layout(
      title = paste0(capitaliser(bar_country),": Rural / Urban Population Split"),
      bargap = 0.1,
      barmode = "stack",
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
}

# example of use:
# pop_split_bar("China")


# 3d scatterplot
scatter_3d_reg <- function(scatter_year){
  wdi_data %>% 
    left_join(dim_country %>% 
                select(country, 
                       non_country, 
                       subregion,
                       region), 
              by = "country") %>%
    filter(var %in% c(
      "pop_total",
      "gdp_value",
      "life_expect",
      #"fertility_rate_pw",
      "health_expend_priv_pogdp",
      "health_expend_publ_pogdp"), 
      !is.na(val),
      non_country == 0,
      country != "Marshall Islands") %>% # outlier
    #mutate(decade = round_any(year,10,floor)) %>% 
    group_by(
      year, 
      region,
      subregion,
      #country, 
      var) %>%
    summarise(val = mean(val,na.rm=T)) %>% ungroup() %>%
    spread(var,val) %>% na.omit() %>%
    mutate(total_health_exp = health_expend_priv_pogdp+health_expend_publ_pogdp,
           Population = round(pop_total)) %>% 
    filter(year == scatter_year) %>% # from 1995 to 2014
    plot_ly(
      x = total_health_exp,
      #y = fertility_rate_pw,
      y = gdp_value,
      z = life_expect,
      group = region,
      size = Population,
      type = "scatter3d",
      mode = "markers",
      text = subregion,
      marker = list(opacity = 0.6, size = 4)
    ) %>% layout(
      title = "Countries by Life Expectancy, Fertility Rate and Health Expenditure",
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      scene = list(
        xaxis = list(title = "Total Health Expenditure as % of GDP", showgrid = T),
        yaxis = list(title = "GDP($)", showgrid = T),
        zaxis = list(title = "Life Expectancy in Years", showgrid = T)
      ),
      camera = list(eye = list(x = -1, y = 1, z = -1))
    )
}

# example of use:
# scatter_3d_reg(1995)



# 2d scatterplot

scatter_reg <- function(scatter_year){
  wdi_data %>% 
    left_join(dim_country %>% 
                select(country, 
                       non_country, 
                       subregion,
                       region), 
              by = "country") %>%
    filter(var %in% c(
      "pop_total",
      "life_expect",
      "health_expend_priv_pogdp",
      "health_expend_publ_pogdp"), 
      !is.na(val),
      non_country == 0,
      country != "Marshall Islands") %>% # outlier
    #mutate(decade = round_any(year,10,floor)) %>% 
    group_by(
      year, 
      region,
      country, 
      var) %>%
    summarise(val = mean(val,na.rm=T)) %>% ungroup() %>%
    spread(var,val) %>% na.omit() %>%
    mutate(total_health_exp = health_expend_priv_pogdp+health_expend_publ_pogdp,
           Population = round(pop_total)) %>% 
    filter(year == scatter_year) %>%
    plot_ly(
      x = total_health_exp,
      y = life_expect,
      group = region,
      size = Population,
      text = country,
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0.4)
    ) %>% layout(
      title = "Health Expenditure vs Life Expectancy",
      xaxis = list(title = "Total Health Expenditure as % of GDP"),
      yaxis = list(title = "Life Expectancy"),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
      #hovermode = "y"
    )
}
# example of use:
# scatter_reg(1995)


# Tab 2: Languages and Currencies -----------------------------------------

# force directed network
forced_net_d3 <- function(
  plot_type  = "languages", # languages or currency
  geo_region =  "All"    # All or Americas, Asia, Africa, Europe, Oceania
){
  measure_attr <- ifelse(plot_type == "languages", "Population", "GDP")
  
  # isolate attributes of interest
  country_attr_reshape <- country_attr_df %>%
    filter(grepl(ifelse(geo_region == "All", "", geo_region),region)) %>%
    select_("country", plot_type, measure_attr) %>%
    rename_(.dots = setNames(measure_attr, "measure")) %>% # rename measure
    rename_(.dots = setNames(plot_type, "plot_type")) %>% # rename plot type
    mutate(measure = as.integer(1+1000*measure/max(measure)),
           plot_type = gsub("\\s+","_",plot_type)) %>%
    filter(!is.na(country), !is.na(measure))
  
  # reshape into a matrix
  attr_set <- unique(unlist(strsplit(country_attr_reshape$plot_type,","))) %>% .[!is.na(.)]
  for(i in attr_set){
    country_attr_reshape[,i] <- grepl(i,country_attr_reshape$plot_type)
  }
  
  # reshape back into df
  country_attr_reshape %<>% 
    select(-plot_type) %>% 
    gather(plot_type, val, -country, -measure) %>%
    filter(val) %>%
    mutate(val = as.numeric(val),
           plot_type = gsub("_"," ",plot_type)) %>%
    filter(country != plot_type)
  
  # isolate attributes
  country_list_n <- country_attr_reshape %>% group_by(country)   %>% summarise(size = mean(measure,na.rm=T)) %>% ungroup()
  measure_list_n <- country_attr_reshape %>% group_by(plot_type) %>% summarise(size = sum(measure,na.rm=T))  %>% ungroup()
  
  country_list  <- country_list_n$country
  measure_list  <- measure_list_n$plot_type
  node_list     <- c(country_list,measure_list)
  grp           <- c(rep(1,length(country_list)),rep(2,length(measure_list)))
  nodesize      <- c(country_list_n$size, measure_list_n$size)
  
  # set up node file
  node_order <- seq(from = 0, to = length(node_list)-1)
  node_file  <- data.frame(name = factor(node_order, 
                                         levels = node_order, 
                                         labels=node_list),
                           size = as.integer(nodesize*1), # increase if nodes are too small
                           group = as.integer(grp),
                           id = node_order,
                           stringsAsFactors = FALSE)
  
  # set up edge file
  edge_file <- country_attr_reshape %>% rename("value" = val) %>%
    left_join(
      node_file %>%
        mutate(country = as.character(name)) %>%
        select(country, id) %>%
        rename("source" = id),
      by = "country"
    ) %>%
    left_join(
      node_file %>%
        mutate(plot_type = as.character(name)) %>%
        select(plot_type, id) %>%
        rename("target" = id),
      by = "plot_type"
    ) %>% select(source,target,value)
  
  # plot the network
  forceNetwork(Links             = edge_file,
               Nodes             = node_file,
               Source            = "source",
               Target            = "target",
               Value             = "value",
               NodeID            = "name",
               Nodesize          = "size",
               Group             = "group",
               linkColour        = "#50c878", # "#50c878"
               fontSize          = 6,
               fontFamily        = "Roboto,Arial Rounded MT Bold,Arial",
               charge            = -200,
               zoom              = TRUE, 
               legend            = FALSE,
               bounded           = FALSE,
               # when background is black, use: #FFFFFF, #FFFF00
               colourScale       = JS('d3.scale.ordinal().domain(["X", "Y"]).range(["#4d4d4d", "#800000"]);'),
               radiusCalculation = JS("Math.sqrt(d.nodesize)+5"),
               linkWidth         = JS("function(d) { return Math.sqrt(d.value)/5; }"),
               #clickAction       = 'd3.select(this).style("fill","lightcoral");',
               #linkDistance      = JS('function(){d3.select("body").style("background-color", "#333333"); return 50;}'),
               opacity           = 0.8,
               opacityNoHover    = 0.4)  #%>% saveNetwork(file = "/Users/Ivan/Documents/Projects/my_shiny_apps/wdi/outputs/wdi_fdg.html")
}

# example of use: (geo_region = All or Americas, Asia, Africa, Europe, Oceania)
# forced_net_d3(
#   plot_type = "currency", 
#   geo_region = "All") %>% 
#   saveNetwork(file = "/Users/Ivan/Documents/Projects/my_shiny_apps/wdi/wdi_fdg_cur.html")




# Tab 3 - Economy and Development -----------------------------------------

# heatmap of key economic indicators
econ_heatmap <- function(heat_year){ # range from 2008 to 2013
  heat_data <- 
    wdi_data %>% # year range: 2008 to 2013
    left_join(dim_country %>% select(country, subregion, region, non_country), by = "country") %>%
    filter(non_country == 0, !is.na(val), var %in% c(
      "gni_per_capita",
      "external_ds_pgni",
      "cash_surp_def_pc",
      "tax_inc_prof_cap_apor",
      "tax_on_goods_apor",
      "foregin_d_invest_ni",
      "employee_comp_poe",
      "deposit_interest_rate",
      "lending_interest_rate",
      "unemploy_total_apol",
      "self_employ_apoe",
      "bank_cap_to_asset_r",
      # "govt_dbt_pgdp",
      #"income_held_by_h10",
      "gdp_value",
      "military_exp_gdp"
    )) %>% filter(year == heat_year, !is.na(region), !is.na(subregion)) %>% 
    group_by(
      region, 
      subregion, 
      var) %>%
    summarise(val = round(mean(val,na.rm=T),2)) %>% 
    ungroup() %>% spread(var,val) %>% 
    mutate(reg_subreg = paste(region,subregion,sep=" - ")) %>%
    select(-region,-subregion)
  heat_data <- heat_data[,c(3,9,2,1,5:8,10:15)]
  rownames(heat_data) <- heat_data$reg_subreg
  heat_data %<>% select(-reg_subreg)
  colnames(heat_data) <- indicator_index[match(colnames(heat_data),indicator_index$label),"description"]
  d3heatmap(heat_data %>% scale(),
            cellnote = heat_data,
            scale = "column", 
            colors = "Blues",
            xaxis_height =180,
            yaxis_width = 180,
            xaxis_font_size = 8,
            yaxis_font_size = 8,
            show_grid = F,
            Rowv = FALSE,
            Colv = FALSE)
}

# example of use
# econ_heatmap(2013)


# economic heatmap -- ggplot + plotly
econ_heatmap2 <- function(heat_year){
  heat_data <- 
    wdi_data %>% # year range: 2008 to 2013
    left_join(dim_country %>% select(country, subregion, region, non_country), by = "country") %>%
    filter(non_country == 0, !is.na(val), var %in% c(
      "gni_per_capita",
      "external_ds_pgni",
      "cash_surp_def_pc",
      "tax_inc_prof_cap_apor",
      "tax_on_goods_apor",
      "foregin_d_invest_ni",
      "employee_comp_poe",
      "deposit_interest_rate",
      "lending_interest_rate",
      "unemploy_total_apol",
      "self_employ_apoe",
      "bank_cap_to_asset_r",
      # "govt_dbt_pgdp",
      #"income_held_by_h10",
      "gdp_value",
      "military_exp_gdp"
    )) %>% filter(year == heat_year, !is.na(region), !is.na(subregion)) %>% 
    group_by(
      region, 
      subregion, 
      var) %>%
    summarise(val = round(mean(val,na.rm=T),2)) %>% 
    ungroup() %>% mutate(Subregion = paste(region,subregion,sep=" - ")) %>%
    select(Subregion, var, val) %>% spread(var,val)
  heat_data[,2:ncol(heat_data)] %<>% scale() 
  heat_data %<>% gather(var,val,-Subregion)
  heat_data %<>% left_join(indicator_index %>% 
                             select(label, description) %>% 
                             rename("var" = label, "Indicator" = description), 
                           by = "var") %>% select(-var)
  
  p <- heat_data %>%
    ggplot(aes(x = Subregion, y = Indicator, fill = val)) + 
    geom_tile(color = "white", size = 0.1) + scale_fill_viridis(name = "Deviation") + 
    coord_equal() + labs(x=NULL, y=NULL) + theme_tufte(base_family="Helvetica") +
    guides(fill = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  return(list(
    "data" = heat_data,
    "plot" = ggplotly(p, source = "econheata") %>% layout(paper_bgcolor = 'rgba(0,0,0,0)',plot_bgcolor = 'rgba(0,0,0,0)')
  ))
}


# zooming into a specific region from the heatmap
econ_bar <- function(eb_year,eb_sr,eb_var){
  wdi_data %>% left_join(
    dim_country %>% 
      select(
        country, 
        subregion, 
        region, 
        non_country), 
    by = "country") %>%
    filter(
      year == eb_year,
      subregion == eb_sr,
      var == eb_var
    ) %>% arrange(val) %>%
    plot_ly(
      x = val,
      y = country,
      type = "bar",
      orientation = "h",
      source = "econbar"
    ) %>% layout(
      title = indicator_index[match(eb_var,indicator_index$label),"description"],
      yaxis = list(title = ""),
      xaxis = list(title = ""),
      margin = list(l = 150),
      bargap = 0.1,
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
}

# example of use:
# econ_bar(2013,"Eastern Africa","lending_interest_rate")



# Time series for selected country

ts_eco_plot <- function(ts_country, ts_var){
  # allowed ts_var variables:
  # "gni_per_capita",
  # "external_ds_pgni",
  # "cash_surp_def_pc",
  # "tax_inc_prof_cap_apor",
  # "tax_on_goods_apor",
  # "foregin_d_invest_ni",
  # "employee_comp_poe",
  # "deposit_interest_rate",
  # "lending_interest_rate",
  # "unemploy_total_apol",
  # "self_employ_apoe",
  # "bank_cap_to_asset_r"
  wdi_data %>% 
    left_join(dim_country %>% select(country, subregion, region, non_country), by = "country") %>%
    filter(
      country == ts_country,
      !is.na(val),
      non_country == 0,
      var %in% ts_var
    ) %>% select(year,val) %>%
    left_join(
      wdi_data %>% 
        left_join(dim_country %>% select(country, subregion, region, non_country), by = "country") %>%
        filter(var == ts_var, !is.na(val), non_country == 0) %>%
        group_by(year) %>% summarise(gval = median(val,na.rm=T)) %>% ungroup(),
      by = "year"
    ) %>% gather(var,val, -year) %>% 
    mutate(g = ifelse(var == "val",ts_country,"Global Median"))%>% 
    arrange(year) %>% 
    plot_ly(
      x = year,
      y = val,
      group = g
    ) %>% layout(
      title = indicator_index[match(ts_var,indicator_index$label),"description"],
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)'
    )
}

# example of use:
# ts_eco_plot("New Zealand","foregin_d_invest_ni")




# Development indicators
dev_trend <- function(dev_country,dev_var){
  # dev vars:
  # "Scientific and technical journal articles" -- sci_tech_pub
  # "Control of Corruption Index" -- corrupt_control
  # "Political Stability and Absence of Violence/Terrorism: Estimate" -- political_stability
  wdi_data %>%
    left_join(dim_country %>% select(country, subregion, region, non_country), by = "country") %>%
    filter(
      country == dev_country,
      !is.na(val),
      non_country == 0,
      var == dev_var
    ) %>% select(year,val) %>%
    left_join(
      wdi_data %>% 
        left_join(dim_country %>% 
                    select(
                      country, 
                      subregion, 
                      region, 
                      non_country), 
                  by = "country") %>% 
        filter(non_country == 0, !is.na(val), var == dev_var) %>%
        group_by(year) %>% summarise(val = median(val,na.rm=T)) %>% 
        ungroup() %>% rename("gval" = val),
      by = "year"
    ) %>% gather(var,value,-year) %>% arrange(year) %>% 
    mutate(g = ifelse(var == "val",dev_country,"Global Median")) %>%
    plot_ly(
      x = year,
      y = value,
      group = g
    ) %>% layout(
      title = indicator_index[match(dev_var,indicator_index$label),"description"], 
      yaxis = list(title = ""))
}

# example of use:
# dev_trend("Sweden","political_stability")




# Tab 4 - Bilateral Trade -------------------------------------------------


# Chord diagram

bt_chord <- function(
  trade_year  = 2000, # range: 1948 to 2000
  granularity = "Subregions",
  geo_region  = "Subregions" # or choice of regions:  Subregions or Americas, Asia, Africa, Europe, Oceania
){
  bt_subset <- bilateral_trade %>% 
    filter(year == trade_year) %>% 
    select(-year) %>% left_join(
      # join attributes A
      bt_attributes %>%
        rename(
          "country_a" = bt_code,
          "country_name_a" = country,
          "subregion_a" = subregion,
          "region_a" = region
        ), by = "country_a"
    ) %>% left_join(
      # joining attributes B
      bt_attributes %>%
        rename(
          "country_b" = bt_code,
          "country_name_b" = country,
          "subregion_b" = subregion,
          "region_b" = region
        ), by = "country_b"
    ) %>% select(country_name_a, subregion_a, region_a,
                 country_name_b, subregion_b, region_b,
                 trade)
  
  # aggregate / restrict further
  if(geo_region == "subregions"){
    bt_subset %<>% 
      group_by(subregion_a, 
               subregion_b) %>%
      summarise(trade = sum(trade)) %>%
      ungroup() %>% rename(
        "category_a" = subregion_a,
        "category_b" = subregion_b
      )
  } else{
    if(granularity == "country"){
      bt_subset %<>% 
        filter(region_a == geo_region, 
               region_b == geo_region) %>% rename(
                 "category_a" = country_name_a,
                 "category_b" = country_name_b
               ) %>%
        select(category_a, category_b, trade)
    } else{
      bt_subset %<>% 
        filter(region_a == geo_region, 
               region_b == geo_region) %>%
        group_by(subregion_a, 
                 subregion_b) %>%
        summarise(trade = sum(trade)) %>%
        ungroup() %>% rename(
          "category_a" = subregion_a,
          "category_b" = subregion_b
        )
    }
  }
  
  if(nrow(bt_subset) > 0){
    # make a square matrix
    id_list <- unique(c(bt_subset$category_a,bt_subset$category_b))
    id_length <- length(id_list)
    chord_matrix <- matrix(NA,nrow=id_length,ncol=id_length,dimnames=list(id_list,id_list))
    for(i in 1:id_length){
      for(j in 1:id_length){
        tmp_index <- which(bt_subset$category_a == id_list[i] & bt_subset$category_b == id_list[j])
        if(length(tmp_index)>0){
          chord_matrix[i,j] <- as.data.frame(bt_subset)[tmp_index,"trade"] 
        }
      }
    }
    
    # plot
    chorddiag(chord_matrix,
              margin = 150,
              showTicks =FALSE,
              groupnameFontsize = 12,
              groupnamePadding = 5,
              groupThickness = .05,
              chordedgeColor = "gray90",
              groupColors  = colorRampPalette(c("tomato2",
                                                "honeydew4",
                                                "aquamarine3",
                                                "antiquewhite4",
                                                "coral3",
                                                "burlywood3",
                                                "gold3",
                                                "cornsilk3",
                                                "mediumpurple4"
              ))(nrow(chord_matrix))
    ) # %>% saveNetwork(file = "/Users/Ivan/Documents/Projects/my_shiny_apps/wdi/outputs/wdi_chord.html")
  }
}

# example of use (granularity = country or other, geo_region = Subregions or Americas, Asia, Africa, Europe, Oceania)
#bt_chord(trade_year = 1980,
#         granularity = "country",
#         geo_region = "Asia")  # %>% saveNetwork(file = "/Users/Ivan/Documents/Projects/my_shiny_apps/wdi/outputs/wdi_chord.html")




# Tab 5 - Energy Use ------------------------------------------------------


# Sankey plot
sankey_plot <- function(
  sankey_domain = "overview", # options: overview, transport, buildings, industry
  sankey_year   = 2013        # options: 2013, 2020, 2025, 2030, 2035, 2040, 2045, 2050
){
  # subset the data
  sankey_subset <- sankey_data %>% 
    filter(year   == sankey_year, 
           domain == sankey_domain,
           value > 0) %>% 
    dplyr::select(-year,-domain)
  
  # set up the nodes
  sankey_nodes    <- data.frame(name = with(sankey_subset,{unique(c(source,target))}), stringsAsFactors = FALSE) 
  sankey_nodes$id <- as.integer(1:nrow(sankey_nodes)-1)
  sankey_nodes %<>% 
    left_join(
      sankey_subset %>% 
        dplyr::select(source,source_group) %>%
        rename("name" = source),
      by = "name"
    ) %>%
    left_join(
      sankey_subset %>% 
        dplyr::select(target,target_group) %>%
        rename("name" = target),
      by = "name"
    ) %>% mutate(
      group = ifelse(is.na(source_group), target_group, source_group)
    ) %>% dplyr::select(-target_group, -source_group) %>% distinct(name,id)
  
  # setting up the edges
  sankey_edges <- sankey_subset %>% 
    left_join(
      sankey_nodes %>% dplyr::select(name, id) %>% rename(
        "source_id" = id,
        "source" = name
      ), by = "source"
    ) %>% 
    left_join(
      sankey_nodes %>% dplyr::select(name, id) %>% rename(
        "target_id" = id,
        "target" = name
      ), by = "target"
    ) %>% dplyr::select(source_id,target_id,value) %>%
    rename(
      "source" = source_id,
      "target" = target_id
    )
  
  # plotting
  sankeyNetwork(Links = sankey_edges,
                Nodes = sankey_nodes,
                Source = "source",
                Target = "target",
                Value = "value",
                NodeID = "name",
                NodeGroup = "group",
                nodePadding = 30,
                units = "EJ",
                fontSize = 10,
                nodeWidth = 30,
                fontFamily = "Arial")
}

# example of use: (domain options: overview, transport, buildings, industry; year options: 2013, 2020, 2025, 2030, 2035, 2040, 2045, 2050)
# sankey_plot(
#   sankey_domain = "overview", 
#   sankey_year   = 2020
# ) %>% saveNetwork(file = "/Users/Ivan/Documents/Projects/my_shiny_apps/wdi/outputs/wdi_sankey.html")

