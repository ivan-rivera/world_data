# Visualising World Development Indicators
# *************************************************************************
# Visualising data in R Shiny
# *************************************************************************
#
# Contents:
# - UI
# - Server
# - App
# 
# Inputs:
# - 
# 
# Outputs:
# - None
# 
# Notes:
# - use fluidRow() or column(# out of 12) to divide layout, for layers use xPanel (eg tabPanel, navbarPanel or sidebarPanel, dashboardPage)
# - Some useful Shiny resources:
#   * functions: http://shiny.rstudio.com/reference/shiny/latest/
#   * video tutorial: https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a#t=0m0s
#   * more tutorials: http://shiny.rstudio.com
#   * Rshiny and Plotly: https://plot.ly/r/shiny-tutorial/
#   * shiny cheatsheet: http://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#   * shiny themes: https://rstudio.github.io/shinythemes/
#   * shiny dashboards: https://rstudio.github.io/shinydashboard/index.html
#   * HTML & CSS tutorial: https://www.codecademy.com/learn/web
#   * dashboard icons 1: http://fontawesome.io/icons/
#   * dashboard icons 2: http://getbootstrap.com/components/#glyphicons


# Shiny Template ----------------------------------------------------------

# library(shiny)
# 
# ui <- fluidPage( # User Interface
#   input_function(InputId = "trigger")
#   output_function("out_object")
# )
# 
# server <- function(input, output){
#   output$out_object <- render_function({
#     f(input$trigger)
#   })
# }
# 
# shinyApp(ui = ui, server = server)


# Preamble ----------------------------------------------------------------

# load functions and constants
source("code/s03_viz.R")

# loading required libraries
load_packages(c(
  "shiny",
  "shinydashboard"
))


###########################################################################
# UI ----------------------------------------------------------------------
###########################################################################

UI <- dashboardPage( # UI
  skin = "black",
  
  dashboardHeader( # DASHBOARD HEADER
    # Dashboard Header --------------------------------------------------------
    title = "World Data Dashboard"
  ), # DASHBOARD HEADER
  
  dashboardSidebar( # DASHBOARD SIDEBAR
    # Dashboard Sidebar -------------------------------------------------------
    
    sidebarMenu(
      
      id = "sb1",
      menuItem("World Map",               tabName = "world_map",             icon = icon("map")),
      menuItem("Population Overview",     tabName = "pop_overview",          icon = icon("group")),
      menuItem("Languages & Currencies",  tabName = "language_and_currency", icon = icon("language")),
      menuItem("Enonomy and Development", tabName = "econ_development",      icon = icon("money")),
      menuItem("Trade Exports ($M)",      tabName = "trade",                 icon = icon("truck")),
      menuItem("Energy Use",              tabName = "energy_use",            icon = icon("plug"))
    ), # sidebarMenu
    
    
    conditionalPanel(
      
      condition = "input.sb1 == 'world_map'",
      
      radioButtons("map_type", label = h4("Map Type"), choices = list(
        "Flat" = "flat",
        "3D" = "3d"
      )),
      
      selectInput("map_var", label = h4("Map Variable"), choices = list(
        "Life Expectancy"              = "life_expect",
        "Birth Rate"                   = "birth_rate_p1000",
        "Neonatal Mortality"           = "mort_neo_p1000",
        "Fertility"                    = "fertility_rate_pw",
        "Contraceptive Prevalence"     = "cotracept_prev",
        "Tuberculosis Deaths"          = "tuber_drp1000",
        "Health Expenditure - Private" = "health_expend_priv_pogdp",
        "Health Expenditure - Public"  = "health_expend_publ_pogdp"
      )),
      
      sliderInput("map_decade", label = "Map Decade", min = 1960, max = 2010, value = 1960, step = 10, sep = "",
                  animate = animationOptions(interval = 1500, loop = TRUE))
    ),
    
    conditionalPanel( # CONDITIONAL PANEL 1: OVERVIEW
      
      condition = "input.sb1 == 'pop_overview'",
      
      # radioButtons("pyramid_mode", label = h4("Pyramid Mode"), choices = list(
      #   "Absolute" = "absolute",
      #   "Relative" = "percent"
      # )),
      
      
      textInput("country_choice", label = h4("Type Country Name"), value = "New Zealand"),
      
      # sliderInput("scatter_year", label = "Scatterplot Year", min = 1995, max = 2010, value = 1995, step = 1, sep = "",
      #             animate = animationOptions(interval = 1500, loop = TRUE)),
      
      sliderInput("pyramid_year", label = "Age Pyramid Year", min = 1990, max = 2050, value = 1990, step = 10, sep = "",
                  animate = animationOptions(interval = 1500, loop = TRUE))
      
    ), # CONDITIONAL PANEL 1
    
    
    conditionalPanel( # CONDITIONAL PANEL 2: LANGUAGE AND CURRENCY
      
      condition = "input.sb1 == 'language_and_currency'",
      
      # Radio button
      radioButtons("fdg_radio",label = h4("Network"), 
                   choices = list(
                     "Languages"  = "languages", 
                     "Currencies" = "currency"
                     )),
      # Region selection
      selectInput("fdg_select_reg",label = h4("Select Region"),
                  choices = list(
                    "All"      = "All",
                    "Americas" = "Americas",
                    "Asia"     = "Asia",
                    "Africa"   = "Africa",
                    "Europe"   = "Europe",
                    "Oceania"  = "Oceania"
                  ))
    ), # CONDITIONAL PANEL 2
    
    
    conditionalPanel( # CONDITIONAL PANEL 3 - ECONOMY AND DEVELOPMENT
      
      condition = "input.sb1 == 'econ_development'",
      
      sliderInput("heat_year", label = "Year", min = 2008, max = 2013, value = 2008, step = 1, sep = "",
                  animate = animationOptions(interval = 1500, loop = TRUE))
      
    ), # CONDITIONAL PANEL 3
    
    
    
    conditionalPanel( # CONDITIONAL PANEL 4 - TRADE
      
      condition = "input.sb1 == 'trade'",
      
      # year input
      sliderInput("btc_year", label = "Trade Year", min = 1948, max = 2000, value = 1948, step = 1, sep="",
                  animate = animationOptions(interval = 1500, loop = TRUE)),
      
      # granularity
      radioButtons("btc_granularity", label = h4("Granularity"),
                   choices = list(
                     "Subregions" = "subregions",
                     "Country" = "country"
                   )),
      # subregions
      selectInput("btc_georeg",label = h4("Select Region"),
                  choices = list(
                    "Subregions View" = "subregions",
                    "Americas"        = "Americas",
                    "Asia"            = "Asia",
                    "Africa"          = "Africa",
                    "Europe"          = "Europe",
                    "Oceania"         = "Oceania"
                  ))
    ), # CONDITIONAL PANEL 4
    
    conditionalPanel( # CONDITIONAL PANEL 5 - SANKEY ENERGY
      
      condition = "input.sb1 == 'energy_use'",
      
      # year input
      sliderInput("energy_year", label = h4("Year Forecast"), min = 2020, max = 2050, value = 2020, step = 5, sep = "",
                  animate = animationOptions(interval = 1000, loop = TRUE)),
      
      # domain input
      selectInput("energy_domain", label = h4("Domain"),
                  choices = list(
                    "Overview"   = "overview",
                    "Transport"  = "transport",
                    "Buildings"  = "buildings",
                    "Industry"   = "industry"
                  ))
      
    ) # CONDITIONAL PANEL 5
    
  ), # DASHBOARD SIDEBAR
  
  dashboardBody( # DASHBOARD BODY
    # Dashboard Body ----------------------------------------------------------
    
    tabItems( # TAB ITEMS
      
      tabItem(
        tabName = "world_map",
        fluidRow(
          column(12, align = "center", plotlyOutput("map", height = "800px"))
        )
      ),
      
      tabItem( # TAB 1 -- GENERAL OVERVIEW
        tabName = "pop_overview",
        fluidRow(
          #column(6, align = "center", plotlyOutput("scatter")), # world map - conditional panel with "NO DATA" switch
          column(12, align = "center", plotlyOutput("pyramid"))
        ),
        fluidRow(column(12, align = "center", div(style = "height:100px"))),
        fluidRow(column(12, align = "center", plotlyOutput("popsplit")))
      ), # TAB 1
      
      tabItem( # TAB 2 -- LANGUAGES AND CURRENCIES
        tabName = "language_and_currency",
        fluidRow(forceNetworkOutput("fdg", height = "1000px"))
      ), # TAB 2
      
      tabItem( # TAB 3 -- ECONOMY AND DEVELOPMENT
        tabName = "econ_development",
        fluidRow(column(6,  align = "center", h3("Subregional Heatmap of Scaled Economic Indicators (Yellow = High, Blue = Low)")),
                 column(6,  align = "center", h3("Country Ranking Based on Heatmap Selection"))),
        fluidRow(
          
          #column(6,  align = "center", d3heatmapOutput("econheat")), # d3 alternative
          column(6, align = "center", plotlyOutput("econheata")),
          
                 column(6,  align = "center", plotlyOutput("econbar"))),
        fluidRow(column(12, align = "center", h3("Time Series Based on Barchart Selection"))),
        fluidRow(column(12, align = "center", plotlyOutput("econots")))
        
      ), # TAB 3
      
      tabItem( # TAB 4 -- BILATERAL TRADE
        
        tabName = "trade",
        
        conditionalPanel( # CONDITIONAL PANEL 1 (if no data is available, display "No Data" message)
          condition = "output.bt_null",
          div(style = "height:400px"), # center and make sure heights add up to the alternative condition
          column(12, align = "center", h2("No data")),
          div(style = "height:500px") # center
        ), # CONDITIONAL PANEL 1
        
        conditionalPanel( # otherwise, display chord diagram
          condition = "!output.bt_null",
          column(12, align = "center",
                 chorddiagOutput("tradechord", width = "100%", height = "900px")
                 ) # COLUMN
          ), # CONDITIONAL PANEL 2
        
        fluidRow(
          column(12, align = "center", 
                 h3("Granularity can only be selected when Subregion is not set to 'Subregion View'")
          )
        )
      ), # TAB 4
      
      tabItem( # TAB 5 -- Energy Use
        tabName = "energy_use",
        fluidRow(sankeyNetworkOutput("sankey", height = "900px"))
      ) # TAB 5
      
      
    ) # TAB ITEMS
    
  ) # DASHBOARD BODY
  
) # UI







###########################################################################
# Server ------------------------------------------------------------------
###########################################################################



SERVER <- function(input,output){
  
  
  # TAB 1 - WORLD MAP -------------------------------------------------------
  
  output$map <- renderPlotly(plot_map(map_decade = input$map_decade, map_var = input$map_var, map_type = input$map_type)[["plot"]])
  
  
  # TAB 2 - POPULATION OVERVIEW ---------------------------------------------
  
  output$pyramid  <- renderPlotly(pyramid_pl(p_country = tolower(input$country_choice), p_decade = input$pyramid_year, pyramid_mode = "absolute"))
  output$scatter  <- renderPlotly(scatter_3d_reg(input$scatter_year))
  output$popsplit <- renderPlotly(pop_split_bar(tolower(input$country_choice)))
  
  
  # TAB 2 - FDG -------------------------------------------------------------
  
  output$fdg <- renderForceNetwork( # RFN 1
    { # RFN 2

      forced_net_d3(
        plot_type  = input$fdg_radio,     # options: languages, currency
        geo_region = input$fdg_select_reg # options: All or Americas, Asia, Africa, Europe, Oceania
      ) # forced_net_d3

    } # RFN 2
  ) # RFN1
  
  
  # TAB 3 - ECONOMY AND DEVELOPMENT -----------------------------------------
  
  
  # EVENTS
  
  # heatmap
  event_heat_click <- reactive({
    event_info_heat <- event_data("plotly_click", source = "econheata")
    if(is.null(event_info_heat)){
      event_info_heat <- list()
      event_info_heat$pointNumber[1] <- event_info_heat$pointNumber[2] <- 0
    } 
    dat <- econ_heatmap2(input$heat_year)[["data"]] %>% spread(Subregion, val)
    tmp_reg <- colnames(dat)[-1]
    tmp_ind <- dat$Indicator
    target_var <- indicator_index[match(tmp_ind[1+unlist(event_info_heat$pointNumber)[1]],indicator_index$description),"label"]
    target_reg <- gsub("^.*- ","",tmp_reg[1+unlist(event_info_heat$pointNumber)[2]])
    list("var" = target_var, "reg" = target_reg)
  })
  
  # barchart
  event_econbar_click <- reactive({
    event_info_econbar <- event_data("plotly_click", source = "econbar")
    if(is.null(event_info_econbar)){
      event_info_econbar <- list()
      event_info_econbar[["y"]] <- "New Zealand"
    }
    event_info_econbar[["y"]]
  })
  
  # PLOTS
  #output$econheat <- renderD3heatmap(econ_heatmap(input$heat_year)) # heatmap D3
  output$econheata <- renderPlotly(econ_heatmap2(input$heat_year)[["plot"]])
  output$econbar   <- renderPlotly(econ_bar(input$heat_year,event_heat_click()[["reg"]], event_heat_click()[["var"]])) # vertical barchart
  output$econots   <- renderPlotly(ts_eco_plot(event_econbar_click(),event_heat_click()[["var"]])) # TS chart
  

  
  
  # TAB 4 - Chord Diagram ---------------------------------------------------
  
  # obtain the chord diagram
  bt_out <- reactive( # REACTIVE 1
    { # REACTIVE 2
      
      bt_chord(
        trade_year  = input$btc_year, # options: 1948 - 2000
        granularity = input$btc_granularity, # options: country, Subregions
        geo_region  = input$btc_georeg # or choice of regions:  Subregions or Americas, Asia, Africa, Europe, Oceania
        
      )
    } # REACTIVE 2
  ) # REACTIVE 1
  
  # status of chord diagram (to handle missings)
  output$bt_null <- reactive({return(is.null(bt_out()))})
  outputOptions(output, 'bt_null', suspendWhenHidden=FALSE)
  
  output$tradechord <- renderChorddiag(bt_out())

  
  # TAB 5 - Sankey Plot -----------------------------------------------------
  
  output$sankey <- renderSankeyNetwork( # RENDER
    sankey_plot( # SANKEY_PLOT
      sankey_domain = input$energy_domain,     # options: overview, transport, buildings, industry
      sankey_year   = input$energy_year        # options: 2013, 2020, 2025, 2030, 2035, 2040, 2045, 2050
    ) # SANKEY_PLOT
  ) # RENDER
  
}







# Run Shiny ---------------------------------------------------------------

shinyApp(
  ui     = UI,
  server = SERVER
)
