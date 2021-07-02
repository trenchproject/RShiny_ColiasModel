packages <- c("shiny", "magrittr", "ggplot2", "dplyr", "leaflet", "maps", "raster", "sp", "rgdal", "viridis", "shinythemes", "shinyWidgets", "shinycssloaders", "shinyjs", "sortable", "rnoaa", "chillR", "reshape2", "tidyr", "gridExtra", "shinyBS", "gridExtra", "ggmap")
# package.check <- lapply(
#   packages,
#   FUN = function(x) {
#     if (!require(x, character.only = TRUE)) {
#       install.packages(x, dependencies = TRUE)
#       library(x, character.only = TRUE)
#     }
#   }
# )
# lapply(packages, library, character.only = TRUE)

library("shiny")
library("magrittr")
library("ggplot2")
library("dplyr")
library("leaflet")
library("maps")
library("raster")
library("sp")
library("rgdal")
library("viridis")
library("shinythemes")
library("shinyWidgets")
library("shinycssloaders")
library("shinyjs")
#library("colorRamps")
library("sortable")
library("rnoaa")
library("chillR")
library("reshape2")
#library("rasterVis")
library("tidyr")
library("raster")
library("shinyBS")
library("gridExtra")
library("ggmap")
library("cicerone")

coltags<-
  lapply(c("Year", "Elevation", "Absorptivity", "Generation"),
    function(co) {
      tag("p",
          list(
            class = class(co),
            tags$span(class = "glyphicon glyphicon-move"),
            tags$strong(co)
          )
      )
    }
  )


Colias <- readRDS("Colias_complete.rds")



shinyUI <- fluidPage(
  use_cicerone(),
  theme = shinytheme("united"),
  setBackgroundColor(color = "#C7DAE0"), 
  useShinyjs(),
  titlePanel(
    div(tags$img(src="TRENCH_Logo_Circle-TrenchMap.png", height = 100), 
        "Butterfly wing coloration and population responses to climate change")
  ),
  title = "Butterfly wing coloration and population responses to climate change",
  hr(),
  
  includeHTML("intro.html"),
  br(), br(),
  h3("Wing coloration alters butterfly body temperatures ", icon("thermometer-half")),
  p("Let's take a look at how butterfly body temperatures differed from air 
    temperatures over the past week in Crested Butte, CO at elevation of 2700m (38.9°, -107.0°). 
    You can change the proportion of solar radiation that is absorbed by the wings from 0.4 (lighter wings) to 0.7 (darker wings). 
    You can also select whether sky conditions are clear or cloudy. How do predicted body temperatures change?"),
  p(strong("How do we estimate body temperatures?"), "Minimum and maximum daily temperatures are retrieved from a weather station and converted to hourly temperatures using functions from chillR. We then use a microclimate model to scale temperatures to 
    plant height. An energy budget function ", 
  code("TrenchR::Tb_butterfly"), "was then used to compute how butterflies exhange heat with their environment and predict their body temperature. We assume ",
  em("Colias eriphyle"), "butterflies are in the sun with wind
  speeds of 0.1 m/s, have a fur thicknesses of 0.82 mm and a thorax diameter of 3.6 mm. We use the ", code("TrenchR::direct_solar_radiation"), " function to estimate hourly
  direct solar radiation and then estimate the fraction of radiation that is diffuse based on whether the weather is clear or cloudy."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("abs_intro", "Wing absorptivity (proportion)", choices = seq(0.4, 0.7, 0.05)),
      selectInput("weather", "Weather", choices = c("Clear", "Partly cloudy", "Cloudy"))
    ),
    mainPanel(
      plotOutput("plot_intro") %>% withSpinner(type = 7)
    )
  ),
  
  includeHTML("intro2.html"),
  br(),
  hr(),
  div(
    id = "viz-wrapper",
    tabsetPanel(type = "tabs", id = "tabs",
                tabPanel("Map",
                        sidebarLayout(
                          sidebarPanel(
                            h4(icon("map-marked-alt"), " Map"),
                            p("Select an individual or population metric to plot. The layer can be switched between 'data' and the underlying 'elevation' by clicking the button on the top. Clicking 
                              on the map gives you detailed data of the specific location. You can select the year, value of wind absorptivity, and generation (1 to 3) to plot"),
                            actionBttn(
                              inputId = "reset_map",
                              label = "Reset", 
                              style = "material-flat",
                              color = "danger",
                              size = "xs"
                            ),
                            actionBttn(
                              inputId = "tour_map",
                              label = "Take a tour!", 
                              style = "material-flat",
                              color = "success",
                              size = "xs"
                            ),
                            hr(),
                            
                            div(
                              id = "metric-wrapper",
                              checkboxGroupInput("metric", "Metric to plot", 
                                                 choices = c("Population growth rate (lambda)", 
                                                             "Flight activity time (h)", 
                                                             "   Egg viability (%)    ", 
                                                             "Body temperature (°C)"),
                                                 selected = "Population growth rate (lambda)")
                            ),
                            div(
                              id = "facet-wrapper",
                              radioButtons("facet", "Facets", choices = c("Wing absorptivity" = "absorp", "Generation" = "gen"))
                            ),
                            div(
                              id = "year-wrapper",
                              sliderInput("year", "Year", min = 1950, max = 2099, value = 2020, sep = "")
                            ),
                            div(
                              id = "abs-wrapper",
                              uiOutput("absInput")
                            ),
                            uiOutput("genInput")
                          ),
                          mainPanel(id = "main",
                            br(),
                            fluidRow(
                              column(3, 
                                     id = "layer-wrapper", 
                                     switchInput(inputId = "layer", label = "Layer", onLabel = "Data", offLabel = "Elevation", inline = TRUE, value = TRUE, size = "small")),
                              column(3, 
                                     offset = 6, 
                                     id = "labels-wrapper", 
                                     materialSwitch("labels", status = "danger", label = "Labels On/Off", value = TRUE))
                            ),
                            verbatimTextOutput("info"),
                            plotOutput("mymap_gg", click = "plot_click") %>% withSpinner(type = 7)
                            # leafletOutput("mymap") %>% withSpinner(type = 7)
                          )
                        )
                ),
                tabPanel("Plot",
                         sidebarLayout(
                           sidebarPanel(
                             h4(icon("chart-bar"), " Plot"),
                             p("Here you can visualize annual population growth rate (where 1= stable population and 2= population size doubling), 
                             the hours of potential flight activity, egg viability (%), and mean body temperature (°C) 
                               You can plot data across years or elevation, both of which can be colored and faceted by other variables by dragging", 
                               icon("glyphicon glyphicon-move", lib = "glyphicon"), "."),
                             actionBttn(
                               inputId = "reset_plot",
                               label = "Reset", 
                               style = "material-flat",
                               color = "danger",
                               size = "xs"
                             ),
                             actionBttn(
                               inputId = "tour_plot",
                               label = "Take a tour!", 
                               style = "material-flat",
                               color = "success",
                               size = "xs"
                             ),
                             hr(),
                             fluidRow(id = "panel-wrapper",
                               column(6,
                                 tags$div(
                                   class = "panel panel-default",
                                   tags$div(
                                     class = "panel-heading",
                                     tags$span(class = "glyphicon glyphicon-search"),"Variables"),
                                   tags$div(
                                     class = "panel-body",
                                     id = "sort1",
                                     coltags
                                   )
                                 
                                 ),
                                 checkboxGroupInput("yaxis", "Y axis", 
                                                    choices = c("Population growth rate (lambda)", 
                                                                "Flight activity time (h)", 
                                                                "   Egg viability (%)    ", 
                                                                "Body temperature (°C)"),
                                                    selected = "Population growth rate (lambda)")
                                 
                               ),
                               column(5,
                                 tags$div(
                                   class = "panel panel-default",
                                   tags$div(
                                     class = "panel-heading",
                                     tags$span(class = "glyphicon glyphicon-stats"),
                                     "x axis"
                                   ),
                                   tags$div(
                                     class = "panel-body",
                                     id = "sort2"
                                   )
                                 ),
  
                                 tags$div(
                                   class = "panel panel-default",
                                   tags$div(
                                     class = "panel-heading",
                                     tags$span(class = "glyphicon glyphicon-tint"),
                                     "Color"
                                   ),
                                   tags$div(
                                     class = "panel-body",
                                     id = "sort3"
                                   )
                                 ),
  
                                 tags$div(
                                   class = "panel panel-default",
                                   tags$div(
                                     class = "panel-heading",
                                     tags$span(class = "glyphicon glyphicon-th-list"),
                                     "Facets"
                                   ),
                                   tags$div(
                                     class = "panel-body",
                                     id = "sort4"
                                   )
                                 )
                               )
                             ),
                               

                                uiOutput(outputId = 'widgetInput'),
                             div(
                               id = "variables-wrapper",
                               selectInput("absPlot", "Wing absorptivity (proportion)", choices = seq(0.4, 0.7, 0.05), selected = 0.4, multiple = TRUE),
                               selectInput("genPlot", "Generation", choices = c(1, 2, 3), selected = 1, multiple = TRUE)
                             ),
                             div(
                               id = "trend-wrapper", 
                               checkboxInput("trendline", "Show Trendline")
                             )
                           ),
                           
                           mainPanel(
                             div(
                               id = "plot-wrapper",
                               plotOutput("plot") %>% withSpinner(type = 7)
                             )
                           )
                         )
                         
                )
                
    )
  ),
  sortable_js(
    "sort1",
    options = sortable_options(
      group = list(
        name = "sortGroup1",
        put = TRUE
      ),
      sort = FALSE, 
      onSort = sortable_js_capture_input("sort_vars")
    )
  ),
  
  sortable_js(
    "sort2",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      onSort = sortable_js_capture_input("sort_x")
    )
  ),
  
  sortable_js(
    "sort3",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      onSort = sortable_js_capture_input("sort_col")
    )
  ),
  
  sortable_js(
    "sort4",
    options = sortable_options(
      group = list(
        group = "sortGroup1",
        put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
        pull = TRUE
      ),
      onSort = sortable_js_capture_input("sort_facet")
    )
  ),
  
  bsTooltip("facet", "Each value of this parameter will make a new map side by side.", placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip("abs", "Larger value correspond to darker coloration.", placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip("sort2", "Drag 'Year' or 'Elevation' here for x axis.", placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip("sort3", "Each value of this parameter will be colored differently on the same plot.", placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip("sort4", "Each value of this parameter will make a new plot side by side.", placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip("gen", "1st, 2nd or 3rd generation of the given year.", placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip("genPlot", "1st, 2nd or 3rd generation of the given year.", placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip("absPlot", "Larger value correspond to darker coloration.", placement = "bottom", trigger = "hover", options = NULL),
  bsTooltip("reset_map", "If you have already changed the variables, reset them to default here before starting the tour."),
  bsTooltip("reset_plot", "If you have already changed the variables, reset them to default here before starting the tour.")
  
)