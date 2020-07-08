packages <- c("shiny", "magrittr", "ggplot2", "dplyr", "leaflet", "ggmap", "maps", "raster", "sp", "rgdal", "viridis", "shinythemes", "shinyWidgets", "shinycssloaders", "shinyjs", "colorRamps", "sortable", "rnoaa", "chillR", "reshape2", "rasterVis", "tidyr", "gridExtra", "shinyBS", "gridExtra")
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
library("colorRamps")
library("sortable")
library("rnoaa")
library("chillR")
library("reshape2")
library("rasterVis")
library("tidyr")
library("raster")
library("shinyBS")
library("gridExtra")

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
  theme = shinytheme("united"),
  setBackgroundColor(color = "#F5F5F5"), 
  useShinyjs(),
  titlePanel(
    div(tags$img(src="Butterfly_icon.png", height = 50), 
        "Colias Visualization")
  ),
  hr(),
  
  includeHTML("intro.html"),
  br(), br(),
  h4("Quick glance at Colias body temperatures ", icon("thermometer-half")),
  p("Let's take a look at how butterfly body temperatures fluctuate during the day in comparison to the air 
    temperature in the past week. Change the weather conditon and absorptivity to see how it affects their body temperatures. Larger absoprtivity correlates to darker wings."),
  p("Minimum and maximum daily temperatures are obtained at a weather station in Crested Butte, CO,
    at elevation of 2700m (38.9°, -107.0°), and converted to hourly temperatures using functions from chillR.", code("TrenchR::Tb_butterfly"), "function was then used to compute the operative temperature of butterflies. 
    Wind speed is set at 1 m/s, and it models butterfly body temperature in the sun during the day unless overcast. 
    Fur thickness = 0.82 mm and thorax diameter = 3.6 mm are used based on measurements for", em("C. eriphyle"), "at several sites in Colorado (Kingsolver, 1983).
    Solar radiation is set to 900 W/m", tags$sup("2"), "for a sunny day, 500 W/m", tags$sup("2"), "for a partially cloudy day and 200 W/m", tags$sup("2"), "for an overcast day."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("abs_intro", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05)),
      selectInput("weather", "Weather", choices = c("Sunny", "Partially cloudy", "Overcast"))
    ),
    mainPanel(
      plotOutput("plot_intro") %>% withSpinner(type = 7)
    )
  ),
  
  includeHTML("intro2.html"),
  br(),
  hr(),
  tabsetPanel(type = "tabs", id = "tabs",
              tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(
                          h4(icon("map-marked-alt"), " Map"),
                          p("This activity displays 4 fitness-related parameters of Colias in western Colorado. 
                            The layer can be switched between 'data' and 'elevation' by clicking the button on the top, where 'data' shows the selected parameters and 'elevation' displays the topography across the range.
                            Clicking on the map gives you detailed data of the specific location."),
                          checkboxGroupInput("metric", "Metric to plot", 
                                             choices = c("Population growth rate", 
                                                         "Flight activity time (s)", 
                                                         "   Egg viability (%)    ", 
                                                         "Body temperature (°C)"),
                                             selected = "Population growth rate"),
                          radioButtons("facet", "Facets", choices = c("Wing absorptivity" = "absorp", "Generation" = "gen")),
                          sliderInput("year", "Year", min = 1950, max = 2099, value = 1999),
                          selectInput("abs", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), multiple = TRUE, selected = 0.4),
                          selectInput("gen", "Generation", choices = c(1, 2, 3), multiple = TRUE, selected = 1)
                        ),
                        mainPanel(
                          switchInput(inputId = "layer", label = "Layer", onLabel = "Data", offLabel = "Elevation", inline = TRUE, value = TRUE, size = "small"),
                          
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
                           p("This activity visualizes 4 fitness-related parameters of Colias as a plot. You can plot data across year or thier distributed elevation range, both of which can be colored and faceted by other variables by dragging", icon("glyphicon glyphicon-move", lib = "glyphicon"), "."),
                           fluidRow(

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
                                                  choices = c("Population growth rate", 
                                                              "Flight activity time (s)", 
                                                              "Egg viability (%)", 
                                                              "Body temperature (°C)"),
                                                  selected = "Population growth rate")
                               
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
                             )),

                             uiOutput(outputId = 'widgetInput'),
                             checkboxInput("trendline", "Show Trendline")
                         ),
                         mainPanel(
                           plotOutput("plot") %>% withSpinner(type = 7)
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
  bsTooltip("gen", "1st, 2nd or 3rd generation of the given year.", placement = "bottom", trigger = "hover", options = NULL)
)