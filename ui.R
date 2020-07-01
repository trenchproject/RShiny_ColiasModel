packages <- c("shiny", "magrittr", "ggplot2", "dplyr", "leaflet", "ggmap", "maps", "raster", "sp", "rgdal", "viridis", "shinythemes", "shinyWidgets", "shinycssloaders", "shinyjs", "colorRamps", "sortable", "rnoaa", "chillR", "reshape2", "rasterVis")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

coltags<-
  lapply(
    c("Year", "Elevation", "Absorptivity", "Generation"),
    function(co) {
      tag(
        "p",
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
  br(),
  h4("Quick glance at Colias body temperatures"),
  sidebarLayout(
    sidebarPanel(
      selectInput("abs_intro", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05)),
      selectInput("weather", "Weather", choices = c("Sunny", "Partially sunny", "Overcast"))
    ),
    mainPanel(
      plotOutput("plot_intro") %>% withSpinner(type = 7),
      p("*the plot assumes butterfly body temperature in sun and wind speed = 1 m/s")
    )
  ),
  
  includeHTML("intro2.html"),
  
  tabsetPanel(type = "tabs", id = "tabs",
              tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(
                          h4("For leaflet"),
                          sliderInput("year", "Year", min = 1950, max = 2099, value = 1999),
                          selectInput("abs", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05)),
                          selectInput("gen", "Generation", choices = c(1, 2, 3)),
                          radioButtons("metric", "Metric to plot", choices = c("Population growth rate", "Flight activity time (s)", "Egg viability (%)", "Body temperature (°C)"))
                        ),
                        mainPanel(
                          h4("Leaflet"),
                          leafletOutput("mymap") %>% withSpinner(type = 7)
                        )
                      ),
                      
                      sidebarLayout(
                        sidebarPanel(
                          h4("For ggplot"),
                          radioButtons("metric_gg", "Metric to plot", choices = c("Population growth rate", "Flight activity time (s)", "Egg viability (%)", "Body temperature (°C)")),
                          radioButtons("facet_gg", "Facets", choices = c("Wing absorptivity" = "absorp", "Generation" = "gen")),
                          sliderInput("year_gg", "Year", min = 1950, max = 2099, value = 1999),
                          selectInput("abs_gg", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), multiple = TRUE, selected = 0.4),
                          selectInput("gen_gg", "Generation", choices = c(1, 2, 3), multiple = TRUE, selected = 1)
                        ),
                        mainPanel(
                          h4("ggplot"),
                          plotOutput("mymap_gg") %>% withSpinner(type = 7),
                          leafletOutput("topo")
                        )
                      )
              ),
              tabPanel("Plot",
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(

                             column(6,
                               tags$div(
                                 class = "panel panel-default",
                                 tags$div(class = "panel-heading", "Variables"),
                                 tags$div(
                                   class = "panel-body",
                                   id = "sort1",
                                   coltags
                                 )
                               
                               ),
                               radioButtons("yaxis", "Y axis", choices = c("Population growth rate", "Flight activity time (s)", "Egg viability (%)", "Body temperature (°C)"))
                               
                             ),
                             column(5,
                               tags$div(
                                 class = "panel panel-default",
                                 tags$div(
                                   class = "panel-heading",
                                   tags$span(class = "glyphicon glyphicon-stats"),
                                   "x axis (Year or Elevation)"
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
                                   tags$span(class = "glyphicon glyphicon-stats"),
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
                                   tags$span(class = "glyphicon glyphicon-stats"),
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
  )
)