packages <- c("shiny", "magrittr", "ggplot2", "dplyr", "leaflet", "ggmap", "maps", "raster", "sp", "rgdal", "viridis", "shinysky", "shinythemes", "shinyWidgets", "shinycssloaders", "shinyjs", "colorRamps")
lapply(packages, library, character.only = TRUE)

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
  
  h3("Model description"),
  p("This application uses a biophysical model that was adapted from Buckley and Kingsolver (2019), 
  which is a combination of microclimate, developmental, biophysical, demographic and evolutionary models."),
  tabsetPanel(type = "tabs",
              tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year", "Year", min = 1950, max = 2099, value = 1999),
                          selectInput("abs", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05)),
                          selectInput("gen", "Generation", choices = c(1, 2, 3)),
                          radioButtons("metric", "Metric to plot", choices = c("Population growth rate", "Flight activity time (s)", "Egg viability (%)", "Body temperature (°C)")),
                          
                        ),
                        mainPanel(
                          h4("ggplot"),
                          plotOutput("mymap") %>% withSpinner(type = 7),
                          hr(),
                          h4("leaflet"),
                          leafletOutput("mymap2") %>% withSpinner(type = 7)
                        )
                      )
              ),
              tabPanel("Plot",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("xaxis", "X axis", choices = c("Elevation" = "elev", "Year" = "year")),
                           radioButtons("yaxis", "Y axis", choices = c("Population growth rate", "Flight activity time (s)", "Egg viability (%)", "Body temperature (°C)")),
                           selectInput("color", "Color by", choices = c("Year", "Generation", "Absorptivity")),
                           uiOutput(outputId = 'axisInput'),
                           checkboxInput("trendline", "Show Trendline")
                         ),
                         mainPanel(
                           plotOutput("plot") %>% withSpinner(type = 7)
                         )
                       )
                       
              )
  )
)