packages <- c("shiny", "magrittr", "ggplot2", "dplyr", "leaflet", "ggmap")
lapply(packages, library, character.only = TRUE)

Colias <- read.csv("ColiasLambdaData.csv")
Colias$year= as.factor(Colias$year)

shinyUI <- fluidPage(
  titlePanel("Colias Visualization"),
  sidebarLayout(
    sidebarPanel(
      #checkboxGroupInput("wingAbs", "Wing absorptivity", choices = c(0.5,0.55), selected = c(0.5, 0.55)),
      sliderInput("year", "Year", min = 1950, max = 2099, value = 1999),
      selectInput("abs", "Absorptivity", choices = seq(0.4, 0.7, 0.05)),
      selectInput("gen", "Generation", choices = c(1, 2, 3)),
      radioButtons("metric", "Metric to plot", choices = c("Population growth rate", "Flight activity time", "Egg viability", "Body temperature")),
      
      sliderInput("elev", "Evelation", min = min(Colias$elev), max = max(Colias$elev), value = c(min(Colias$elev), max(Colias$elev))),
      checkboxInput("trendline", "Show Trendline")
      
    ),
    mainPanel(
      #plotOutput("plot"),
      br(), br(), br(), br(), br(), br(),
      plotOutput("mymap")
    )
  )
)