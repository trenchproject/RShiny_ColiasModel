Colias <- readRDS("Colias_complete.rds")
av.Colias <- read.csv("av.Colias.csv")
shortName = c("Population growth rate" = "lambda", "Flight activity time (s)" = "FAT", "Egg viability (%)" = "eggV", "Body temperature (°C)" = "temp")
variables <- c("Year" = "year", "Absorptivity" = "absorp", "Generation" = "gen")

shinyServer <- function(input, output, session) {
  output$axisInput <- renderUI({
    if(input$xaxis == "elev") {
      yearInput <- sliderInput(inputId = "yearPlot", "Year", min = 1950, max = 2099, value = 2020)
      genInput <- selectInput("genPlot", "Generation", choices = c(1, 2, 3), selected = 1, multiple = FALSE)
      absInput <- selectInput("absPlot", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), selected = 0.4, multiple = FALSE)
      if (input$color == "Generation") {
        genInput <- selectInput("genPlot", "Generation", choices = c(1, 2, 3), selected = 1, multiple = TRUE)
      } else if (input$color == "Absorptivity") {
        absInput <- selectInput("absPlot", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), selected = 0.4, multiple = TRUE)
      } else {
        yearInput <- selectInput(inputId = "yearPlot", "Year", choices = c(1950:2099), selected = 2020, multiple = TRUE)
      }
      list(sliderInput("elev", "Evelation", min = min(Colias$elev), max = max(Colias$elev), value = c(min(Colias$elev), max(Colias$elev))),
           yearInput, 
           genInput, 
           absInput
      )
    } else {
      if(input$color == "Generation") {
        genInput <- selectInput("genPlot", "Generation", choices = c(1, 2, 3), selected = 1, multiple = TRUE)
        absInput <- selectInput("absPlot", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), selected = 0.4, multiple = FALSE)
      } else {
        genInput <- selectInput("genPlot", "Generation", choices = c(1, 2, 3), selected = 1, multiple = FALSE)
        absInput <- selectInput("absPlot", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), selected = 0.4, multiple = TRUE)
      }
      list(
        sliderInput("rangeYear", "Year", min = 1950, max = 2099, value = c(1950, 2099)),
        genInput,
        absInput
      )
    }
  })
  
  observe({
    if(input$xaxis == "year") {
      updateSelectInput(session, inputId = "color", label = "Color by", choices = c("Generation", "Absorptivity"))
    } else {
      selectInput("color", "Color by", choices = c("Year", "Generation", "Absorptivity"))
    }
  })

  
  data <- reactive({
    df <- Colias %>% filter(year == input$year & absorp %in% input$abs & gen %in% input$gen)
    df[,c("lat", "lon", paste(shortName[input$metric]))] %>% na.omit()
  })


  output$mymap <- renderLeaflet({
    filtered <- data()
    coordinates(filtered)=~lon+lat
    proj4string(filtered)=CRS("+init=epsg:4326") # set it to lat-long
    gridded(filtered) = TRUE
    dfRaster <- raster(filtered)
    mapStates = map("state", fill = TRUE, plot = FALSE)
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(dfRaster),
                        na.color = "transparent")
    
    leaflet(data = mapStates) %>%
      addProviderTiles(providers$OpenTopoMap) %>%
      setView(lng = (min(Colias$lon) + max(Colias$lon)) / 2, lat = (min(Colias$lat) + max(Colias$lat)) / 2, zoom = 6) %>%
      addRasterImage(dfRaster) %>%
      addLegend(pal = pal, values = values(dfRaster), title = input$metric, position = "bottomright")
  })
  
  
  yearData <- reactive({
    df <- av.Colias %>% filter(year >= input$rangeYear[1] & year <= input$rangeYear[2] & gen %in% input$genPlot & absorp %in% input$absPlot)
    df %>% na.omit()
  })
  
  elevData <- reactive({
    df <- Colias %>% filter(year %in% input$yearPlot & elev >= input$elev[1] & elev <= input$elev[2] & gen %in% input$genPlot & absorp %in% input$absPlot)
    df %>% na.omit()
  })

  output$plot <- renderPlot({
    color <- paste("factor(", variables[input$color], ")")
    
    if (input$xaxis == "year") {
      p <- ggplot(data=yearData(), aes_string(x= input$xaxis, y= paste("av.", shortName[input$yaxis], sep = ""), col = color)) + geom_point() +
           ylab(input$yaxis) + xlab("Year") + theme_bw(base_size = 16) + scale_color_manual(name = input$color, values = c("orange", "blue", "red"))
    } else {
      p <- ggplot(data=elevData(), aes_string(x= input$xaxis, y= shortName[input$yaxis], col = color)) + geom_point() +
           ylab(input$yaxis) + xlab("Elevation (m)") + theme_bw(base_size = 16) + scale_color_manual(name = input$color, values = c("#b35806", "#f1a340", "#998ec3", "#542788", "#fee0b6", "#d8daeb", "#f7f7f7"))
          #theme(legend.position="bottom") + scale_color_manual(name = "year", values = c("orange", "blue"))
    }
    if (input$trendline) {
      p + geom_smooth(method="loess", se=FALSE)
    } else {
      p
    }
  })
  
}
