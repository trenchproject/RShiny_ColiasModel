source("functions.R", local = TRUE)

Colias <- readRDS("Colias_complete.rds")
shortName = c("Population growth rate" = "lambda", "Flight activity time (s)" = "FAT", "Egg viability (%)" = "eggV", "Body temperature (°C)" = "temp")
variables <- c("Year" = "year", "Absorptivity" = "absorp", "Generation" = "gen", "Elevation" = "elev")
elevCat <- c("1392 ~ 1700", "1701 ~ 2000", "2001 ~ 2300", "2301 ~ 2600", "2601 ~ 2900", "2901 ~ 3198")

ghcnd(stationid = "GHCND:USC00051959")

tMin <- ghcnd_search(stationid = "USC00051959", token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", var = "TMIN")
tMax <- ghcnd_search(stationid = "USC00051959", token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", var = "TMAX")

tMax <- as.data.frame(tMax) %>% dplyr::select(c(tmax.tmax, tmax.date))
tMin <- as.data.frame(tMin) %>% dplyr::select(c(tmin.tmin, tmin.date))

tMax <- filter(tMax, tmax.date > Sys.Date() - 9, tmax.date < Sys.Date() - 1)

tMin <- filter(tMin, tmin.date > Sys.Date() - 9, tmin.date < Sys.Date() - 1)
colnames(tMin)[2] <- "tmax.date"

temp <- merge(tMax, tMin, by = "tmax.date")
temp[,c(2:3)] <- temp[,c(2:3)]/10
temp$Year <- NA
temp$Month <- NA
temp$Day <- NA
for (row in 1:dim(temp)[1]) {
  temp$Year[row] <- strsplit(as.character(temp$tmax.date), "-")[[row]][1]
  temp$Month[row] <- strsplit(as.character(temp$tmax.date), "-")[[row]][2]
  temp$Day[row] <- strsplit(as.character(temp$tmax.date), "-")[[row]][3]
}
colnames(temp)[c(2,3)] <- c("Tmax", "Tmin")

hourlyTemp <- make_hourly_temps(mean(Colias$lat), temp)
hourlyTemp <- hourlyTemp[, c(8:31)] %>% t() %>% as.data.frame()

hourlyTemp$Hour <- c(0:23)
colnames(hourlyTemp) <- c(as.character(seq.Date(from = Sys.Date() - 8, to = Sys.Date() - 10 + length(hourlyTemp), by = "1 day")), "Hour")

combined <- melt(hourlyTemp, id.vars = "Hour")

for(row in 1:dim(combined)[1]) {
  combined$dateHour[row] <- paste0(combined$variable[row], " ", combined$Hour[row], ":00")
}
hours <- as.POSIXct(combined$dateHour, format="%Y-%m-%d %H:%M")
combined$dateHour <- hours



shinyServer <- function(input, output, session) {
  
  output$plot_intro <- renderPlot({
    if (input$weather == "Sunny") {
      H_sdir <- 900
    } else if (input$weather == "Partially sunny") {
      H_sdir <- 500
    } else {
      H_sdir <- 200
    }
    
    seq <- rep(seq.Date(from = Sys.Date() - 8, to = Sys.Date() - 10 + length(hourlyTemp), by = "1 day"), each = 24)
    doy = day_of_year(seq)
    hour = rep(0:23, times = length(hourlyTemp) - 1)
    z <- zenith_angle(doy= doy, lat = mean(Colias$lat), lon = mean(Colias$lon), hour= hour)
    shade <- rep(FALSE, length(z))
    for (i in 1:length(z)) {
      if (z[i] == 90) {
        shade[i] <- TRUE
      }
    }
    colors <- c("Environmental \ntemperature" = "black", "Operative \ntemperature" = "blue")
    
    Tb <- mapply(Tb_butterfly, 
                 combined$value, combined$value+5, Tg_sh = combined$value-5, u = 1, H_sdir = H_sdir, H_sdif = 400, z = 30, D=0.36, delta=1.46, alpha=as.numeric(input$abs_intro), r_g=0.3, shade = shade)
    
    ggplot() + geom_line(aes(x = combined$dateHour, y = combined$value, color = "Environmental \ntemperature"), size = 1.3) + geom_line(aes(x = combined$dateHour, y = Tb, color = "Operative \ntemperature"), size = 1.3) +
      xlab("Day") + ylab("Temperature (°C)") + theme_bw() + ggtitle("Colias body temperatures in the past week") +
      scale_color_manual(values = colors) +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 13), axis.title = element_text(size = 16), legend.text = element_text(size = 13), 
            legend.title = element_blank())
  })
  
  # widgets to show
  output$widgetInput <- renderUI({
    validate(
      need(input$sort_x == "Year" || input$sort_x == "Elevation", "")
    )
    genInput <- selectInput("genPlot", "Generation", choices = c(1, 2, 3), selected = 1, multiple = TRUE)
    absInput <- selectInput("absPlot", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), selected = 0.4, multiple = TRUE)
    elevInput <- sliderInput("elev", "Elevation (m)", min = min(Colias$elev), max = max(Colias$elev), value = c(min(Colias$elev), max(Colias$elev)))
    
    if(input$sort_x == "Elevation") {
      yearInput <- sliderInput(inputId = "yearPlot", "Year", min = 1950, max = 2099, value = 2020)
      if (!is.null(input$sort_col) || !is.null(input$sort_facet)) {
        if (input$sort_col == "Year" || input$sort_facet == "Year") {
          yearInput <- selectInput(inputId = "yearPlot", "Year", choices = c(1950:2099), selected = 2020, multiple = TRUE)
        }
      }
    } else {  # sort_x == "Year"
      yearInput <- sliderInput("yearPlot", "Year", min = 1950, max = 2099, value = c(1950, 2099))
      if (!is.null(input$sort_col) && length(input$sort_col) != 0) {
        if (input$sort_col == "Elevation") {
          elevInput <- selectInput("elev", "Elevation (m)", choices = elevCat, selected = "1392 ~ 1700", multiple = TRUE)
        }
      }
    }
    list(elevInput, yearInput, genInput, absInput)
  })

  # Data filter for map
  
  data <- reactive({
    df <- Colias %>% filter(year == input$year & absorp %in% input$abs & gen %in% input$gen) %>% na.omit()
    df[,c("lat", "lon", paste(shortName[input$metric]))] %>% na.omit()
  })

  
  output$mymap <- renderLeaflet({
    filtered <- data()
    coordinates(filtered)=~lon+lat
    proj4string(filtered)=CRS("+init=epsg:4326") # set it to lat-long
    gridded(filtered) = TRUE
    dfRaster <- raster(filtered)
    mapStates = map("state", fill = TRUE, plot = FALSE)
    pal <- colorNumeric(c("Red", "Orange"), values(dfRaster),
                        na.color = "transparent")
    
    leaflet(data = mapStates) %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      setView(lng = (min(Colias$lon) + max(Colias$lon)) / 2, lat = (min(Colias$lat) + max(Colias$lat)) / 2, zoom = 6) %>%
      addRasterImage(dfRaster, opacity = 0.7, colors = pal) %>%
      addLegend(pal = pal, values = values(dfRaster), title = input$metric, position = "bottomright")
  })
  
  # data filter for ggplot
  data_gg <- reactive({
    df <- Colias %>% filter(year == input$year_gg & absorp %in% input$abs_gg & gen %in% input$gen_gg) %>% na.omit()
    df[,c("lat", "lon", "absorp", "gen", paste(shortName[input$metric_gg]))] %>% na.omit() %>% 
      gather(Param, value, shortName[input$metric_gg])

  })
  
  output$mymap_gg <- renderPlot({
    
    ggplot() +
      borders(fill="grey",colour="black") +
      xlab("Longitude (°)") + ylab("Latitude (°)") + theme_bw( ) + 
      geom_raster(data = data_gg(), aes_string(x = "lon", y = "lat", fill = "value")) +
      coord_quickmap(xlim = c(min(data_gg()$lon), max(data_gg()$lon)), ylim = c(min(data_gg()$lat), max(data_gg()$lat)), expand = TRUE) +
      scale_fill_gradientn(name = input$metric_gg, colors = viridis(12), na.value = "white") + facet_grid(as.formula(paste("Param ~", input$facet_gg))) +
      theme(strip.text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5)) +
      #theme(plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "#F5F5F5")) +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
            legend.title = element_text(size = 12)) #+ geom_sf() #+ coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)
  })
  
  output$topo <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      setView(lng = (min(Colias$lon) + max(Colias$lon)) / 2, lat = (min(Colias$lat) + max(Colias$lat)) / 2, zoom = 6) %>%
      addRectangles(lng1 = min(Colias$lon), lng2 = max(Colias$lon), lat1 = min(Colias$lat), lat2 = max(Colias$lat))
  })
  
  # Data for plot when x axis is Year
  yearData <- reactive({
    validate(
      need(input$elev, "")
    )
    if (is.integer(input$elev)) {  # when elevation is a slider input
      if (input$sort_x == "Elevation") {
        df <- Colias %>% filter(year %in% input$yearPlot & elev >= input$elev[1] & elev <= input$elev[2] & gen %in% input$genPlot & absorp %in% input$absPlot)
      } else {
        df <- Colias %>% filter(year >= input$yearPlot[1] & year <= input$yearPlot[2] & elev >= input$elev[1] & elev <= input$elev[2] & gen %in% input$genPlot & absorp %in% input$absPlot)
      }
      df <- df %>% na.omit() %>% 
        group_by(year, absorp, gen) %>%
        summarise(lambda = mean(lambda), FAT = mean(FAT), eggV = mean(eggV), temp = mean(temp))
      
    } else {  # when elevation is a select input (only when x = year & col = elevation)
      Colias$elevRange <- NA  # new column named elevRange
      for (i in 1:length(input$elev)) {
        min <- as.numeric(strsplit(input$elev[i], split = " ")[[1]][1]) 
        max <- as.numeric(strsplit(input$elev[i], split = " ")[[1]][3])
        Colias[Colias$elev >= min & Colias$elev <= max, ]$elevRange <- input$elev[i]
      }
      av.Colias <- Colias[] %>% 
        group_by(year, absorp, gen, elevRange) %>% na.omit() %>% 
        summarise(lambda = mean(lambda), FAT = mean(FAT), eggV = mean(eggV), temp = mean(temp))
      df <- av.Colias %>% filter(year >= input$yearPlot[1] & year <= input$yearPlot[2] & gen %in% input$genPlot & absorp %in% input$absPlot & elevRange %in% input$elev )
      df %>% na.omit()
    }
    df %>% 
      gather(Param, value, shortName[input$yaxis])
  })

  # Data for plot when x axis is Elevation
  elevData <- reactive({
    df <- Colias %>% filter(year %in% input$yearPlot & elev >= input$elev[1] & elev <= input$elev[2] & gen %in% input$genPlot & absorp %in% input$absPlot)
    df %>% na.omit() %>% 
      gather(Param, value, shortName[input$yaxis])
  })
  
  # title <- reactive({
  #   validate(
  #     need(input$sort_x, "")
  #   )
  #   if (!is.null(input$sort_col) && length(input$sort_col) != 0) {
  #     if (input$sort_x == "Elevation") {
  #       title <- ""
  #       if (input$sort_col != "Year" && input$sort_facet != "Year") {
  #         title <- paste(title, "Year :", input$yearPlot)
  #       } 
  #       if (input$sort_col != "Generation" && input$sort_facet != "Generation") {
  #         title <- paste("| Generation :", input$genPlot)
  #       }
  #       if (input$sort_col != "Absorptivity" && input$sort_facet != "Absorptivity") {
  #         title <- paste("| Absorptivity :", input$absPlot)
  #       }
  #     }
  #   } else {
  #     title <- paste("Year :", input$yearPlot, "| Generation :", input$genPlot, "| Absorptivity :", input$absPlot)
  #   }
  #   title
  # })
  
  output$plot <- renderPlot({
    validate(
      need(input$sort_x == "Year" || input$sort_x == "Elevation", "Drag \"Year\" or \"Elevation\" to x axis")
    )
    if (!is.null(input$sort_col) && length(input$sort_col) != 0) {  # with color
      
      color <- paste("factor(", variables[input$sort_col], ")")

      if (input$sort_x == "Year") {
        if (input$sort_col == "Elevation") {
          p <- ggplot(data=yearData(), aes_string(x= variables[input$sort_x], y= "value", col = "elevRange"))
        } else {  # x axis = year, color = gen or absorp
          p <- ggplot(data=yearData(), aes_string(x= variables[input$sort_x], y= "value", col = color))
        }
      } else {  # x axis = Elevation, color = anything
        p <- ggplot(data=elevData(), aes_string(x= variables[input$sort_x], y= "value", col = color))
      }
      p <- p + scale_color_manual(name = input$sort_col, values = c("#b35806", "#f1a340", "#998ec3", "#542788", "#fee0b6", "#d8daeb", "#f7f7f7")) 

    } else {  # no color
      if (input$sort_x == "Year") {
        p <- ggplot(data=yearData(), aes_string(x= variables[input$sort_x], y= "value")) 
      } else {  # x axis == "Elevation"
        p <- ggplot(data=elevData(), aes_string(x= variables[input$sort_x], y= "value"))
      }
    }
    
    p <- p + geom_point() + xlab(input$xaxis) + theme_bw(base_size = 16)
    
    if (!is.null(input$sort_facet) && length(input$sort_facet) != 0) {  # add facets
      if(length(input$yaxis) == 1) {
        xFac <- "~"
      } else {
        xFac <- "Param ~"
      }
      p <- p + facet_grid(as.formula(paste(xFac, variables[input$sort_facet])), scales = "free", switch = "y")
    } else if (length(input$yaxis) != 1) {
      p <- p + facet_grid(Param ~ . , scales = "free", switch = "y")
    } 
    if (input$trendline) {  # add trendlines
      p <- p + geom_smooth(method="loess", se=FALSE)
    }
    if (length(input$yaxis) == 1) {
      p <- p + ylab(input$yaxis) 
    }
    p
  })
  
}
