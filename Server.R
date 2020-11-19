source("functions.R", local = TRUE)
source("cicerone.R", local = TRUE)

Colias <- readRDS("Colias_complete.rds")
COelev <- read.csv("COelev.csv")
shortName = c("Population growth rate" = "lambda", "Flight activity time (s)" = "FAT", "   Egg viability (%)    " = "eggV", "Body temperature (°C)" = "temp")
variables <- c("Year" = "year", "Absorptivity" = "absorp", "Generation" = "gen", "Elevation" = "elev")
elevCat <- c("1392 ~ 1700", "1701 ~ 2000", "2001 ~ 2300", "2301 ~ 2600", "2601 ~ 2900", "2901 ~ 3198")
load(file = "my_map.RData")

x <- 1
t <- 0

while(t == 0) {
  x <- x + 1
  tmax <- ncdc(datasetid = 'GHCND', 
               stationid = "GHCND:USC00051959", 
               token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
               startdate = Sys.Date() - x - 6, 
               enddate = Sys.Date() - x, 
               datatypeid = "TMAX")
  t <- dim(tmax$data)[1]
}

tmin <- ncdc(datasetid = 'GHCND', 
             stationid = "GHCND:USC00051959", 
             token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
             startdate = Sys.Date() - x - 6, 
             enddate = Sys.Date() - x, 
             datatypeid = "TMIN")



# tmax <- ncdc(datasetid = 'GHCND', 
#              stationid = "GHCND:USC00051959", 
#              token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
#              startdate = Sys.Date() - 8, 
#              enddate = Sys.Date() - 2, 
#              datatypeid = "TMAX")
# 
# tmin <- ncdc(datasetid = 'GHCND', 
#              stationid = "GHCND:USC00051959", 
#              token = "MpEroBAcjEIOFDbJdJxErtjmbEnLVtbq", 
#              startdate = Sys.Date() - 8, 
#              enddate = Sys.Date() - 2, 
#              datatypeid = "TMIN")

temp <- as.data.frame(tmax$data) %>% dplyr::select(date, value) %>% 
  set_colnames(c("date", "Tmax")) %>% 
  cbind(as.data.frame(tmin$data) %>% dplyr::select(value) %>% set_colnames("Tmin"))

temp[c(2,3)] <- temp[c(2,3)] / 10


temp$Year <- NA
temp$Month <- NA
temp$Day <- NA
for (row in 1:dim(temp)[1]) {
  temp$Year[row] <- strsplit(as.character(temp$date), "-")[[row]][1]
  temp$Month[row] <- strsplit(as.character(temp$date), "-")[[row]][2]
  temp$Day[row] <- strsplit(as.character(temp$date), "-")[[row]][3] 
  temp$Day[row] <- strsplit(as.character(temp$Day), "T")[[row]][1]
}

hourlyTemp <- make_hourly_temps(mean(Colias$lat), temp)
hourlyTemp <- hourlyTemp[, c(8:31)] %>% t() %>% as.data.frame()

hourlyTemp$Hour <- c(0:23)
colnames(hourlyTemp) <- c(as.character(seq.Date(from = Sys.Date() - x - 6, to = Sys.Date() - x - 8 + length(hourlyTemp), by = "1 day")), "Hour")

combined <- melt(hourlyTemp, id.vars = "Hour")

for(row in 1:dim(combined)[1]) {
  combined$dateHour[row] <- paste0(combined$variable[row], " ", combined$Hour[row], ":00")
}
hours <- as.POSIXct(combined$dateHour, format="%Y-%m-%d %H:%M")
combined$dateHour <- hours



shinyServer <- function(input, output, session) {
  
  observeEvent(input$tour_map, guide_map$init()$start())
  
  observeEvent(input$tour_plot, guide_plot$init()$start())
  
  
  output$plot_intro <- renderPlot({
    seq <- rep(seq.Date(from = Sys.Date() - x - 6, to = Sys.Date() - x - 8 + length(hourlyTemp), by = "1 day"), each = 24)
    doy = day_of_year(seq)
    hour = rep(0:23, times = length(hourlyTemp) - 1)
    z <- zenith_angle(doy = doy, lat = mean(Colias$lat), lon = mean(Colias$lon), hour = hour)
    shade <- rep(FALSE, length(z))
    for (i in 1:length(z)) {
      if (z[i] == 90) {
        shade[i] <- TRUE
      }
    }
    
    if (input$weather == "Clear") {
      solrad <- 8000
    } else if (input$weather == "Partly cloudy") {
      solrad <- 5000
    } else {
      solrad <- 2000
      shade = FALSE
    }
    
    rad <- diurnal_radiation_variation(doy, solrad = solrad, hour = hour, lon = mean(Colias$lon), lat = mean(Colias$lat))
    rad[rad < 0] <- 0

    colors <- c("Environmental" = "black", "Operative" = "blue")
    
    Tb <- mapply(Tb_butterfly, 
                 combined$value, combined$value+5, Tg_sh = combined$value-5, u = 1, H_sdir = rad, H_sdif = 400, z = 30, D=0.36, delta=0.82, alpha=as.numeric(input$abs_intro), r_g=0.3, shade = shade)
    
    ggplot() + geom_line(aes(x = combined$dateHour, y = combined$value, color = "Environmental"), size = 1.3) + geom_line(aes(x = combined$dateHour, y = Tb, color = "Operative"), size = 1.3) +
      xlab("Day") + ylab("Temperature (°C)") + theme_bw() + ggtitle("C. eriphyle body temperatures in the past week") +
      scale_color_manual(values = colors) +
      theme(plot.title = element_text(size = 18), axis.text = element_text(size = 13), axis.title = element_text(size = 16), legend.text = element_text(size = 13), 
            legend.title = element_blank())
  })
  
  # widgets to show
  output$widgetInput <- renderUI({
    validate(
      need(input$sort_x == "Year" || input$sort_x == "Elevation", "")
    )
    elevInput <- sliderInput("elev", "Elevation (m)", min = min(Colias$elev), max = max(Colias$elev), value = c(min(Colias$elev), max(Colias$elev)))
    
    if(input$sort_x == "Elevation") {
      yearInput <- sliderInput("yearPlot", "Year", min = 1950, max = 2099, value = 2020)
      if (!is.null(input$sort_col) || !is.null(input$sort_facet)) {
        if (input$sort_col == "Year" || length(input$sort_col) != 0 || input$sort_facet == "Year" || length(input$sort_facet) != 0) {
          yearInput <- selectInput("yearPlot", "Year", choices = c(1950:2099), selected = 2020, multiple = TRUE)
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
    list(elevInput, yearInput)
  })

  # Data filter for map
  
  # data <- reactive({
  #   df <- Colias %>% filter(year == input$year & absorp %in% input$abs & gen %in% input$gen) %>% na.omit()
  #   df[,c("lat", "lon", paste(shortName[input$metric]))] %>% na.omit()
  # })
  # 
  # 
  # output$mymap <- renderLeaflet({
  #   filtered <- data()
  #   coordinates(filtered)=~lon+lat
  #   proj4string(filtered)=CRS("+init=epsg:4326") # set it to lat-long
  #   gridded(filtered) = TRUE
  # 
  #   dfRaster <- raster(filtered)
  #   mapStates = map("state", fill = TRUE, plot = FALSE)
  #   pal <- colorNumeric(c("Red", "Orange"), values(dfRaster),
  #                       na.color = "transparent")
  #   
  #   leaflet(data = mapStates) %>%
  #     addProviderTiles(providers$Stamen.Terrain) %>%
  #     setView(lng = (min(Colias$lon) + max(Colias$lon)) / 2, lat = (min(Colias$lat) + max(Colias$lat)) / 2, zoom = 6) %>%
  #     addRasterImage(dfRaster, opacity = 0.7, colors = pal) %>%
  #     addLegend(pal = pal, values = values(dfRaster), title = input$metric[1], position = "bottomright")
  # })
  
  # data filter for ggplot
  data_gg <- reactive({
    df <- Colias %>% filter(year == input$year & absorp %in% input$abs & gen %in% input$gen) %>% na.omit()
    df[,c("lat", "lon", "elev", "absorp", "gen", paste(shortName[input$metric]))]
    
    # df[,c("lat", "lon", "absorp", "gen", paste(shortName[input$metric]))] %>% na.omit() %>% 
    #   gather(Param, value, shortName[input$metric])

  })
  
  
  output$info <- renderText({
    validate(
      need(input$plot_click, "Click on the map to see detailed values"),
      need(!input$layer || length(input$metric) < 2, "Select one metric")
    )
    if (input$layer) {
      elev <- round(colMeans(nearPoints(data_gg(), input$plot_click, xvar = "lon", yvar = "lat")["elev"]), digits = 0)
      value <- round(colMeans(nearPoints(data_gg(), input$plot_click, xvar = "lon", yvar = "lat")[shortName[input$metric]]), digits = 2)
      paste0("Elevation: ", elev, " m\n", trimws(input$metric, which = "both"), ": ", value)
    } else {
      elev <- round(colMeans(nearPoints(COelev, input$plot_click, xvar = "x", yvar = "y")["z"]), digits = 0)
      paste("Elevation:", elev, "m")
    }
  })
  
  defaultPlot <- function(param, axis = FALSE) {
    p <- ggmap(map) +
      xlab("Longitude (°)") + ylab("Latitude (°)") + theme_bw( ) +
      geom_raster(data = data_gg(), aes_string(x = "lon", y = "lat", fill = shortName[param])) +
      coord_quickmap(xlim = c(min(data_gg()$lon), max(data_gg()$lon)), ylim = c(min(data_gg()$lat), max(data_gg()$lat)), expand = TRUE) +
      scale_fill_gradientn(name = param, colors = alpha(viridis(12), 0.7) , na.value = "white") + facet_grid(as.formula(paste("~", input$facet))) +
      theme(strip.text = element_text(size = 12)) + 
      #theme(plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "#F5F5F5")) +
      theme(plot.title = element_text(size = 18, hjust = 0.5), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
            legend.title = element_text(size = 12)) 
    if (!axis) {
      p <- p + theme(axis.title.x = element_blank())
    }
    if (input$labels) {
      p <- p + geom_text(aes(x = -104.9903, y = 39.7392, label = "·"), size = 12) + geom_text(aes(x = -105.2903, y = 39.6192, label = "Denver"), size = 5) +
        geom_text(aes(x = -108.5506, y = 39.0639, label = "·"), size = 12) + geom_text(aes(x = -108.3906, y = 39.2639, label = "Grand \njunction"), size = 4) +
        geom_text(aes(x = -105.2705, y = 40.0150, label = "·"), size = 12) + geom_text(aes(x = -105.2705, y = 40.1350, label = "Boulder"), size = 5) +
        geom_text(aes(x = -107.8762, y = 38.4783, label = "·"), size = 12) + geom_text(aes(x = -107.8762, y = 38.3683, label = "Montrose"), size = 4) +
        geom_text(aes(x = -106.8175, y = 39.1911, label = "·"), size = 12) + geom_text(aes(x = -106.8175, y = 39.2911, label = "Aspen"), size = 4)
        
    }
    return(p)
  }
  
  height <- reactive({
    if(input$layer) {
      n <- length(input$metric) * 150 + 250
    } else {
      n <- 400
    }
  })
  
  output$mymap_gg <- renderPlot({
    validate(
      need(input$metric, "Select a metric to map")
    )
    if (input$layer) {   # Data
      n <- length(input$metric)
      first <- defaultPlot(input$metric[1])
      last <- defaultPlot(input$metric[n], TRUE)
      if(n == 2) {
        grid.arrange(first, last, heights = c(1, 1.05))
      } else if (n == 3) {
        grid.arrange(first, defaultPlot(input$metric[2]), last, heights = c(1, 1, 1.05))
      } else if (n == 4) {
        grid.arrange(first, defaultPlot(input$metric[2]), defaultPlot(input$metric[3]), last, heights = c(1, 1, 1, 1.05))
      } else {
        last
      }
    } else {    # Elevation
      map <- ggmap(map) +
        xlab("Longitude (°)") + ylab("Latitude (°)") +
        theme(plot.title = element_text(size = 18, hjust = 0.5), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 12)) 
      # ggplot() + geom_raster(data = COelev, aes(x = x, y = y, fill = z)) + xlab("Longitude (°)") + ylab("Latitude (°)") +
      #   coord_quickmap(xlim = c(-108.8125, -104.9375), ylim = c(37.1875, 40.8125), expand = TRUE) +
      #   scale_fill_gradientn(name = "Elevation (m)", colors = heat.colors(10), na.value = "white") + 
      #   theme(strip.text = element_text(size = 12)) + 
      #   #theme(plot.background = element_rect(fill = "#F5F5F5"), panel.background = element_rect(fill = "#F5F5F5")) +
      #   theme(plot.title = element_text(size = 18, hjust = 0.5), axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.text = element_text(size = 12),
      #         legend.title = element_text(size = 12)) 

      if (input$labels) {
        map <- map + geom_text(aes(x = -104.9903, y = 39.7392, label = "·"), size = 12) + geom_text(aes(x = -105.2903, y = 39.6192, label = "Denver"), size = 5) +
          geom_text(aes(x = -108.5506, y = 39.0639, label = "·"), size = 12) + geom_text(aes(x = -108.3906, y = 39.2639, label = "Grand \njunction"), size = 4) +
          geom_text(aes(x = -105.2705, y = 40.0150, label = "·"), size = 12) + geom_text(aes(x = -105.2705, y = 40.1350, label = "Boulder"), size = 5) +
          geom_text(aes(x = -107.8762, y = 38.4783, label = "·"), size = 12) + geom_text(aes(x = -107.8762, y = 38.3683, label = "Montrose"), size = 4) +
          geom_text(aes(x = -106.8175, y = 39.1911, label = "·"), size = 12) + geom_text(aes(x = -106.8175, y = 39.2911, label = "Aspen"), size = 4)
      }
      map
      
    }
    
        
  }, height = height)

  
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
  
  output$plot <- renderPlot({
    validate(
      need(input$sort_x == "Year" || input$sort_x == "Elevation", "Drag \"Year\" or \"Elevation\" to x axis"),
      need(input$yaxis, "Select a metric for y axis")
    )
    if (!is.null(input$sort_col) && length(input$sort_col) != 0) {  # with color
      
      color <- paste("factor(", variables[input$sort_col], ")")

      if (input$sort_x == "Year") {
        if (input$sort_col == "Elevation") {
          p <- ggplot(data=yearData(), aes_string(x= variables[input$sort_x], y= "value", col = "elevRange")) + xlab("Year")
        } else {  # x axis = year, color = gen or absorp
          p <- ggplot(data=yearData(), aes_string(x= variables[input$sort_x], y= "value", col = color)) + xlab("Year")
        }
      } else {  # x axis = Elevation, color = anything
        p <- ggplot(data=elevData(), aes_string(x= variables[input$sort_x], y= "value", col = color)) + xlab("Elevation (m)")
      }
      p <- p + scale_color_manual(name = input$sort_col, values = c("#b35806", "#f1a340", "#998ec3", "#542788", "#fee0b6", "#d8daeb", "#f7f7f7")) 

    } else {  # no color
      if (input$sort_x == "Year") {
        p <- ggplot(data=yearData(), aes_string(x= variables[input$sort_x], y= "value")) + xlab("Year")
      } else {  # x axis == "Elevation"
        p <- ggplot(data=elevData(), aes_string(x= variables[input$sort_x], y= "value")) + xlab("Elevation (m)")
      }
    }
    
    p <- p + geom_point() + theme_bw(base_size = 16)
    
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
  
  output$absInput <- renderUI({
    if (input$facet == "gen") {
      selectInput("abs", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), multiple = FALSE, selected = 0.4)
    } else {
      selectInput("abs", "Wing absorptivity", choices = seq(0.4, 0.7, 0.05), multiple = TRUE, selected = 0.4)
    }
  })

  output$genInput <- renderUI({
    if (input$facet == "gen") {
      selectInput("gen", "Generation", choices = c(1, 2, 3), multiple = TRUE, selected = 1)
    } else {
      selectInput("gen", "Generation", choices = c(1, 2, 3), multiple = FALSE, selected = 1)
    }
  })

}
