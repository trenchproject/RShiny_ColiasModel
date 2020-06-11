Colias <- readRDS("Colias_complete.rds")

shinyServer <- function(input, output) {
  
  
  data <- reactive({
    df <- Colias %>% filter(year %in% input$year & elev > input$elev[1] & elev < input$elev[2])
  })
    

  
  output$mymap <- renderPlot({
    
    #ggmap
    bbox <- ggmap::make_bbox(lon, lat, dat2, f = 0.1)
    map_loc <- get_map(location = bbox, source = 'osm', maptype = 'terrain')
    
    map1 <- ggmap(map_loc, extent='device', base_layer=ggplot(dat2, aes(x=lon, y=lat)))
    
    co.map<- map1 + geom_raster(aes(fill = lambda), alpha=0.5)+ coord_cartesian()
    co.map
    
  })
  
  # output$plot <- renderPlot({
  #   p <- ggplot(data=data(), aes(x=elev, y=value, color=as.character(year)))+geom_point()+facet_grid(fitcomp~absorp, scales="free")+
  #     ylab("fitness component")+xlab("elevation (m)")+theme_bw(base_size = 16) +
  #     theme(legend.position="bottom") + scale_color_manual(name = "year", values = c("orange", "blue"))
  #     
  #   if (input$trendline) {
  #     p + geom_smooth(method="loess", se=FALSE, aes(group=year), col = "black") 
  #   } else {
  #     p
  #   }
  # })
  
}
