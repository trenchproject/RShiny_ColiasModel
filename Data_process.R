# Data for location
allPoints <- read.csv("COpoints.csv")

# Filter only the necessary columns
points <- allPoints[, c("lon", "lat", "elev", "airpr")]

# All the biophysical data of Colias
array <- readRDS("lambda1_ccsm4.rds")

# To get desired data from this array, use array[year, cell, abs, gen, category], 
# where year is stored as 1-150 that correspond to 1950-2099,
#       cell corresponds to the row number of the points file,
#       abs is the absorptivity stored as 1-7 that corresponds to (0.4, 0.45. 0.5, 0.55, 0.6, 0.65, 0.7),
#       gen is the generation 1-3,
#       category is the metric of the data stored. 1: Population growth rate 
#                                                  2: Flight activity time
#                                                  3: Egg viability
#                                                  4: Body temperature


aseq= seq(0.4,0.7,0.05)
complete <- NA

# Make one big daraframe out of the array
for (year in 1:150) {
  for (abs in 1:7) {
    for (gen in 1:3) {
      lambda <- array[year, , abs, gen, 1]
      fat <- array[year, , abs, gen, 2]
      eggV <- array[year, , abs, gen, 3]
      Tb <- array[year, , abs, gen, 4]
      df <- cbind(points, lambda, fat, eggV, Tb) 
      colnames(df)[5:8] <- c("lambda", "FAT", "eggV", "temp")
      df$year <- year + 1949   # year 1 corresponds to 1950
      df$absorp <- aseq[abs]
      df$gen <- gen
      complete <- rbind(complete, df)
    }
  }
}

saveRDS(complete[-1,], file = "Colias_complete.rds")



#_____________________________________________________________________________________
# Processing elevation data from NOAA
elevation <- raster("ETOPO1_Ice_g_gmt4.grd")

e <- as(extent(-108.8125, -104.9375, 37.1875, 40.8125), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
r <- crop(elevation, e)
COelev <- rasterToPoints(r) %>% as.data.frame()


write.csv(x = COelev, "COelev.csv", row.names = FALSE)


#______________________________________________________________________________________
# Saving stamenmap
map <- get_stamenmap(bbox = c(left = -108.8125, right = -104.9375, bottom = 37.1875, top = 40.8125), zoom = 10, maptype = "terrain")
save(map, file = "my_map.RData")