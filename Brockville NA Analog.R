#Brockville Climate Analog in North America

install.packages("spData")
install.packages("cancensus")
install.packages("readr")
install.packages("geojsonsf")

library(terra)
library(RNetCDF)
library(leaflet)
library(spData)
library(sf)
library(cancensus)
library(readr)
library(geojsonsf)

#Process NetCDF 
#Data sourced from NSF NA-CORDEX Search

avgtemp <- rast(
  "Documents/Documents/SFU/NSERC/USRA 2/Data/Temp/tas.rcp85.GFDL-ESM2M.WRF.mon.NAM-22i.raw.nc"
  )

startyear <- 2015
nmonths <- nlyr(avgtemp) #dimensions of a spatraster
dates <- seq(as.Date(paste0(startyear, "-01-01")),
             by="month", length.out = nmonths
)

time(avgtemp) #dates are attached to raster layers

#Find the raster layers for June monthly temperatures

june_indices <- which(format(dates, "%m") == "06") #convert months to numeric and match them to 06.

june_raster <- avgtemp[[june_indices]] #return only the raster layers for 06 months

june_dates <- dates[june_indices] #return a list of 06 month from each year

futurejune <- which(format(june_dates, "%Y") %in% 2050:2060) #convert years to numeric and see if it is bw 2050 and 2060.

#Locate Brockville by Downscaling from the North American continent.

#Brockville is missing from WRF, so let's create a box around Brockville.

west <- -75.8
east <- -75.5
south <- 44.5
north <- 44.8

brockville_area <- ext(west, east, south, north)

brockville_areabox <- crop(june_raster[[futurejune]], brockville_area)

brockville_futurejune <- values(brockville_areabox)

brockville_futurejune_avg <- mean(brockville_futurejune, na.rm=TRUE)

#Mapping Brockville?

set_cancensus_api_key(
      "CensusMapper_f5c2090496b7235615b4c6c6dc2c9664", install = TRUE
      )

#we want to source the shape of Brockville from census data, but set a dummy vector (returns no data)
#CA16 is census 2016, CSD = Municipal Region, 3507015 is the municipal code.
brockvilleshape <- get_census(
  dataset = "CA16", regions = list(CSD = "3507015"), vectors = "V_CA16_1",use_cache=FALSE, geo_format = "sf"
  )

#fuse the coordinates from census to the coordinates that we set up previously for Brockville
brockvilleshape <- st_transform(brockvilleshape, crs((brockville_areabox)))
plot(brockville_areabox[[1]], main="Brockville's June Temperature in 2050")
plot(st_geometry(brockvilleshape), add=TRUE, border = "black", lwd = 0.9)

#North America June Avg Monthly Temp

presentjune <- which(format(june_dates, "%Y") %in% 2015:2035)
presentjune_rasters <- june_raster[[presentjune]]

presentjune_avg <- mean(presentjune_rasters, na.rm=TRUE)

#Mapping North America
northamerica <- world[world$continent == "North America", ]
plot(presentjune_avg, main = "Mean June Temperature (2015-2035)")
#use st_geometry instead of $geometry as terra tends to not recognize the column in sf object.
plot(st_geometry(northamerica), add=TRUE, border = "white", lwd=0.8)


#Find the difference between global avg temp and Brockville avg temp for June

difference_raster <- abs(presentjune_avg - brockville_futurejune_avg)

plot(difference_raster, main = "Present Global Temperature Difference from Future Brockville")

#Finding Brockville's climate analog (the smallest difference)

analog_cell <- which.min(values(difference_raster))

analog_coords <- xyFromCell(difference_raster, analog_cell) #xyFromCell gets the center of each raster cell

print(analog_coords) #The resulting location is Hoxie, Kansas, USA.

#Build this out on a map

brockville_coord <- c(lng = -75.68, lat = 44.59)

analog_coord <- c(lng = -102.125, lat = 38.875)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% #add a third party base map for me to work with
  addMarkers(lng = brockville_coord["lng"], lat = brockville_coord["lat"],
             label = "Brockville",
             popup = "Brockville, Ontario (Temp 2025)"
  ) %>%
  addMarkers(lng = analog_coord["lng"], lat = analog_coord["lat"],
             label = "Climate Analog",
             popup = "Hoxie, Kansas. Brockville will bear the same temperature by 2050!"
  ) %>%
  addPolylines(lng = c(brockville_coord["lng"], analog_coord["lng"]),
               lat = c(brockville_coord["lat"], analog_coord["lat"]),
               color = "orange", weight = 2)


