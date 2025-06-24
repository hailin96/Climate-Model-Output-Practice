# Brockville Climate Analog

library(terra)
library(RNetCDF)
library(leaflet)

#Process June average temperature projection data from CMIP6

avgtemp <- rast(
  "Documents/Documents/SFU/NSERC/USRA 2/Data/Temp/tas_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012.nc"
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

#Locate Brockville by Downscaling from Global Scale

#Brockville is missing from CMIP6, so let's create a box around the Ottawa area.

west <- 284
east <- 285
south <- 44
north <- 45

brockville_area <- ext(west, east, south, north)

brockville_areabox <- crop(june_raster[[futurejune]], brockville_area)

brockville_futurejune <- values(brockville_areabox)

brockville_futurejune_avg <- mean(brockville_futurejune, na.rm=TRUE)


#Global June Avg Monthly Temp

presentjune <- which(format(june_dates, "%Y") %in% 2015:2035)
presentjune_rasters <- june_raster[[presentjune]]

presentjune_avg <- mean(presentjune_rasters, na.rm=TRUE)

plot(presentjune_avg, main = "Mean June Temperature (2015-2035)")


#Find the difference between global avg temp and Brockville avg temp for June

difference_raster <- abs(presentjune_avg - brockville_futurejune_avg)

plot(difference_raster, main = "Present Global Temperature Difference from Future Brockville")

#Finding Brockville's climate analog (the smallest difference)

analog_cell <- which.min(values(difference_raster))

analog_coords <- xyFromCell(difference_raster, analog_cell) #xyFromCell gets the center of each raster cell

print(analog_coords) #The resulting location is above the ocean near Cape Town, South Africa and below Madagascar!

#Build this out on a map

brockville_coord <- c(lng = -75.68, lat = 44.59)

analog_coord <- c(lng = 46.875, lat = -33.5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% #add a third party base map for me to work with
  addMarkers(lng = brockville_coord["lng"], lat = brockville_coord["lat"],
             label = "Brockville",
             popup = "Brockville, Ontario (Temp 2025)"
             ) %>%
  addMarkers(lng = analog_coord["lng"], lat = analog_coord["lat"],
             label = "Climate Analog",
             popup = "Somewhere near Cape Town. Brockville will bear the same temperature by 2050!"
             ) %>%
  addPolylines(lng = c(brockville_coord["lng"], analog_coord["lng"]),
               lat = c(brockville_coord["lat"], analog_coord["lat"]),
               color = "orange", weight = 2)

