#Downloading ECCC Historical Temperature Data for Ontario

#Using the Open Geospatial Consortium of MSC Open Data for API

#Adjusted and Homogenized Canadian Climate Data (AHCCD) Monthly observational data.

setwd("Documents/Documents/SFU/NSERC/USRA 2/Data/Temp")

library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(terra)

#setup API that works:
#URL
#filter for datetime and set limit = 10000 rows
url <- "https://api.weather.gc.ca/collections/ahccd-monthly/items?datetime=1990-01-01/2024-12-31&f=csv&limit=10000"

#quiet suppresses warnings and other popups
download.file(url, destfile = "ahccd_1990_2024.csv", quiet = TRUE)

canada_temp <- read_csv("ahccd_1990_2024.csv", show_col_types = FALSE)
head(canada_temp)

ontario_temp <- canada_temp |> filter(province__province == "ON")

#Parse the date to year, month

ontario_temp <- ontario_temp |>
  mutate(date = as.Date(date)) |> #error in date format
  mutate(year = year(date), month = month(date)) |>
  filter(month == 6)

#fix the date format
ontario_temp <- ontario_temp |>
  mutate(date = paste0(date, "-01"), #add a day to the month to make it into yy-mm-dd
        date = ymd(date),
        year = year(date),
        month = month(date)) |>
  filter(month == 6)

#I want to group by station id and year (June is already filtered)
ontario_temp_june <- ontario_temp |> #ignore lat and lon as each station ID is unique
  group_by(station_id__id_station, year) |> #there are a lot of NA mean temps
  summarize(mean_june_temp = mean(temp_mean__temp_moyenne, na.rm=TRUE),
            latitude = first(lat__lat), #the latitude is the same for every station, so take the first
            longitude = first(lon__long), #same as above
            .groups = "drop" #only one way of grouping is used while the other is dropped
            )

#Let's make each time step into a single raster
years <- sort(unique(ontario_temp_june$year)) #make sure years do not repeat and get a list of years
raster_list <- list()

ontario_points <- vect(
                        ontario_temp_june, geom = c("longitude", "latitude"),
                        crs = "EPSG:4326"
                        )

base_ontario_raster <- rast(ext(ontario_points), resolution = 0.1) #set template raster to have 0.1 degree cells
crs(base_ontario_raster) <- crs(ontario_points)

#Rasterize each time step
for (yr in years) { #years is a vector, and yr is an entry in the vector
  
  #filter for each year's June temp
  yearly_temp <- ontario_temp_june |> filter(year == yr)
  
  #skip the data (next) if there are rows of NA for each station
  if (nrow(yearly_temp) == 0) next
  
  #covert points into SpatVectors
  ontario_points <- vect(yearly_temp, geom = c("longitude", "latitude"), 
                         crs = "EPSG:4326")
  
  #rasterize that year's points onto the base raster
  r <- rasterize(ontario_points, base_ontario_raster, field = "mean_june_temp")
  
  #name the raster layer as june_yyyy
  names(r) <- paste0("june_", yr)
  
  raster_list[[as.character(yr)]] <- r #store each year's raster in a list
}

ontario_single_raster <- rast(raster_list) #stacking raster

plot(ontario_single_raster[[1]], main = "Single Layer June Monthly Temp of Ontario 1990")

plot(ontario_single_raster, main = "June Monthly Temp of Ontario (1990-2024)") #not a good presentation

ontario_shape <- vect("Ontario_Simple_Boundary/Ontario Simple Boundary.shp")

plot(ontario_single_raster[[1]], main = "Single Layer June Monthly Temp of Ontario 1990")
plot(ontario_shape, add = TRUE, border = "black")

plot(ontario_single_raster[[20]], main = "Single Layer June Monthly Temp of Ontario 2010")
plot(ontario_shape, add = TRUE, border = "black")

#Aggregating the rasters?
nyears <- nlyr(ontario_single_raster) #how many layers do we have?
agg_list <- list() #empty list

#Calculate cumulative mean (ex. at year = 2015, we want the mean temp from 1990-2015)
for (i in 1:nyears) {
  
  #take the mean temp of years 1 to i, discarding NA
  r_mean <- mean(ontario_single_raster[[1:i]], na.rm = TRUE)
  
  #assigning a name to each new raster that's the same as the single layer ones
  names(r_mean) <- names(ontario_single_raster)[i]
  
  #add each year's agg raster into the list
  agg_list[[i]] <- r_mean
}

ontario_agg_raster <- rast(agg_list) #stacking rasters

#errors because the values are entirely NA
plot(ontario_agg_raster[10], main = "Aggregated June Monthly Temp of Ontario 2000")

