# Obtain Ontario Historical Observational Data from ECCC

#API query from climate summary (monthly observations) 

setwd("Documents/Documents/SFU/NSERC/USRA 2/Data/Temp")

library(httr)
library(readr)
library(dplyr)
library(readr)
library(purrr)
library(lubridate)
library(terra)

#API ERRORS --------------------------------
#URL
#filter for datetime and set limit = 10000 rows (Overall ON has 300k entries)
#limit = 100000 chokes up the server and prevents downloads
#server also crashes when you download 1990-2024 in one go
#already filtered for ON historical data
#download.file() is buggy and should use read_csv() instead
#ECCC is buggy with returning CSV, so we should rely on JSON
url <- "https://api.weather.gc.ca/collections/climate-monthly/items?datetime=1990-01-01/1991-01-01&f=csv&limit=1000"

response <- GET(url)

if (status_code(response) == 200) {
  json_data <- content(response, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(json_data, flatten = TRUE)
  
  climate_df <- parsed$features %>%
    select(
      id,
      date = properties.date,
      station_name = properties.station_name,
      province = properties.province,
      longitude = geometry.coordinates[[1]],
      latitude = geometry.coordinates[[2]],
      mean_temperature = properties.mean_temperature
    )
  
  glimpse(climate_df)
} else {
  cat("Error: ", status_code(response), "\n")
}

canada1990 <- read_csv(url, show_col_types=FALSE)

head(canada_1990)


url <- "https://api.weather.gc.ca/collections/climate-monthly/items?limit=5"

response <- GET(url)

if (status_code(response) == 200) {
  json_data <- content(response, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(json_data, flatten = TRUE)
  
  climate_df <- parsed$features %>%
    select(
      id,
      date = properties.date,
      station_name = properties.station_name,
      province = properties.province,
      longitude = geometry.coordinates[[1]],
      latitude = geometry.coordinates[[2]],
      mean_temperature = properties.mean_temperature
    )
  
  print(climate_df)
} else {
  cat("Error: ", status_code(response), "\n")
}


url<- "https://api.weather.gc.ca/collections/climate-monthly/items?limit=10&offset=0&FRE_PROVINCE_NAME=Ontario"

response <- GET(url)

url <- "https://api.weather.gc.ca/collections/climate-monthly/items?limit=2000&offset=0&FRE_PROVINCE_NAME=Ontario&f=csv"

response <- GET(url, user_agent("Mozilla/5.0"))

if (status_code(response) == 200) {
  # Save to file
  writeBin(content(response, "raw"), "ontario_climate_monthly.csv")
  
  # Or read it in directly
  climate_data <- read_csv("ontario_climate_monthly.csv")
  glimpse(climate_data)
} else {
  cat("Error: ", status_code(response), "\n")
} #error in csv due to lack of access to js servers from R

#MUST HAVE CHROME INSTALLED
install.packages("RSelenium")
library(RSelenium)

download_dir <- normalizePath("~/Documents/Documents/SFU/NSERC/USRA 2/Data/Temp/", mustWork = FALSE)
dir.create("~/Documents/Documents/SFU/NSERC/USRA 2/Data/Temp/Google Chrome for Testing", showWarnings = FALSE)

driverpath = "~/Documents/.../Google Chrome for Testing/chromedriver"
chrome <- list( 
  "download.default_directory" = download_dir,
  "download.promot_for_download" = FALSE,
  "download.directory_upgrade" = TRUE,
  "safebrowsing_enabled" = TRUE
  )

chrome_opts <- list(
  args = c("--headless", "--disable-gpu", "--window-size=1280,800", "--no-sandbox"),
  prefs = chrome
)

rD <- rsDriver(
  browser = "chrome",
  chromever = NULL,  #bypassing automatic version check
  port = 4545L,
  driverpath = "~/Documents/Documents/SFU/NSERC/USRA 2/Data/Temp/Google Chrome for Testing/chromedriver", 
  # adjust path if needed
  check = FALSE
)
remDr <- rD$client

#Use RSelenium to control the url website and click on download csv for you

remDr$navigate("https://api.weather.gc.ca/collections/climate-monthly/items?limit=2000&offset=0&FRE_PROVINCE_NAME=Ontario")

Sys.sleep(5)

csv_btn <- remDr$findElement(using = "link text", value = "CSV")
csv_btn$clickElement()

Sys.sleep(10)

#Manual download of csv START HERE------------------

ontario1 <- read_csv("climate-monthly-2.csv", show_col_types=FALSE)
head(ontario1)

ontario2 <- read_csv("climate-monthly-3.csv", show_col_types=FALSE)
ontario3 <- read_csv("climate-monthly-4.csv", show_col_types=FALSE)
ontario4 <- read_csv("climate-monthly-5.csv", show_col_types=FALSE)
ontario5 <- read_csv("climate-monthly-6.csv", show_col_types=FALSE)
ontario6 <- read_csv("climate-monthly-7.csv", show_col_types=FALSE)

file_list <- list(
  "climate-monthly-2.csv",
  "climate-monthly-3.csv",
  "climate-monthly-4.csv",
  "climate-monthly-5.csv",
  "climate-monthly-6.csv",
  "climate-monthly-7.csv"
)

#return a list that fills file_list with all the csvs just downloaded
#found out that R didn't interpret all column titles as characters
ontario_list <- lapply(file_list, read_csv, show_col_types = FALSE)

#map() is a part of purrr
#loops over each csv in the file_list
#applies the function read_csv() to each csv
#every column of csv must be read as a character by default
#returns a list of events
ontario_list <- map(file_list, ~ read_csv(.x, show_col_types = FALSE, col_types = cols(.default = "c")))
ontario_tot <- bind_rows(ontario_list)

head(ontario_tot)

#filter for only June data by parsing dates
ontario_tot <- ontario_tot |>
  mutate(date = paste0(LOCAL_DATE, "-01"), #add a day to the month to make it into yyyy-mm-dd
         date = ymd(date),
         year = year(date),
         month = month(date)) |>
  filter(month == 6)

#filter for 1990-2024
ontario_tot <- ontario_tot |>
  filter(year >= 1990, year <= 2024)

#I want to group by station name and year (June is already filtered)
#convert mean temperature, latitude and longitude to numeric
ontario_tot$MEAN_TEMPERATURE <- as.numeric(ontario_tot$MEAN_TEMPERATURE)
ontario_tot$LATITUDE <- as.numeric(ontario_tot$LATITUDE)
ontario_tot$LONGITUDE <- as.numeric(ontario_tot$LONGITUDE)

ontario_tot_june <- ontario_tot |> 
  group_by(STATION_NAME, year) |> #filter by station name instead of ID, cleaner look
  summarize(mean_temp = mean(MEAN_TEMPERATURE, na.rm=TRUE),
            latitude = first(LATITUDE), #the latitude is the same for every station, so take the first
            longitude = first(LONGITUDE), #same as above
            .groups = "drop" #only one way of grouping is used while the other is dropped
  )

#let's make each time step into a single raster
years <- sort(unique(ontario_tot_june$year)) #make sure years do not repeat and get a list of years
raster_list <- list()

ontario_points <- vect(
  ontario_tot_june, geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

base_ontario_raster <- rast(ext(ontario_points), resolution = 0.1) #set template raster to have 0.1 degree cells
crs(base_ontario_raster) <- crs(ontario_points)

#rasterize each time step
for (yr in years) { #years is a vector, and yr is an entry in the vector
  
  #filter for each year's June temp
  yearly_temp <- ontario_tot_june |> filter(year == yr)
  
  #skip the data (next) if there are rows of NA for each station
  if (nrow(yearly_temp) == 0) next
  
  #covert points into SpatVectors
  ontario_points <- vect(yearly_temp, geom = c("longitude", "latitude"), 
                         crs = "EPSG:4326")
  
  #rasterize that year's points onto the base raster
  r <- rasterize(ontario_points, base_ontario_raster, field = "mean_temp")
  
  #name the raster layer as june_yyyy
  names(r) <- paste0("june_", yr)
  
  raster_list[[as.character(yr)]] <- r #store each year's raster in a list
}

ontario_single_raster <- rast(raster_list) #stacking raster

plot(ontario_single_raster[[1]], main = "Single Layer June Monthly Temp of Ontario 1990")

ontario_shape <- vect("Ontario_Simple_Boundary/Ontario Simple Boundary.shp")

plot(ontario_single_raster[[1]], main = "Single Layer June Monthly Temp of Ontario 1990")
plot(ontario_shape, add = TRUE, border = "black")

plot(ontario_single_raster[[10]], main = "Single Layer June Monthly Temp of Ontario 2000")
plot(ontario_shape, add = TRUE, border = "black")

