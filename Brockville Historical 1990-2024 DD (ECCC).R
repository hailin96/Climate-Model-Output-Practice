#Brockville Historical 1990-2025 MSC DD-Alpha

#The MSC Datamart is located at https://dd.weather.gc.ca/.

#Instead of going through API, I'm going to use this datamart.

setwd("Documents/Documents/SFU/NSERC/USRA 2/Data/Temp")

library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(terra)

url <- "https://dd.weather.gc.ca/climate/observations/monthly/csv/ON/"
page <- read_html(url)

file_links <- page %>%
  html_elements("a") %>%
  html_attr("href") %>%
  str_subset("\\.csv$")

csv_urls <- paste0(url, file_links)

#extract year from file name (e.g., 2015 from climate_monthly_ON_6010400_2015_P1M.csv)
years <- str_extract(file_links, "_(\\d{4})_") %>% str_remove_all("_") %>% as.integer()

#keep only files from 1990–2025
file_links_filtered <- file_links[years >= 1990 & years <= 2025]

csv_urls <- paste0(base_url, file_links_filtered)

message("Total files for 1990–2025: ", length(csv_urls))

#download all the urls between years 1990 to 2025
for (i in seq_along(csv_urls)) {
  url <- csv_urls[i]
  file_name <- basename(url)
  dest_path <- file.path("ontario_station_csvs", file_name)
  
  message("Downloading: ", file_name)
}

csv_files <- list.files("ontario_station_csvs", pattern = "\\.csv$", full.names = TRUE)

#map() is a part of purrr
#loops over each csv in the file_list
#applies the function read_csv() to each csv
#every column of csv must be read as a character by default
#returns a list of events
#make sure you're not using read.csv() but read_csv()
csv_list <- map(csv_files, ~ read_csv(.x, show_col_types = FALSE, col_types = cols(.default = "c")))

climate_ontario_all <- bind_rows(csv_list)

head(climate_ontario_all)

#filter for only June data 
#years and month are already parsed in original download
climate_ontario_all <- climate_ontario_all |>
  filter(Month == 6)

#I want to convert some characters into numeric
climate_ontario_all$Longitude <- as.numeric(climate_ontario_all$Longitude)
climate_ontario_all$Latitude <- as.numeric(climate_ontario_all$Latitude)
climate_ontario_all$Tm <- as.numeric(climate_ontario_all$Tm)
str(climate_ontario_all$Year)
climate_ontario_all$Year <- as.numeric(climate_ontario_all$Year)

#filter for 2010-2025
climate_ontario_all <- climate_ontario_all |>
  filter(Year >= 2010, Year <= 2025)

#I want to group by year and station/city name
climate_ontario_all_t <- climate_ontario_all |> 
  group_by(`Station Name`, Year) |> #filter by station name instead of ID, cleaner look
  summarize(mean_temp = mean(Tm, na.rm=TRUE),
            latitude = first(Latitude), #the latitude is the same for every station, so take the first
            longitude = first(Longitude), #same as above
            .groups = "drop" #only one way of grouping is used while the other is dropped
  )

#make each time step into a single raster
years <- sort(unique(climate_ontario_all_t$Year)) #make sure years do not repeat and get a list of years
raster_list <- list()

ontario_points <- terra::vect(
  climate_ontario_all_t, geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

base_ontario_raster <- terra::rast(ext(ontario_points), resolution = 0.1) #set template raster to have 0.1 degree cells
crs(base_ontario_raster) <- crs(ontario_points)

#rasterize each time step
for (yr in years) { #years is a vector, and yr is an entry in the vector
  
  #filter for each year's June temp
  yearly_temp <- climate_ontario_all_t |> filter(Year == yr)
  
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

plot(ontario_single_raster[[1]], main = "Single Layer June Monthly Temp of Ontario 2010")

ontario_shape <- vect("Ontario_Simple_Boundary/Ontario Simple Boundary.shp")

plot(ontario_single_raster[[10]], main = "Single Layer June Monthly Temp of Ontario 2020")
plot(ontario_shape, add = TRUE, border = "black")

# Interpolation -----------------------
#using thin plate spline, which is good for temperature graphics, to fit scattered points w/o even distribution
#has good fit to observed data points
#sum of radial basis functions centered on data points
#minimize COST = GOODNESS OF FIT + (lambda * SMOOTHNESS PENALTY)
#didn't work, try again later

#try out another interpolation method: inverse density weighted 
#IDW interpolation ---------------------------
library(gstat)
library(sf)
library(sp)
library(raster)

ontario_points_sf <- st_as_sf(ontario_points)
ontario_points_sp <- as(ontario_points_sf, "Spatial") #converting to spatialpointsdataframe

ontario_idw <- gstat::gstat(formula = `mean temp` ~ 1,
                            data = ontario_points_sp,
                            nmax = 7,
                            set = list(idp = 2.0)
                            )

#we need to make a grid that fits Ontario's shape

obbox <- st_bbox(ontario_shape)

grid <- expand.grid(
  x = seq(obbox["xmin"], obbox["xmax"], by = 0.1),
  y = seq(obbox["ymin"], obbox["ymax"], by = 0.1)
)

coordinates(grid) <- ~x + y

gridded(grid) <- TRUE

proj4string(grid) <- proj4string(ontario_points_sp)

#there are NA values in mean_temp so clean that up
ontario_points_sp_clean <- ontario_points_sp[!is.na(ontario_points_sp$mean_temp), ]

idw_result <- gstat::idw(
                        mean_temp ~ 1, 
                        locations = ontario_points_sp_clean, 
                        newdata = grid, 
                        idp = 2.0
                        )

idw_raster <- raster::raster(idw_result)

ontario_shape_sp <- as(ontario_shape, "Spatial")

#make sure the raster fits the ontario shape
masked_raster <- raster::mask(idw_raster, ontario_shape_sp)

idw_df <- as.data.frame(raster::rasterToPoints(masked_raster))

colnames(idw_df) <- c("x", "y", "temperature")

#plot out the masked raster -> data frame with ggplot()

idw_temp_df <- ggplot()+
  geom_tile(data = idw_df, aes(x = x, y = y, fill = temperature)) +
  geom_sf(data = ontario_shape_sf, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(option = "plasma", name = "°C") +
  coord_sf(expand = FALSE) +
  labs(title = "IDW-Interpolated June Mean Temperature",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
idw_temp_df

#Precipitation Interpolation -----------------------

csv_files <- list.files("ontario_station_csvs", pattern = "\\.csv$", full.names = TRUE)

csv_list <- map(csv_files, ~ read_csv(.x, show_col_types = FALSE, col_types = cols(.default = "c")))

climate_ontario_all <- bind_rows(csv_list)

climate_ontario_all <- climate_ontario_all |>
  filter(Month == 6)

#I want to convert some characters into numeric
climate_ontario_all$Longitude <- as.numeric(climate_ontario_all$Longitude)
climate_ontario_all$Latitude <- as.numeric(climate_ontario_all$Latitude)
climate_ontario_all$Tm <- as.numeric(climate_ontario_all$Tm)
climate_ontario_all$Year <- as.numeric(climate_ontario_all$Year)
climate_ontario_all$P <- as.numeric(climate_ontario_all$P)

climate_ontario_all <- climate_ontario_all |>
  filter(Year >= 2010, Year <= 2025)

#precipitation (mm) variable

#I want to group by year and station/city name
climate_ontario_all_p <- climate_ontario_all |> 
  group_by(`Station Name`, Year) |> #filter by station name instead of ID, cleaner look
  summarize(precipitation = mean(P, na.rm=TRUE),
            latitude = first(Latitude), #the latitude is the same for every station, so take the first
            longitude = first(Longitude), #same as above
            .groups = "drop" #only one way of grouping is used while the other is dropped
  )

years <- sort(unique(climate_ontario_all_p$Year)) #make sure years do not repeat and get a list of years
raster_list_p <- list()

ontario_points_p <- vect(
  climate_ontario_all_p, geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

base_ontario_raster <- rast(ext(ontario_points_p), resolution = 0.1) #set template raster to have 0.1 degree cells
crs(base_ontario_raster) <- crs(ontario_points_p)


#rasterize each time step
for (yr in years) { #years is a vector, and yr is an entry in the vector
  
  #filter for each year's June temp
  yearly_prec <- climate_ontario_all_p |> filter(Year == yr)
  
  #skip the data (next) if there are rows of NA for each station
  if (nrow(yearly_prec) == 0) next
  
  #covert points into SpatVectors
  ontario_points <- vect(yearly_prec, geom = c("longitude", "latitude"), 
                         crs = "EPSG:4326")
  
  #rasterize that year's points onto the base raster
  r <- rasterize(ontario_points, base_ontario_raster, field = "precipitation")
  
  #name the raster layer as june_yyyy
  names(r) <- paste0("june_", yr)
  
  raster_list_p[[as.character(yr)]] <- r #store each year's raster in a list
}

#idw interpolation

ontario_points_p_sf <- st_as_sf(ontario_points_p)
ontario_points_p_sp <- as(ontario_points_p_sf, "Spatial") #spatialpointsdataframe



ontario_p_idw <- gstat::gstat(formula = `precipitation` ~ 1,
                            data = ontario_points_p_sp,
                            nmax = 7,
                            set = list(idp = 2.0)
)

#cleaning up NA
ontario_points_p_sp_clean <- ontario_points_p_sp[!is.na(ontario_points_p_sp$precipitation), ]

idw_p_result <- gstat::idw(
  `precipitation` ~ 1,
  locations = ontario_points_p_sp_clean,
  newdata = grid,
  idp = 2.0
)

#convert idw result into a raster
p_raster <- raster(idw_p_result)

#mask raster from ontario shape
p_raster <- mask(p_raster, ontario_shape_sp)

idw_p_df <- as.data.frame(raster::rasterToPoints(p_raster))

colnames(idw_p_df) <- c("x", "y", "precipitation")

#plot out the masked raster -> data frame with ggplot()

idw_prec_df <- ggplot()+
  geom_tile(data = idw_p_df, aes(x = x, y = y, fill = precipitation)) +
  geom_sf(data = ontario_shape_sf, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(option = "cividis") +
  coord_sf(expand = FALSE) +
  labs(title = "IDW-Interpolated June Mean Precipitation",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
idw_prec_df

#Combine the IDW Interpolations -----------------

climate_idw <- stack(masked_raster, p_raster)
names(climate_idw) <- c("temperature", "precipitation")

climate_rast <- rast(climate_idw)

climate_df <- as.data.frame(climate_rast, xy = TRUE, na.rm = TRUE) #spatialpointsdataframe

climate_plot <- ggplot()+
  geom_tile(data = climate_df, aes(x = x, y = y, fill = precipitation)) +
  geom_sf(data = ontario_shape_sf, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(option = "cividis") +
  coord_sf(expand = FALSE) +
  labs(title = "IDW-Interpolated June Mean Precipitation",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
climate_plot

#Logistic Regression (Binomial Family) ----------------
#GEOTIFF flood maps

flood2011 <- rast("DFO_3861_From_20110827_to_20110913/DFO_3861_From_20110827_to_20110913.tif")

flood2013 <- rast("DFO_4051_From_20130427_to_20130429/DFO_4051_From_20130427_to_20130429.tif")

plot(flood2011, main = "Flood in ON 2011")
plot(st_geometry(ontario_shape_sf), add=TRUE, border = "red")

plot(flood2013[[1]], main = "Flood in ON 2013")
plot(st_geometry(ontario_shape_sf), add=TRUE, border = "red")

#resampling to fit the flood rasters to climate (two var) raster
#method used: nearest neighbour, the default method

flood_2011_res <- resample(flood2011, climate_rast, method = "near")
flood_2013_res <- resample(flood2013, climate_rast, method = "near")

#stack flood raster with climate raster
climate_flood2011 <- c(climate_rast, flood_2011_res[[1]])
nlyr(climate_flood2011)
names(climate_flood2011) <- c("temperature", "precipitation", "flood") 

flood_df <- as.data.frame(climate_flood2011, xy = TRUE, na.rm = TRUE)

#logistic regression
fit <- glm(flood ~ temperature + precipitation, data = flood_df, family = binomial)

summary(fit)

#predictors do slightly better than no predictors at all based off of null to residual deviance
#pseudo R2 = 2.6% improvement in deviance
#need to fit another model w more predictors in order to assess change in AIC
#prediction? based off of badly fit model

flood_prob <- predict(climate_rast, fit, type = "response")
plot(flood_prob, main = "Susceptibility of ON Flood Based on 2011 Events")

#Cross Validation -------------------
library(mlr3)
library(mlr3learners) #contains logistic regression learner
library(mlr3measures) #for AUROC (ONLY FOR BINOMIAL!)
library(data.table)
library(pROC)

flood_df$flood <- as.factor(flood_df$flood) #convert flood to factor bc previously it was numeric

task <- TaskClassif$new(id = "flood_task",
                        backend = flood_df,
                        target = "flood",
                        positive = "1"
                        )

#specify logistic regression
#predict probabilities
learner <- lrn("classif.log_reg",
               predict_type = "prob"
               )

#five fold cross validation
#split into five portions
#a rotation where every fold gets to be tested once
#at one time, there's always 4 training folds and 1 testing fold
#another common choice is 10 folds (k=10) with 90/10 split, less bias w more folds
resampling <- rsmp("cv", folds = 5)

resampling$instantiate(task)

#for each fold, it trains the model on the training portion
#makes prediction on the testing portion (fold)
#stores the predictions
rr <- resample(task, learner, resampling, store_models = TRUE)

#auroc measures how well positives(correct predictions) vs negatives(incorrect predictions)
#probability that each observation is a 1
#prob > p = flood, we get some false positives and false negatives
#true positive rate (sensitivity) vs false positive rate (1-specificity)
#auroc = 1 is a perfect model that always ranks true positives above negatives
#auroc = 0.78146 78% chance that the model will rank a flood higher than no flood
rr$aggregate(msr("classif.auc"))

preds <- rr$prediction()

preds_dt <- as.data.table(preds)

#truth indicates actual outcome from flood data
#predicted probability that it's 1
roc_obj <- roc(preds_dt$truth, preds_dt$prob.1)
plot(roc_obj, main = "AUROC Score")

#Decision Trees --------------------

library(rpart) #direct engine for decision tree application
library(caret) #meta engine for decision tree application

#model interpretability packages
library(rpart.plot)  #for plotting decision trees
library(vip)         #for feature importance
library(pdp)         #for feature effects


fit2 <- rpart(
  formula = flood ~ .,
  data = flood_df,
  method = "anova"
)

#results tell me:
#node = unique id for each split
#split = splitting rule at the node
#n = number of observations at the node
#deviance = measure of purity, low means less uncertainty, more pure
#yval = predicted value at each node
# 1) tells us that there is a 0.1206273 probability of a 1.
#if latitude is > x and longtitude is < y, then probability of a 1 is...

fit2
rpart.plot(fit2, type = 2, extra = 104, fallen.leaves = TRUE,
           box.palette = "Blues", main = "Flood Risk Decision Tree")

#Random Forest -----------------

library(randomForest)

fit_rf <- randomForest(flood ~ x + y + temperature + precipitation,
                       data = flood_df,
                       ntree = 500,
                       importance = TRUE
                       )

#low with only 1 variable tried at each split
#MSE of 0.0575 (error bw predicted and observed floods)
#we've only explained 45.78% of the variation in response with our predictors

fit_rf

#importance tells me about the % increase in mean squared error
#as each variable is removed from the model, how much error increases in the model
#each split causes a total reduction in variance
#seems like latitude and longitude has more influence than all other variables...
importance(fit_rf)
varImpPlot(fit_rf)

#Cross Validation Ranger -----------------

library(mlr3measures)
library(ranger)

learner <- lrn("classif.ranger", predict_type = "prob")

rrrf <- resample(task, learner, resampling, store_models = TRUE)

rrrf$aggregate(msr("classif.auc"))

preds.rf <- rrrf$prediction()

preds.rf_dt <- as.data.table(preds.rf)

roc.rf <- roc(preds.rf_dt$truth, preds.rf_dt$prob.1)

plot(roc.rf, main = "AUROC Score RF")

#Logistic Regression for Multiple Layers -----------------

#train one model across years

#create a similar stack for 2011 - 2013 floods
library(dplyr)

nlyr(climate_rast)
plot(climate_rast[[2]], main="2nd layer?")

nlyr(masked_raster)
nlyr(p_raster)

class(masked_raster)

#change to RasterStack
#I need to use an interpolation method on every single layer of 2010-2025 for both temperature and precipitation!
#switch to thin plate spline for uneven distributions
#as a radial basis function, tps posits that the further the point is, the less influence it has on interpolated value
#influence decays with distance as a function of phi
#we are looking for the coefficient lambda, which determines influence of "circle" with center at point x.


library(fields)

years_tps <- 2010:2025

#empty list to fill
tps_t_list <- list()

#create template rasters filled with grid values of 1 to be filled later w temperature interpolated data
grid_raster <- rast(ext(ontario_shape), resolution = 0.1, crs = "EPSG:4326")
grid_raster <- init(grid_raster, fun = 1)

#make sure they have the same crs as the ontario shape file (now a spatvector)
crs(grid_raster) <- crs(ontario_shape)

#crop the template raster so that it's within the ontario_shape bounds
grid_raster <- crop(grid_raster, ontario_shape)
grid_raster <- mask(grid_raster, ontario_shape)

#convert to data frame points
template_points <- as.data.frame(as.points(grid_raster), xy = TRUE)

ontario_points_t_sf <- st_as_sf(ontario_points)
coords <- st_coordinates(ontario_points_t_sf)
ontario_points_t_sf$Year <- as.integer(ontario_points_t_sf$Year)
ontario_points_t_sf$temperature <- as.numeric(ontario_points_t_sf$mean_temp)
ontario_points_t_sf$x <- coords[, 1]
ontario_points_t_sf$y <- coords[, 2]

ontario_bbox <- st_bbox(ontario_shape)
xvals <- seq(ontario_bbox$xmin, ontario_bbox$xmax, by = 0.1)
yvals <- seq(ontario_bbox$ymin, ontario_bbox$ymax, by = 0.1)

grid.list <- list(x = xvals, y = yvals)

#creating loop
for (yr in years_tps) {
  message("Interpolating year ", yr, "...")
  
  #filter out all NA for temperature and convert to dataframe
  t_year <- ontario_points_t_sf |> 
    filter(Year == yr & !is.na(temperature)) |>
    as.data.frame()
  
  t_year$Year <- as.numeric(t_year$Year)

  #interpolation
  tps_model <- Tps(x = as.matrix(t_year[, c("x", "y")]),
                   Y = t_year$temperature)
  
  #predict within the bounds of the bbox
  preds <- predictSurface(tps_model, grid.list = grid.list)
  
  #create dataframe for predicted (interpolated) values
  zvals <- as.vector(preds$z)
  grid_df <- expand.grid(x = preds$x, y = preds$y)
  pred_df <- cbind(grid_df, temp = zvals)
  
  #turn dataframe into a raster
  rast_year <- rast(pred_df, type = "xyz", crs = "EPSG:4326")
  
  #make sure it's the SAME CRS!!
  crs(rast_year) <- crs(ontario_shape)
  
  #crop again to make sure that the raster is masked to ontario's shape
  rast_year <- crop(rast_year, ontario_shape)
  rast_year <- mask(rast_year, ontario_shape)
  
  tps_t_list[[as.character(yr)]] <- rast_year
}

plot(tps_t_list[[1]], main = "Ontario Mean June Temperature 2010")
plot(ontario_shape, add=TRUE, border = "black")



#now the same interpolation for precipitation
tps_p_list <- list()

ontario_points_p_sf <- st_as_sf(ontario_points_p)
coords <- st_coordinates(ontario_points_p_sf)
ontario_points_p_sf$Year <- as.integer(ontario_points_p_sf$Year)
ontario_points_p_sf$precipitation <- as.numeric(ontario_points_p_sf$precipitation)
ontario_points_p_sf$x <- coords[, 1]
ontario_points_p_sf$y <- coords[, 2]

for (yr in years_tps) {
  message("Interpolating year ", yr, "...")
  
  p_year <- ontario_points_p_sf |> 
    filter(Year == yr & !is.na(precipitation)) |>
    as.data.frame()
  
  p_year$Year <- as.numeric(p_year$Year)
  
  tps_p_model <- Tps(x = as.matrix(p_year[, c("x", "y")]),
                   Y = p_year$precipitation)
  
  preds_p <- predictSurface(tps_p_model, grid.list = grid.list)
  
  zvals_p <- as.vector(preds_p$z)
  grid_p_df <- expand.grid(x = preds_p$x, y = preds_p$y)
  pred_p_df <- cbind(grid_p_df, temp = zvals_p)
  
  rast_p_year <- rast(pred_p_df, type = "xyz", crs = "EPSG:4326")
  
  crs(rast_p_year) <- crs(ontario_shape)
  
  rast_p_year <- crop(rast_p_year, ontario_shape)
  rast_p_year <- mask(rast_p_year, ontario_shape)
  
  tps_p_list[[as.character(yr)]] <- rast_p_year
}

plot(tps_p_list[[1]], main = "Ontario Mean June Precipitation 2010")
plot(ontario_shape, add=TRUE, border = "black")

#Looks like this is not the best interpolation. I will be playing around interpolating all of NA next.

