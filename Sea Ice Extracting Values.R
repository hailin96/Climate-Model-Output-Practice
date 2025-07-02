#Extracting Sea Ice Values from Raster

setwd("Documents/Documents/SFU/NSERC/USRA 2/Data/Temp")

#Taking a look at an example GeoTIF file for North Arctic in June 1979.
#North Arctic 1979 -------------
url <- 
  "https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/geotiff/06_Jun/N_197906_concentration_v3.0.tif"
destination <- "N1979_concentration.tif"
download.file(url, destination, mode = "wb")

url <- 
  "https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/geotiff/06_Jun/N_197906_extent_v3.0.tif"
destination <- "N1979_extent.tif"
download.file(url, destination, mode = "wb")

#Convert GeoTIF file into a raster.
install.packages("exactextractr")

library(terra)
library(sf)
library(exactextractr)
library(spData)
library(raster)

seaice_1979 <- rast("N1979_concentration.tif")
plot(seaice_1979, main = "Northern Arctic Sea Ice in June 1979")

#X and Y display polar stereographic projection, so we will transform it to a CRS we are familiar with.
crs(seaice_1979) <- "EPSG:3411" #The code for polar stereographic projection CRS.
seaice_1979_lalo <- project(seaice_1979, "EPSG:4326")
plot(seaice_1979_lalo, main = "Northern Arctic Sea Ice in June 1979")

#Looks like latitude-longitude doesn't really work here, so let's stick with polar stereographic projection.
#I want to place Canada's shape (sf object) on top.

canada <- world[world$name_long == "Canada", ]
canada_polar <- st_transform(canada, crs(seaice_1979))

plot(seaice_1979, main = "Northern Arctic Sea Ice in June 1979")
plot(st_geometry(canada_polar), add=TRUE, border = "yellow")

#Using a single line transect, we want to be able to extract sea ice concentration values IN CANADA.
canada_transect <- cbind(c(-2000000, -100000), c(-700000, -1000000)) |> #Near QE Islands, manual adjustment
  st_linestring() |>
  st_sfc(crs = crs(seaice_1979)) |>
  st_sf(geometry = _)

#Creating an unique ID for each row of the line.
canada_transect$id <- 1:nrow(canada_transect)
canada_transect <- st_segmentize(
                                  canada_transect, 
                                  dfMaxLength = 100000 #distance between points is smaller than 100000m
                                  )
canada_transect <- st_cast(canada_transect, "POINT")

plot(seaice_1979, main = "Experimenting with 1 Line Transect")
plot(st_geometry(canada_polar), add = TRUE, border = "yellow")
plot(canada_transect, add = TRUE, col="red")

#Now the plot looks too big and I want to zoom into Canada.
#How much area does the line transect and the Canada sf object need?

combo <- st_union(canada_polar, canada_transect) #combine into one sf object
combo_area <- st_bbox(combo) #extracts the bounding box parameters
combo_area_ext <- ext(combo_area) #what's the extent of the above box?

#Add some buffer room to the comboarea boundaries.
combo_buffer <- ext(
  xmin(combo_area_ext) - 100000,
  xmax(combo_area_ext) + 100000,
  ymin(combo_area_ext) - 100000,
  ymax(combo_area_ext) + 100000
)

plot(seaice_1979, main = "Canadian Sea Ice in June 1979", ext = combo_buffer)
plot(st_geometry(canada_polar), add=TRUE)
plot(canada_transect, col = "red", add=TRUE)

#There's an area of southern BC and Alberta that's not covered by the sea ice raster.
#crop it out
combo_buffer <- ext(
  xmin(combo_area_ext) + 1000000,
  xmax(combo_area_ext) + 300000,
  ymin(combo_area_ext) - 100000,
  ymax(combo_area_ext) + 100000
)

plot(seaice_1979, main = "Canadian Sea Ice in June 1979", ext = combo_buffer)
plot(st_geometry(canada_polar), add=TRUE, border = "yellow")
plot(canada_transect, col = "red", add=TRUE)

#Let's extract sea ice concentration values from the line
seaiceconc <- terra::extract(seaice_1979, canada_transect)
seaiceconc <- cbind(canada_transect, seaiceconc)
head(seaiceconc)

#Let's use exactextract to see the difference
#Exact_extract() only works with polygons, not the POINTS in our line transect.
#We must create a buffer zone around the points to create a circle.
buffer <- st_buffer(canada_transect, dist = 25000) #A circle with 25km radius.
seaiceconc_exact <- exact_extract(seaice_1979, buffer, fun = "mean") 
seaiceconc_exact <- cbind(canada_transect, seaiceconc_exact)
head(seaiceconc_exact)

#How many sea ice pixels are sampled in each buffer?
seaiceconc_exact <- exact_extract(seaice_1979, buffer, fun = "count") 
seaiceconc_exact <- cbind(canada_transect, seaiceconc_exact)
head(seaiceconc_exact) #3.14 pixels?

#How far does the line transect go?
#fun="length" only works with linestrings. We must convert our line transect into a true line.
#Exact_extract() no longer supports fun="length".
#Exact_extract() only works with POLYGONS, not LINES.
#Fun=length calculates how many cells the line has touched.
#Exact_extract() also works with raster package better than terra.
seaice_1979_raster <- raster(seaice_1979 >15) #takes away the NA cells 
canada_line <- st_union(canada_transect) |> #combine all points 
  st_cast("LINESTRING") |>
  st_sf(geometry = _) #linestring is now a sf object with linestring geometry column
seaiceline <- exact_extract(seaice_1979_raster, canada_line, fun = "length") #doesn't work anymore!

#Taking a look at an example GeoTIF file for North Arctic on 2024.
#North Arctic 2024 ------------
url <- 
  "https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/geotiff/06_Jun/N_202406_concentration_v3.0.tif"
destination <- "N2024_concentration.tif"
download.file(url, destination, mode = "wb")

url <- 
  "https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/geotiff/06_Jun/N_202406_extent_v3.0.tif"
destination <- "N2024_extent.tif"
download.file(url, destination, mode = "wb")

seaice_2024 <- rast("N2024_concentration.tif")
plot(seaice_2024, main = "Northern Arctic Sea Ice in June 2024")

canada_polar_2024 <- st_transform(canada, crs(seaice_2024))

plot(seaice_2024, main = "Northern Arctic Sea Ice in June 2024")
plot(st_geometry(canada_polar_2024), add=TRUE, border = "yellow")

plot(seaice_2024, main = "Experimenting with 1 Line Transect 2024")
plot(st_geometry(canada_polar_2024), add = TRUE, border = "yellow")
plot(canada_transect, add = TRUE, col="red")

plot(seaice_2024, main = "Canadian Sea Ice in June 2024", ext = combo_buffer)
plot(st_geometry(canada_polar_2024), add=TRUE, border = "yellow")
plot(canada_transect, col = "red", add=TRUE)

#Let's extract sea ice concentration values from the line
seaiceconc_2024 <- terra::extract(seaice_2024, canada_transect)
seaiceconc_2024 <- cbind(canada_transect, seaiceconc_2024)
head(seaiceconc_2024)

#Find the difference between 1979 and 2024
#Concentration is scaled by 10, where 1000 = 100% of raster cell.
conc1979 <- seaiceconc$N1979_concentration/10
conc2024 <- seaiceconc_2024$N2024_concentration/10
PointID <- seaiceconc_2024$id

conc_change <- conc1979 - conc2024
conc_comp <- cbind(PointID, conc_change) 
head(conc_comp)

#Plot the difference dataframe
library(ggplot2)

ggplot(conc_comp, aes(x = PointID, y = conc_change)) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Change in Sea Ice Concentration (1979 - 2024)",
       x = "Point ID",
       y = "Change in Ice Concentration (% change, scaled by 10)") + 
  theme_minimal()