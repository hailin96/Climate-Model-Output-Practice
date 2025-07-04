# The Diversity of Geographic Data (Chapter 8)

#Files are either vectors or rasters.
#GDAL "goo-dal" - Geospatial Data Abstraction Library

library(sf)
library(terra)

#Upload an example GeoTIFF file
setwd("Documents/Documents/SFU/NSERC/USRA 2/Data/Temp")

url <- 
  "https://noaadata.apps.nsidc.org/NOAA/G02135/north/monthly/geotiff/06_Jun/N_197906_concentration_v3.0.tif"
destination <- "N1979_concentration.tif"
download.file(url, destination, mode = "wb")

seaice_1979 <- rast("N1979_concentration.tif")

#Dealing with raster data -------------------
#Unsigned integers (INT1U, INT2U, INT4U) are suitable for categorical data, while 
#float numbers (FLT4S and FLT8S) usually represent continuous data

writeRaster(seaice_1979, filename = "1979 Sea Ice Copy.tif", datatype = "INT1U", overwrite=TRUE)
#only works with INT1U

#Geoportals ---------------
download.file(url = "https://hs.pangaea.de/Maps/PeRL/PeRL_permafrost_landscapes.zip",
              destfile = "PeRL_permafrost_landscapes.zip", 
              mode = "wb")
unzip("PeRL_permafrost_landscapes.zip")
canada_perma_land = read_sf("PeRL_permafrost_landscapes/canada_perma_land.shp")

#shp is a shapefile that's a geospatial vector

#Geodata package -----------------

library(geodata)
library(rnaturalearth)
#raster with multiple time steps, but why resolution is presented in time instead of km?
worldprecipitation <- worldclim_global("prec", res = 10, path = tempdir())
class(worldprecipitation)

plot(worldprecipitation[[1]], main = "Precipitation T=1")

canadashape <- ne_countries(country = "Canada", returnclass = "sf")
plot(canadashape, add=TRUE, border = "yellow")

#Interactive maps -------------------

#Let's map out Ontario's average temperature for May
library(tmap)
library(readr)
library(mapview)
library(rnaturalearthdata)

ontario <- read_csv("en_climate_summaries_ON_05-2025.csv")

#convert the csv into spatvectors in order to illustrate spatial points

ontario_vect <- vect(ontario, geom = c("Long", "Lat"), crs = "EPSG:4326")

#create a template raster for these spatvectors

ontario_raster <- rast(ext(ontario_vect), resolution = 0.1) #Using 0.1 degrees as length/width of cell.

#what's the mean temperature within each cell?
ontario_raster <- rasterize(ontario_vect, ontario_raster, field = "Tx", fun = "mean")

plot(ontario_raster, main = "Ontario's May Monthly Temperature for 2025")

#add Ontario's shape
ontario_shape <- canadashape[canadashape$name == "Ontario", ] #doesn't work, ON not in library

#found a creator on ArcGIS (BlueSkyNetGIS)

ontario_shape <- vect("Ontario_Simple_Boundary/Ontario Simple Boundary.shp")

plot(ontario_raster, main = "Ontario's Mean Maximum Temperature for May 2025")
plot(ontario_shape, add=TRUE, border = "black")

mapview(ontario_raster, layer.name = "Mean Max Temp (Tx) of May 2025") +
  mapview(ontario_shape, col.region = "lightblue", color = "black", lwd = 2) #fix ON shape

mapview(ontario_raster, layer.name = "Mean Max Temp (Tx) of May 2025") +
  mapview(ontario_vect, color = "red", size = 2) + #markers for each Tx point
  mapview(ontario_shape, col.region = "lightblue", color = "black")

#add Brockville onto this Ontario plot

library(cancensus)
set_cancensus_api_key(
  "CensusMapper_f5c2090496b7235615b4c6c6dc2c9664", install = TRUE
)

#we want to source the shape of Brockville from census data, but set a dummy vector (returns no data)
#CA16 is census 2016, CSD = Municipal Region, 3507015 is the municipal code.
brockvilleshape <- get_census(
  dataset = "CA16", regions = list(CSD = "3507015"), vectors = "V_CA16_1",use_cache=FALSE, geo_format = "sf"
)

#fuse the coordinates from census to the coordinates that we set up previously for Brockville
brockvilleshape <- st_transform(brockvilleshape, crs((ontario_raster)))

plot(ontario_raster, main = "Ontario's May Monthly Temperature for 2025")
plot(ontario_shape, add=TRUE, border = "black")
plot(brockvilleshape, add=TRUE, border = "red")

mapview(ontario_raster, layer.name = "Mean Max Temp (Tx) of May 2025") +
  mapview(ontario_shape, col.region = "lightblue", color = "black", lwd = 2) + #fix ON shape
  mapview(brockvilleshape, col.region = "lavender", color = "purple", lwd = 2)

mapview(ontario_raster, layer.name = "Tx") +
  mapview(ontario_vect, color = "red", size = 2) + #markers for each Tx point
  mapview(ontario_shape, col.region = NA, color = "black") +
  mapview(brockvilleshape, col.region = NA, color = "purple", lwd = 2)

#use ggplot() in the future for a smoother presentation
