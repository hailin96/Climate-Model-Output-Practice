# Line Extraction 

library(sf)
library(spData)
library(terra)
library(lwgeom)
library(tidyverse)
library(dplyr)

#srtm is a raster object of Utah elevation above sea level
#zion is a vector object of Zion National Park
srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
#we want to transform zion into a geometric object with srtm's coordinate reference system.
zion = st_transform(zion, st_crs(srtm))

#Looking at Zion National Park, we want to extract raster elevation value at corresponding zion points.
#creates a dataframe with one column that we will then bind with zion_points.
data("zion_points", package = "spDataLarge")
elevation = terra::extract(srtm, zion_points)
zion_points = cbind(zion_points, elevation)
plot(srtm, main = "Elevation Data")
plot(zion, add = TRUE)
plot(zion_points, add = TRUE)


#using a line to determine which cells are selected
#where the line touches, the srtm values are recorded as a dataframe, giving you an elevation profile.
#we will create a new line 
zion_transect = cbind(c(-113.2, -112.9), c(37.45, 37.2)) |> #basic pipe operator
  st_linestring() |> 
  st_sfc(crs = crs(srtm)) |>
  st_sf(geometry = _)


#LINESTRING GEOMETRY: Set of Connected Points
#We only have one LINE TRANSECT
#create a new "id" column to the sf object zion_transect. 
#unique id for each row of the sf object. From 1 to the number of rows.
#add points to areas around the line
#convert these points and find out the distances between them
zion_transect$id = 1:nrow(zion_transect)
zion_transect = st_segmentize(zion_transect, dfMaxLength = 250)
zion_transect = st_cast(zion_transect, "POINT")

plot(srtm, main = "Experimenting with Line Transects")
plot(zion, add = TRUE)
plot(zion_transect, add = TRUE)

#deriving distances between points using st_distance().
zion_transect = zion_transect |> 
  group_by(id) |> 
  mutate(dist = st_distance(geometry)[, 1]) 

#extract elevation values 
zion_elev = terra::extract(srtm, zion_transect)
zion_transect = cbind(zion_transect, zion_elev)
zion_transect

#POLYGON RASTERIZATION
#We want to convert vector data (incl polygons) into a raster format.Drawing -> pixelated painting.
#Can also simplify datasets so everything has the same spatial resolution.
cycle_hire_osm = spData::cycle_hire_osm
#Reproject bike data onto the British National Grid
cycle_hire_osm_projected = st_transform(cycle_hire_osm, "EPSG:27700")
#ext() gets the bounding box to contain ALL bike points
raster_template = rast(ext(cycle_hire_osm_projected), resolution = 1000,
                       crs = crs(cycle_hire_osm_projected))

ch_raster3 = rasterize(cycle_hire_osm_projected, raster_template, 
                       field = "capacity", fun = sum, na.rm = TRUE)

plot(ch_raster3, main = "Bike Points")
#------
california = dplyr::filter(us_states, NAME == "California")
#Converting the california polygon into just line transects or borders.
california_borders = st_cast(california, "MULTILINESTRING")
#Create a blank raster the same size as california box.
raster_template2 = rast(ext(california), resolution = 0.5,
                        crs = st_crs(california)$wkt)
#Any cell that touches the borders of california, fill it in the raster.
california_raster1 = rasterize(california_borders, raster_template2,
                               touches = TRUE)
#Only based on the cells that are within the shape.
california_raster2 = rasterize(california, raster_template2) 

plot(california_raster1, main = "Filling Around the Border")
plot(california_borders, add=TRUE)
plot(california_raster2, main = "Filling Inside Border")
plot(california_borders, add=TRUE)


