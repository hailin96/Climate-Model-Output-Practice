# Aggregating Temperature Data

install.packages("raster")
install.packages("terra")
install.packages("RNetCDF")
install.packages("OpenStreetMap")
install.packages("sf")
install.packages("tsbox")
install.packages("leaflet")

library(raster)
library(RNetCDF) #what is this package?
#RNetCDF deals with NetCDF files, which are designed for sharing array-oriented scientific data.

precipitation <- open.nc(
  "Documents/Documents/SFU/NSERC/USRA 2/Data/Test Prec/pr_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012.nc"
  )

pr.dates <- as.Date(
  var.get.nc(precipitation, "time"), origin="1850-01-01 00:00:00"
)

pr.scenes <- sapply(
  1:length(pr.dates), function(z) {
    grid = var.get.nc(precipitation, "pr", start = c(NA, NA, z), count = c(NA, NA, 1))
    x = raster(grid, xmn=-90, xmx=90, ymn=0, ymx=360,
               crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
               )
    x = rotate(flip(t(x), 2))
    x = x * 3401.575 * 30
    return(x)
  }
)

close.nc(precipitation)

plot(
  log(pr.scenes[[1000]]), main=pr.dates[[1000]],
     col=colorRampPalette(c("tan2", "lightgray", "darkgreen"))(32)
     )

#Aggregated precipitation for 2025

indices <- which(
  (pr.dates >= as.Date(paste0("2025-01-01"))) &
    (pr.dates <= as.Date(paste0("2025-12-31")))
)

pr.2025 = pr.scenes[[indices[1]]]

for (scene in pr.scenes[indices[2:length(indices)]]) {
  pr.2025 = pr.2025 + scene
}

plot(
  log(pr.2025), main="2025", col=colorRampPalette(c("tan2", "lightgray", "darkgreen"))(32)
     )

#Aggregated precipitation for 2050

indices2 <- which(
  (pr.dates >= as.Date(paste0("2050-01-01"))) &
    (pr.dates <= as.Date(paste0("2050-12-31")))
)

pr.2050 = pr.scenes[[indices2[1]]]

for(scene in pr.scenes[indices2[2:length(indices2)]]
) {
  pr.2050 = pr.2050 + scene
}

plot(
  log(pr.2050), main="2050", col=colorRampPalette(c("tan2", "lightgray", "darkgreen"))(32)
)

#Aggregated precipitation for 2090

indices3 <- which(
  (pr.dates >= as.Date(paste0("2090-01-01"))) &
    (pr.dates <= as.Date(paste0("2090-12-31")))
)

pr.2090 = pr.scenes[[indices3[1]]]

for(scene in pr.scenes[indices3[2:length(indices3)]]) {
  pr.2090 = pr.2090 + scene
}

plot(
  log(pr.2090), main="2090", col=colorRampPalette(c("tan2", "lightgray", "darkgreen"))(32)
)