pkgCheck <- function(x){ 
  if (!require(x,character.only = TRUE)){
    install.packages(x,dependencies=TRUE)
    if(!require(x,character.only = TRUE)) {
      stop()
    }
  }
}

pkgCheck("raster")
pkgCheck("spsurvey")
pkgCheck("terra")
pkgCheck("ggplot2")
pkgCheck("tibble")
pkgCheck("readr")
pkgCheck("dplyr")
pkgCheck("tidyr")
pkgCheck("scales")
pkgCheck("hdf5r")
pkgCheck("sp")
pkgCheck("sf")
pkgCheck("spatstat.geom")
pkgCheck("spatstat.explore")
pkgCheck("gstat")
pkgCheck("purrr")
pkgCheck("rgdal")

L_2 <- raster("raster_files/GL1_v_3_28.tif")
crs(L_2) <- "EPSG: 2056"
plot(L_2)

L_3 <- raster("raster_files/GL1_wd_3_28.tif")
crs(L_3) <- "EPSG: 2056"
plot(L_3)

L_2 <- raster("raster_files/GL1_v_6_00.tif")
crs(L_2) <- "EPSG: 2056"
plot(L_2)

L_3 <- raster("raster_files/GL1_wd_6_00.tif")
crs(L_3) <- "EPSG: 2056"
plot(L_3)

