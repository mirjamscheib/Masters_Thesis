
####  HYDRODYNAMIC MODEL RESULTS ####

# convert hydrodynamic model results into a shapefile, rasters 
# and return the number of cells of the mesh 

# meaning of the functions arguments: 
# h5_path = path of the .h5 file you want to convert into a raster
# Twodm_path = path of the .2dm file (mesh file) which resulted from hydrodynamic simulations
# shp_path = path and name of the shapefile you want to write and read 
# raster_wd_path = path and name of the raster reflecting water depth at a specific discharge
# raster_v_path = path and name of the raster reflecting velocity at a specific discharge

hdm_results <- function(h5_path, Twodm_path, shp_path, raster_wd_path, raster_v_path){
  # read h5 file 
  h5 <- h5file(h5_path)
  
  # access the different sub-directories ("groups") with "[[]]"
  results <- h5[["RESULTS"]][["CellsAll"]]
  
  # extract velocity and water depth for every cell
  bottomElevation <- h5[["CellsAll"]][["BottomEl"]][,]
  waterSurfElev <- results[["HydState"]][["0000003"]][1,]
  qX <- results[["HydState"]][["0000003"]][2,]
  qY <- results[["HydState"]][["0000003"]][3,]
  depth <- waterSurfElev - bottomElevation
  vX <- qX/depth
  vY <- qY/depth
  V_abs <- (vX^2+vY^2)^.5
  
  
  #read mesh as a table
  Twodm <- read.table(Twodm_path, sep="", skip=2, header=F, fill=T)
  
  #split mesh in nodes and elements
  nd_df <- Twodm[which(Twodm$V1=="ND"),-1]
  colnames(nd_df) <- c( "ID", "X", "Y", "Z")
  nd_df$X <- as.numeric(nd_df$X)
  nd_df$Y <- as.numeric(nd_df$Y)
  
  # elements
  e3t_df <- Twodm[which(Twodm$V1=="E3T"),][,-1]
  colnames(e3t_df) <- c( "ID", "N1", "N2", "N3")
  
  
  # create table with coordinates of each triangular element (x1,y1,x2,y2,x3,y3)
  triangles <- (cbind(nd_df[e3t_df$N1, c("X","Y")], nd_df[e3t_df$N2, c("X","Y")], nd_df[e3t_df$N3, c("X","Y")], nd_df[e3t_df$N1, c("X","Y")]))
  
  tr_matrix <- matrix(unlist(triangles), ncol=8, byrow = F)
  
  # number of elements
  ID <- e3t_df$ID
  length(ID) # 38694 triangular cells in the computational mesh
  
  # Create SP object
  polys <- SpatialPolygons(mapply(function(poly, id) {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  }, split(tr_matrix, row(tr_matrix)), ID))
  
  
  # Create SPDF (exportable as shapefile)
  mesh.sp <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
  
  # link discharge results with mesh 
  mesh.sp$waterSurfElev <- as.numeric(round(waterSurfElev,3))
  mesh.sp$depth <- as.numeric(round(depth,3))
  mesh.sp$V_abs <- as.numeric(round(V_abs,3))
  
  # write new shapefile
  shape <- shapefile(mesh.sp, shp_path, overwrite = TRUE)
  
  # Read the shapefile
  shapefile <- shapefile(shp_path)
  
  # Set the raster extent using the bounding box of the shapefile
  raster_extent <- extent(shapefile)
  
  # Set the raster resolution in meters
  raster_resolution <- 0.5
  
  # Create an empty raster layer with the specified extent and resolution
  raster_layer <- raster(ext = extent(shapefile), res = raster_resolution)
  
  # Rasterize the shapefile into the empty raster layer
  # Water depth
  raster_depth <- rasterize(shapefile, raster_layer, shapefile$depth)
  
  # velocity
  raster_velocity <- rasterize(shapefile, raster_layer, shapefile$V_abs)
  
  # Save the raster layer as a GeoTIFF file
  writeRaster(raster_depth, raster_wd_path, format = "GTiff", overwrite = TRUE)
  writeRaster(raster_velocity, raster_v_path, format = "GTiff",
              overwrite = TRUE)
  
  return(length(ID))
}


#### FOEN HABITAT MODEL ####

# function for reclassified rasters without persistent habitats

# raster_path = path to the raster which contains a certain discharge scenario
# output_path_reclass = output path of reclassified raster
# HSC_path = path of HSC 
# output_path_hm = output path for raster of univariat habitat model

foen_hm <- function(raster_path, base_flow, output_path_pers_reclass, output_path_reclass, HSC_path, output_path_hm, output_path_hm_persistent, river, discharge){
  # read raster with discharge scenario
  scenario <- raster(raster_path) 
  
  # Set all 0-cells in the raster to NA
  scenario[scenario == 0] <- NA  
  
  # Define reclassification table according to Schmidlin et al. (2023)
  reclass_table <- c(0, 0.05, 1, 
                     0.05, 0.25, 3, 
                     0.25, 0.75, 5,
                     0.75, 1.50, 4,
                     1.50, 2.50, 2,
                     2.50, Inf, NA)
  
  # reclassify raster according to reclass_table 
  raster_classify <- reclassify(scenario, reclass_table,  include.lowest = TRUE) 
  
  # cut reclassified raster with base flow scenario to get persistent habitats
  pers_class <- mask(raster_classify, raster(base_flow))
  
  # save reclassified raster
  writeRaster(pers_class, output_path_pers_reclass, format = "GTiff", overwrite = TRUE) 
  # save reclassified raster of persistent habitats
  writeRaster(raster_classify, output_path_reclass, format = "GTiff", overwrite = TRUE) 
  # read HSC 
  HSC <- read.csv(HSC_path, sep=";", dec=".", header = TRUE)
  
  # calculate univariat habitat model 
  univariat <- calc(raster_classify, fun = approxfun(HSC$Bewohnbarkeitsklasse, HSC$HSI,
                                                     rule = 2))
  
  # cut univariat model results with base flow scenario to get persistent habitats
  persistent_univariat <- mask(univariat, raster(base_flow)) 
  
  # save univariat model results 
  writeRaster(univariat, output_path_hm, format = "GTiff", overwrite = TRUE)
  
  # save univariat model results of persistent habitats 
  writeRaster(persistent_univariat, output_path_hm_persistent, format = "GTiff",
              overwrite = TRUE)
  
  # calculate weighted usable area (WUA) 
  wua <- cellStats(univariat, 'sum') * 4    
  
  # calculate WUA of persistent habitats
  wua_pers <- cellStats(persistent_univariat, 'sum') * 4 
  
  # calculate frequency table 
  freq_table <- as.data.frame(freq(univariat, digit=1, useNA= "no")) 
  
  # calculate frequency table of persistent habitats  
  freq_table_pers <- as.data.frame(freq(persistent_univariat, digit=1, useNA= "no"))
  
  # calculate wetted area (WA)
  wa_tot <- sum(freq_table[["count"]]) * 4
  
  # calculate WA of persistent habitats 
  wa_tot_pers <- sum(freq_table_pers[["count"]]) * 4
  
  # calculate hydraulic habitat suitability (HHS)
  hhs <- wua/wa_tot 
  
  # calculate HHS of persistent habitats 
  hhs_pers <- wua_pers/wa_tot_pers 
  
  # combine all relevant metrics into a dataframe 
  results_metrics <- data.frame(WUA = wua, WUA_pers = wua_pers, WA_tot = wa_tot,
                                WA_tot_pers = wa_tot_pers, HHS = hhs, HHS_pers =
                                  hhs_pers, River = river, Discharge = discharge)
  
  # combine all frequency table results into a dataframe 
  results_freq <- full_join(freq_table, freq_table_pers, by = "value")
  results_freq <- cbind(river, discharge, results_freq)
  
  # give frequency table dataframe column names 
  colnames(results_freq) <- c("River", "Discharge", "value", "normal",
                              "persistent")
  
  # combine the two resulting dataframes into a list object to recall later
  results <- list(metrics = results_metrics, freq = results_freq)
}