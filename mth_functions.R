
####  HYDRODYNAMIC MODEL RESULTS ####
# convert hydrodynamic model results into a shapefile, rasters 
# and return the number of cells of the mesh 

# meaning of the functions arguments: 
# h5_path = path of the .h5 file you want to convert into a raster
# twodm_path = path of the .2dm file (mesh file) which resulted from hydrodynamic simulations
# shp_path = path and name of the shapefile you want to write and read 
# raster_wd_path = path and name of the raster reflecting water depth at a specific discharge
# raster_v_path = path and name of the raster reflecting velocity at a specific discharge

hdm_results <- function(h5_path, twodm_path, shp_path, raster_wd_path, raster_v_path){
  h5 <- h5file(h5_path)                                                # read h5 file 
  results <- h5[["RESULTS"]][["CellsAll"]]                             # access the different sub-directories ("groups") with "[[]]"
  bottomElevation <- h5[["CellsAll"]][["BottomEl"]][,]                 # extract velocity and water depth for every cell
  waterSurfElev <- results[["HydState"]][["0000003"]][1,]     
  qX <- results[["HydState"]][["0000003"]][2,]                
  qY <- results[["HydState"]][["0000003"]][3,]                
  depth <- waterSurfElev - bottomElevation                    
  vX <- qX/depth                                              
  vY <- qY/depth                                              
  V_abs <- (vX^2+vY^2)^.5                                     
  twodm <- read.table(twodm_path, sep="", skip=2, header=F, fill=T)    # read mesh as a table
  nd_df <- twodm[which(twodm$V1=="ND"),-1]                             # split mesh in nodes and elements
  colnames(nd_df) <- c( "ID", "X", "Y", "Z")                           
  nd_df$X <- as.numeric(nd_df$X)                                       
  nd_df$Y <- as.numeric(nd_df$Y)                                       
  e3t_df <- twodm[which(twodm$V1=="E3T"),][,-1]                        # elements
  colnames(e3t_df) <- c( "ID", "N1", "N2", "N3")                      
  triangles <- (cbind(nd_df[e3t_df$N1, c("X","Y")],                    # create table with coordinates of each triangular element (x1,y1,x2,y2,x3,y3)
                      nd_df[e3t_df$N2, c("X","Y")], nd_df[e3t_df$N3, 
                      c("X","Y")], nd_df[e3t_df$N1, c("X","Y")]))
  tr_matrix <- matrix(unlist(triangles), ncol=8, byrow = F)           
  ID <- e3t_df$ID                                                      # number of elements
  polys <- SpatialPolygons(mapply(function(poly, id) {                 # Create SP object
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  }, split(tr_matrix, row(tr_matrix)), ID))
  mesh.sp <- SpatialPolygonsDataFrame(polys,                           # Create SPDF (exportable as shapefile)
                                      data.frame(id=ID, row.names=ID)) 
  mesh.sp$waterSurfElev <- as.numeric(round(waterSurfElev,3))          # link discharge results with mesh 
  mesh.sp$depth <- as.numeric(round(depth,3))
  mesh.sp$V_abs <- as.numeric(round(V_abs,3))
  shape <- shapefile(mesh.sp, shp_path, overwrite = TRUE)              # write new shapefile
  shapefile <- shapefile(shp_path)                                     # read the shapefile
  raster_extent <- extent(shapefile)                                   # Set the raster extent using the bounding box of the shapefile
  raster_resolution <- 0.5                                             # Set the raster resolution in meters
  raster_layer <- raster(ext = extent(shapefile),                      # Create an empty raster layer with the specified extent and resolution
                         res = raster_resolution)
  raster_depth <- rasterize(shapefile, raster_layer, shapefile$depth)  # Rasterize the shapefile into the empty raster layer - water depth
  raster_velocity <- rasterize(shapefile, raster_layer, shapefile$V_abs) # Rasterize the shapefile into the empty raster layer - velocity
  writeRaster(raster_depth, raster_wd_path, format = "GTiff",            # Save the raster layer as a GeoTIFF file
              overwrite = TRUE)
  writeRaster(raster_velocity, raster_v_path, format = "GTiff",          # Save the raster layer as a GeoTIFF file
              overwrite = TRUE)
  return(length(ID))                                                   # return number of triangle cells 
}


#### FUNCTION TO STACK RASTERS #### 
# convert rasters with single attributes (water depth or velocity) into stacked rasters

# meaning of the functions arguments: 
# v_path = path of a raster containing velocity as attribute 
# wd_path = path of a raster containing water depth as attribute 
# stack_path = path and name of the stacked raster containing velocity and water depth per discharge

stack_raster <- function(v_path, wd_path, stack_path){
  v <- raster(v_path)                                    # read raster containing velocity per discharge
  wd <- raster(wd_path)                                  # read raster containing water depth per discharge
  section <- stack(v, wd)                                # combine/stack raster velocity and water depth 
  writeRaster(section, stack_path, overwrite = TRUE)     # save stacked/combined raster 
}


#### FOEN HABITAT MODEL ####
# function for reclassified rasters without persistent habitats

# raster_path = path to the raster which contains a certain discharge scenario
# output_path_reclass = output path of reclassified raster
# HSC_path = path of HSC 
# output_path_hm = output path for raster of univariat habitat model

foen_hm <- function(raster_path, base_flow, output_path_pers_reclass, output_path_reclass, HSC_path, col1, col2, 
                    output_path_hm, output_path_hm_persistent, river, discharge){
  scenario <- raster(raster_path)                          # read raster with discharge scenario
  scenario[scenario == 0] <- NA                            # Set all 0-cells in the raster to NA 
  reclass_table <- c(0, 0.05, 1,                           # Define reclassification table according to Schmidlin et al. (2023)
                     0.05, 0.25, 3, 
                     0.25, 0.75, 5,
                     0.75, 1.50, 4,
                     1.50, 2.50, 2,
                     2.50, Inf, NA)
  raster_classify <- reclassify(scenario, reclass_table,   # reclassify raster according to reclass_table
                                include.lowest = TRUE) 
  pers_class <- mask(raster_classify, raster(base_flow))   # cut reclassified raster with base flow scenario to get persistent habitats
  writeRaster(pers_class, output_path_pers_reclass,        # save reclassified raster
              format = "GTiff", overwrite = TRUE) 
  writeRaster(raster_classify, output_path_reclass,        # save reclassified raster of persistent habitats
              format = "GTiff", overwrite = TRUE) 
  HSC <- read.csv(HSC_path, sep=";", dec=".", header = TRUE)  # read Habitat suitability curve
  univariat <- calc(raster_classify, fun = approxfun(         # calculate univariat habitat model 
    HSC[ ,col1], HSC[ ,col2], rule = 2))
  persistent_univariat <- mask(univariat, raster(base_flow))  # cut univariat model results with base flow scenario to get persistent habitats
  writeRaster(univariat, output_path_hm, format = "GTiff",    # save univariat model results 
              overwrite = TRUE)
  writeRaster(persistent_univariat, output_path_hm_persistent,   # save univariat model results of persistent habitats 
              format = "GTiff", overwrite = TRUE)
  wua <- cellStats(univariat, 'sum') * 4                      # calculate weighted usable area (WUA)
  wua_pers <- cellStats(persistent_univariat, 'sum') * 4      # calculate WUA of persistent habitats
  freq_table <- as.data.frame(freq(univariat,                 # calculate frequency table 
                                   digit=1, useNA= "no")) 
  freq_table_pers <- as.data.frame(freq(persistent_univariat, # calculate frequency table of persistent habitats 
                                        digit=1, useNA= "no"))
  wa_tot <- sum(freq_table[["count"]]) * 4                    # calculate wetted area (WA)
  wa_tot_pers <- sum(freq_table_pers[["count"]]) * 4          # calculate WA of persistent habitats 
  hhs <- wua/wa_tot                                           # calculate hydraulic habitat suitability (HHS)
  hhs_pers <- wua_pers/wa_tot_pers                            # calculate HHS of persistent habitats 
  results_metrics <- data.frame(WUA = wua,                    # combine all relevant metrics into a dataframe 
                                WUA_pers = wua_pers, 
                                WA_tot = wa_tot,
                                WA_tot_pers = wa_tot_pers, 
                                HHS = hhs, 
                                HHS_pers = hhs_pers, 
                                River = river, 
                                Discharge = discharge)
  results_freq <- full_join(freq_table, freq_table_pers,      # combine all frequency table results into a dataframe 
                            by = "value")
  results_freq <- cbind(river, discharge, results_freq)
  colnames(results_freq) <- c("River", "Discharge",           # give frequency table dataframe column names 
                              "value", "normal", "persistent")
  results <- list(metrics = results_metrics, freq = results_freq)   # combine the two resulting dataframes into a list object to recall later
}