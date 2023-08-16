
rm(list = ls())

# load all Packages ---
library(hdf5r)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(Metrics)
library(raster)
library(rgeos)
library(hydroGOF)
library(jsonlite)


basement_old <- function(model_json, model_new , mesh_path, simulation_json, simulation_new , setup_h5, results_h5, 
                     results_xdmf, results_json, discharge){
#read in model json
model <- fromJSON(model_json)
model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$mesh_file <- mesh_path

#change discharge
model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$discharge[1] <- discharge

#change Strickler
# model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$FRICTION$default_friction <- 38

#export 
model_exp <- toJSON(model, pretty = TRUE, auto_unbox = TRUE)
write(model_exp, model_new)


#read in simulation json
simulation <- fromJSON(simulation_json)

#modify it
simulation$SIMULATION$TIME$end <- 3600

#export 
simulation_exp <- toJSON(simulation, pretty = TRUE, auto_unbox = TRUE)
write(simulation_exp, simulation_new)

#setup --------
  setup_cmd_name <- "c:\\Program Files\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_setup.exe"
  
  setup_param1 = paste("-f ", getwd(), "\\", model_new, sep="") # oder model_json
  setup_param2 = paste("-o ", getwd(), "\\", setup_h5, sep="")
  
  system2(setup_cmd_name, args = c(setup_param1, setup_param2))


#simulation ---------
  simulation_cmd_name <- "c:\\Program Files\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_cudaC.exe"
  
  simulation_param1 = paste("-f ", getwd(), "\\", simulation_new, sep="") # oder simulation_json
  simulation_param2 = paste("-r ", getwd(), "\\", setup_h5, sep="")
  simulation_param3 = paste("-o ", getwd(), "\\", results_h5,  sep="")
  
  
  system2(simulation_cmd_name, args = c(simulation_param1, simulation_param2,
                                        simulation_param3, "-p"))

#results ---------
  results_cmd_name <- "c:\\Program Files\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_results.exe"
  
  results_param1 = paste("-f ", getwd(), "\\", results_json, sep="")
  results_param2 = paste("-r ", getwd(), "\\", results_h5, sep="")
  results_param3 = paste("-o ", getwd(), "\\", results_xdmf, sep="")
  
  system2(results_cmd_name, args = c(results_param1, results_param2, results_param3))
}


