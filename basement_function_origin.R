
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

#setup --------
bas_setup <- function(model_json, setup_h5){
  
  setup_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_setup.exe"
  
  setup_param1 = paste("-f ", getwd(), "\\", model_json, sep="")
  setup_param2 = paste("-o ", getwd(), "\\", setup_h5, sep="")
  
  system2(setup_cmd_name, args = c(setup_param1, setup_param2))
}


#simulation ---------
bas_simul <- function(simulation_json, setup_h5, results_h5) {
  
  simulation_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_cudaC.exe"
  
  simulation_param1 = paste("-f ", getwd(), "\\", simulation_json, sep="")
  simulation_param2 = paste("-r ", getwd(), "\\", setup_h5, sep="")
  simulation_param3 = paste("-o ", getwd(), "\\", results_h5,  sep="")
  
  
  system2(simulation_cmd_name, args = c(simulation_param1, simulation_param2,
                                        simulation_param3, "-p"))
}

#results ---------
#results_cmd_name <- "c:\\Program Files\\BASEMENT 3.1.1\\bin\\BMv3_BASEplane_results.exe"
bas_results <- function(results_json, results_h5, results_xdmf) {
  
  results_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_results.exe"
  
  results_param1 = paste("-f ", getwd(), "\\", results_json, sep="")
  results_param2 = paste("-r ", getwd(), "\\", results_h5, sep="")
  results_param3 = paste("-o ", getwd(), "\\", results_xdmf, sep="")
  
  system2(results_cmd_name, args = c(results_param1, results_param2, results_param3))
}




#paths------
#INPUT
#model json path
model_json <- "hdm_models/S2/model.json"
#simulation json path
simulation_json <- "hdm_models/S2/simulation.json"
results_json <- "hdm_models/S2/results.json"
#OUTPUT
setup_h5 <- "hdm_models/S2/setup.h5"
results_h5 <- "hdm_models/S2/results_S2_2_00.h5"
#this can actually remain always the same
results_xdmf <- "hdm_models/S2/results_S2_2_00"

#read in model json
model <- fromJSON(model_json)

#modify it
#change mesh
mesh_path <- "hdm_models/S2/computational-mesh-7-290323.2dm"
model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$mesh_file <- mesh_path

#change discharge
model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$discharge[1] <- 2.00

#change Strickler
model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$FRICTION$default_friction <- 36.00

#export 
model_exp <- toJSON(model, pretty = TRUE, auto_unbox = TRUE)
write(model_exp, "hdm_models/S2/model_new.json")




#read in simulation json
simulation <- fromJSON(simulation_json)

#modify it
simulation$SIMULATION$TIME$end <- 3600

#export 
simulation_exp <- toJSON(simulation, pretty = TRUE, auto_unbox = TRUE)
write(simulation_exp, "hdm_models/S2/simulation_new.json")





#RUN BAsement from RStudio
#setup
bas_setup(model_json, setup_h5)
#simulation
bas_simul(simulation_json, setup_h5, results_h5)
#export results
bas_results(results_json, results_h5, results_xdmf)






basement_old <- function(model_json, model_new , mesh_path, simulation_json, simulation_new , setup_h5, results_h5, 
                     results_xdmf, results_json, discharge){
  #paths------
  #INPUT
  #model json path
  model_json <- model_json
  #simulation json path
  simulation_json <- simulation_json
  results_json <- results_json
  #OUTPUT
  setup_h5 <- setup_h5
  results_h5 <- results_h5
  #this can actually remain always the same
  results_xdmf <- results_xdmf
  
  #read in model json
  model <- fromJSON(model_json)
  
  #modify it
  #change mesh
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
  setup_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_setup.exe"
  
  setup_param1 = paste("-f ", getwd(), "\\", model_new, sep="")
  setup_param2 = paste("-o ", getwd(), "\\", setup_h5, sep="")
  
  system2(setup_cmd_name, args = c(setup_param1, setup_param2))


#simulation ---------
  simulation_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_cudaC.exe"
  
  simulation_param1 = paste("-f ", getwd(), "\\", simulation_new, sep="")
  simulation_param2 = paste("-r ", getwd(), "\\", setup_h5, sep="")
  simulation_param3 = paste("-o ", getwd(), "\\", results_h5,  sep="")
  
  
  system2(simulation_cmd_name, args = c(simulation_param1, simulation_param2,
                                        simulation_param3, "-p"))


#results ---------
  results_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_results.exe"
  
  results_param1 = paste("-f ", getwd(), "\\", results_json, sep="")
  results_param2 = paste("-r ", getwd(), "\\", results_h5, sep="")
  results_param3 = paste("-o ", getwd(), "\\", results_xdmf, sep="")
  
  system2(results_cmd_name, args = c(results_param1, results_param2, results_param3))
}






