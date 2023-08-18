
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
bas_setup <- function(model_json, setup_h5, simulation_json, results_h5, results_json, results_xdmf, 
                      simulation_new, model_new, mesh_path, strickler, discharge){
  #read in model json
  model <- fromJSON(model_json)
  
  #modify it
  #change mesh
  model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$mesh_file <- mesh_path
  
  #change names
  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[1] == "input", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] <- "input", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] <- "output")
  
  #change names
  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[2] == "output", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] <- "output", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] <- "input")
  
  #change names
  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[1] == "inflow", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] <- "input", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] <- "output") 
  
  #change names
  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[2] == "outflow", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] <- "output", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] <- "input")
  
  #change discharge
  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] == "input", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$discharge[1] <- discharge, 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$discharge[1] <- NA) 
  
  # change input
  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] == "input", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$type[1] <- "uniform_in", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$type[1] <- "uniform_out") 
  
  # change input
  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] == "output", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$type[2] <- "uniform_out", 
         model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$type[2] <- "uniform_in") 
  
  #change Strickler
  model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$FRICTION$default_friction <- strickler
  
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

  # Set up Basement 
  setup_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_setup.exe"
  
  setup_param1 = paste("-f ", getwd(), "\\", model_new, sep="")
  setup_param2 = paste("-o ", getwd(), "\\", setup_h5, sep="")
  
  system2(setup_cmd_name, args = c(setup_param1, setup_param2))

# Simulation
  simulation_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_cudaC.exe"
  
  simulation_param1 = paste("-f ", getwd(), "\\", simulation_json, sep="")
  simulation_param2 = paste("-r ", getwd(), "\\", setup_h5, sep="")
  simulation_param3 = paste("-o ", getwd(), "\\", results_h5,  sep="")
  
  system2(simulation_cmd_name, args = c(simulation_param1, simulation_param2,
                                        simulation_param3, "-p"))

# Export Results
  results_cmd_name <- "c:\\Programme\\BASEMENT 3.2.0\\bin\\BMv3_BASEplane_results.exe"
  
  results_param1 = paste("-f ", getwd(), "\\", results_json, sep="")
  results_param2 = paste("-r ", getwd(), "\\", results_h5, sep="")
  results_param3 = paste("-o ", getwd(), "\\", results_xdmf, sep="")
  
  system2(results_cmd_name, args = c(results_param1, results_param2, results_param3))
}



#RUN BAsement from RStudio
#setup
bas_setup(model_json = "hdm_models/S2/model.json", 
          setup_h5 = "hdm_models/S2/setup.h5", 
          simulation_json = "hdm_models/S2/simulation.json",
          results_h5 = "hdm_models/S2/results_S2_20_00.h5", 
          results_json = "hdm_models/S2/results.json",
          results_xdmf = "hdm_models/S2/results_S2_20_00", 
          simulation_new = "hdm_models/S2/simulation_new.json", 
          model_new = "hdm_models/S2/model_new.json",
          mesh_path = "hdm_models/S2/computational-mesh-7-290323.2dm",
          discharge = 20.00, 
          strickler = 36.0)

GL1_31_76 <- basement(model_json = "hdm_models/GL1/model.json",
                      model_new = "hdm_models/GL1/model_new.json",
                      mesh_path = "hdm_models/GL1/GL1_mesh_computational-mesh_3.2dm", 
                      simulation_json = "hdm_models/GL1/simulation.json",
                      simulation_new = "hdm_models/GL1/simulation_new.json",
                      setup_h5 = "hdm_models/GL1/setup.h5",
                      results_h5 = "hdm_models/GL1/results_GL1_31_76.h5",
                      results_xdmf = "hdm_models/GL1/results_GL1_31_76",
                      results_json = "hdm_models/GL1/results.json",
                      discharge = 31.76, 
                      strickler = 38, 
                      time_end = 3600,
                      slope = 0.01)



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





