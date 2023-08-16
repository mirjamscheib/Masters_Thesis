
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

source("U:/MZB/04_R_mzb/R2bas_func.r")

# set working directory ------
setwd("u:/MZB/03_modelle/7_glenner/GL2/3_basement")

#paths------
#INPUT
#model json path
model_json <- "model.json"
#simulation json path
simulation_json <- "simulation.json"
results_json <- "results.json"
#OUTPUT
setup_h5 <- "setup.h5"
results_h5 <- "results.h5"
#this can actually remain always the same
results_xdmf <- "results"

#read in model json
model <- fromJSON(model_json)

#modify it
#change mesh
mesh_path <- "U:/MZB/03_modelle/7_glenner/GL2/3_basement/Gl2_computational-mesh_gcp2_mod.2dm"
model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$mesh_file <- mesh_path

#change discharge
model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$discharge[1] <- 11

#change Strickler
model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$FRICTION$default_friction <- 38

#export 
model_exp <- toJSON(model, pretty = TRUE, auto_unbox = TRUE)
write(model_exp, "model_new.json")




#read in simulation json
simulation <- fromJSON(simulation_json)

#modify it
simulation$SIMULATION$TIME$end <- 3600

#export 
simulation_exp <- toJSON(simulation, pretty = TRUE, auto_unbox = TRUE)
write(simulation_exp, "simulation_new.json")





#RUN BAsement from RStudio
#setup
bas_setup(model_json, setup_h5)
#simulation
bas_simul(simulation_json, setup_h5, results_h5)
#export results
bas_results(results_json, results_h5, results_xdmf)





