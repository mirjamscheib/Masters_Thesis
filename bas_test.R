#
#title: "R2bas_func.r"
#



#setup --------
bas_setup <- function(model_json, setup_h5){
  
  setup_cmd_name <- "c:\\Program Files\\BASEMENT 3.1.1\\bin\\BMv3_BASEplane_setup.exe"
  
  setup_param1 = paste("-f ", getwd(), "\\", model_json, sep="")
  setup_param2 = paste("-o ", getwd(), "\\", setup_h5, sep="")
  
  system2(setup_cmd_name, args = c(setup_param1, setup_param2))
}


#simulation ---------
bas_simul <- function(simulation_json, setup_h5, results_h5) {
  
  simulation_cmd_name <- "c:\\Program Files\\BASEMENT 3.1.1\\bin\\BMv3_BASEplane_cudaC.exe"
  
  simulation_param1 = paste("-f ", getwd(), "\\", simulation_json, sep="")
  simulation_param2 = paste("-r ", getwd(), "\\", setup_h5, sep="")
  simulation_param3 = paste("-o ", getwd(), "\\", results_h5,  sep="")
  
  
  system2(simulation_cmd_name, args = c(simulation_param1, simulation_param2,
                                        simulation_param3, "-p"))
}

#results ---------
#results_cmd_name <- "c:\\Program Files\\BASEMENT 3.1.1\\bin\\BMv3_BASEplane_results.exe"
bas_results <- function(results_json, results_h5, results_xdmf) {
  
  results_cmd_name <- "c:\\Program Files\\BASEMENT 3.1.1\\bin\\BMv3_BASEplane_results.exe"
  
  results_param1 = paste("-f ", getwd(), "\\", results_json, sep="")
  results_param2 = paste("-r ", getwd(), "\\", results_h5, sep="")
  results_param3 = paste("-o ", getwd(), "\\", results_xdmf, sep="")
  
  system2(results_cmd_name, args = c(results_param1, results_param2, results_param3))
}














