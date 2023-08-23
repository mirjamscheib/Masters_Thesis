# from input / output
ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[1] == "input", 
       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] <- "input", 
       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] <- "output")

ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[2] == "output", 
       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] <- "output", 
       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] <- "input")

ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[1] == "input", 
       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$string_name[1] <- "input", 
       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$string_name[1] <- "output")

ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[2] == "output", 
       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$string_name[2] <- "output", 
       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$string_name[2] <- "input") 

#change names from STRINGDEF
# from inflow / outflow
#  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[1] == "inflow", 
#        model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] <- "inflow", 
#       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[1] <- "outflow")

#  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[2] == "outflow", 
#        model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] <- "outflow", 
#       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$name[2] <- "inflow")

#  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[1] == "inflow", 
#        model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$string_name[1] <- "inflow", 
#       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$string_name[1] <- "outflow")

#  ifelse(model$SETUP$DOMAIN$BASEPLANE_2D$GEOMETRY$STRINGDEF$name[2] == "outflow", 
#        model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$string_name[2] <- "outflow", 
#       model$SETUP$DOMAIN$BASEPLANE_2D$HYDRAULICS$BOUNDARY$STANDARD$string_name[2] <- "inflow")