# Shiny application for SyncroSim epidemic package COVID-19 output.
# Copyright ? 2007-2020 Apex Resource Management Solutions Ltd. (ApexRMS). All rights reserved.
# The TERMS OF USE and END USER LICENSE AGREEMENT for this software can be found in the LICENSE file.

# This script copies the most up-to-date model outputs from the "data" folder to the "shiny/covid19canada/data" folder

# Set the working directory to the script's folder (works only in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get output files from "data" folder
datafolder = "c:/gitprojects/covid19sim/data"
outputFiles <- list.files(datafolder, pattern="model-output")

# Confirm there are exactly 4 output files
if(!length(outputFiles) == 4){stop(paste("There are", length(outputFiles), "output files, instead of 4"))}

# Copy files to "shiny/covid19canada/data" folder
file.copy(paste0(datafolder,"/", outputFiles), paste0(getwd(),"/covid19canada/data/"), overwrite=T)

# deploy the app
library(rsconnect)
options(rsconnect.http = "curl")
userName = readline(prompt="Enter rsconnect user name: ")
userToken = readline(prompt="Enter rsconnect token: ")
userSecret = readline(prompt="Enter rsconnect secret: ")

rsconnect::setAccountInfo(name=userName, token=userToken, secret=userSecret)

shinyAppDir = "C:/gitprojects/covid19simshiny/src/covid19canada"
rsconnect::deployApp(shinyAppDir)
