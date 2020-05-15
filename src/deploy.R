# Shiny application for SyncroSim epidemic package COVID-19 output.
# Copyright 2007-2020 Apex Resource Management Solutions Ltd. (ApexRMS). All rights reserved.
# The TERMS OF USE and END USER LICENSE AGREEMENT for this software can be found in the LICENSE file.

# This script copies the SyncroSim and IHME model outputs to the "src/covid19canada/data" folder for this repository.
# IHME model outputs are also formatted for consistency with SyncroSim model outputs.

# MANUAL STEPS to perform prior to running this script:
# 1. Download any new IHME model outputs from the IHME website: http://www.healthdata.org/covid/data-downloads
# 2. Unzip the folder downloaded.
# 3. Within the unzipped folder, there should be a unique subfolder. Ensure this subfolder is named after the correct IHME forecast date (format yy_mm_dd).
# 4. Copy this subfolder (thereafter called the "date folder") to your "IHME" local folder (where IHME data for other dates is hosted).
# 5. Check that the date folder contains a CSV called either "hospitalization_all_locs_corrected.csv" or "Hospitalization_all_locs.csv".
# 6. Confirm that column names in this CSV are identical to those of previous dates.
# 7. Drop the new date folder in the R906/IHME folder on S3.

#### Workspace ####
# Packages
library(tidyverse)
library(magrittr)

# Set the working directory to the script's folder (works only in RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data directories
syncroSimDataDir = "C:/gitprojects/covid19sim/data"
ihmeDataDir = "C:/gitprojects/covid19simshiny/src/IHME" # Local folder where the IHME data is located

#### SyncroSim data ####
outputFiles <- list.files(syncroSimDataDir, pattern="model-output")

# Confirm there are exactly 4 output files
if(!length(outputFiles) == 4){stop(paste("There are", length(outputFiles), "output files, instead of 4"))}

# Copy files to "shiny/covid19canada/data" folder
file.copy(paste0(syncroSimDataDir,"/", outputFiles), paste0(getwd(),"/covid19canada/data/"), overwrite=T)

#### IHME data ####
# Compile IHME data form all available dates
ihmeDates <- list.dirs(path = ihmeDataDir, full.names = F, recursive = F)

for(i in ihmeDates){
  ihmeFile <- list.files(paste(ihmeDataDir, i, sep="/"), pattern="ospitalization_all_locs")
  data <- read.csv(paste(ihmeDataDir, i, ihmeFile, sep='/')) # Load
  
  if("date_reported" %in% colnames(data)) data %<>% rename(date = date_reported) # Rename column "date_reported", if present
  
  if(!"est_infections_mean" %in% colnames(data)){
    data %<>% select(location_name, date, deaths_mean, deaths_lower, deaths_upper, totdea_mean, totdea_lower, totdea_upper) %>% # Only keep columns of interest
      mutate(forecastDate = as.Date(gsub("_", "-", i))) %>% # Add forecast date
      mutate(date = as.Date(date)) %>%
      mutate(location_name = as.character(location_name))
  }else{
    data %<>% select(location_name, date, deaths_mean, deaths_lower, deaths_upper, totdea_mean, totdea_lower, totdea_upper, est_infections_mean, est_infections_lower, est_infections_upper) %>% # Only keep columns of interest
      mutate(forecastDate = as.Date(gsub("_", "-", i))) %>% # Add forecast date
      mutate(date = as.Date(date)) %>%
      mutate(location_name = as.character(location_name))
  }
  
  if(!exists("allData")){
    allData <- data
  }else{
    allData %<>% bind_rows(., data)
  }
}
rm(i, data)

# Format output
# Daily deaths
dailyDeaths <- allData %>%
  select(location_name, date, deaths_mean, deaths_lower, deaths_upper, forecastDate) %>%
  rename(Jurisdiction = location_name,
         Date = date,
         Mean = deaths_mean,
         Lower = deaths_lower,
         Upper = deaths_upper,
         date_model_run = forecastDate) %>%
  arrange(Jurisdiction, date_model_run, Date)

# Cumulative deaths
cumulativeDeaths <- allData %>%
  select(location_name, date, totdea_mean, totdea_lower, totdea_upper, forecastDate) %>%
  rename(Jurisdiction = location_name,
         Date = date,
         Mean = totdea_mean,
         Lower = totdea_lower,
         Upper = totdea_upper,
         date_model_run = forecastDate) %>%
  arrange(Jurisdiction, date_model_run, Date)

# Daily infected
dailyInfected <- allData %>%
  select(location_name, date, est_infections_mean, est_infections_lower, est_infections_upper, forecastDate) %>%
  rename(Jurisdiction = location_name,
         Date = date,
         Mean = est_infections_mean,
         Lower = est_infections_lower,
         Upper = est_infections_upper,
         date_model_run = forecastDate) %>%
  filter(!is.na(Mean)) %>% # Remove NA rows
  arrange(Jurisdiction, date_model_run, Date)

# Cumulative infected
cumulativeInfected <- dailyInfected %>%
  arrange(Date) %>%
  group_by(Jurisdiction, date_model_run) %>%
  mutate(Mean = cumsum(Mean),
         Lower = cumsum(Lower),
         Upper = cumsum(Upper)) %>%
  ungroup() %>%
  arrange(Jurisdiction, date_model_run, Date)

# Save
write.csv(dailyDeaths, file=paste0(getwd(),"/covid19canada/data/", "IHME-deaths-daily.csv"), row.names = F)
write.csv(cumulativeDeaths, file=paste0(getwd(),"/covid19canada/data/", "IHME-deaths-cumulative.csv"), row.names = F)
write.csv(dailyInfected, file=paste0(getwd(),"/covid19canada/data/", "IHME-infected-daily.csv"), row.names = F)
write.csv(cumulativeInfected, file=paste0(getwd(),"/covid19canada/data/", "IHME-infected-cumulative.csv"), row.names = F)

#### Deploy the app ####
library(rsconnect)
options(rsconnect.http = "curl")
userName = readline(prompt="Enter rsconnect user name: ")
userToken = readline(prompt="Enter rsconnect token: ")
userSecret = readline(prompt="Enter rsconnect secret: ")

rsconnect::setAccountInfo(name=userName, token=userToken, secret=userSecret)

shinyAppDir = "C:/gitprojects/covid19simshiny/src/covid19canada"
rsconnect::deployApp(shinyAppDir)