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
syncroSimDataDir = "C:/gitprojects/covid19sim/data/"
#syncroSimDataDir = "E:/covid19sim/data/"
ihmeDataDir = "C:/gitprojects/covid19simshiny/src/IHME" # Local folder where the IHME data is located
#ihmeDataDir = "E:/covid19sandbox/src/IHME"

#### SyncroSim data ####
outputFiles <- list.files(syncroSimDataDir, pattern="model-output")

# Confirm there are exactly 4 output files
if(!length(outputFiles) == 4){stop(paste("There are", length(outputFiles), "output files, instead of 4"))}

# Format
dailyDeaths <- read.csv(paste0(syncroSimDataDir, outputFiles[which(grepl("deaths-daily-model-output", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Deaths") %>%
  mutate(Source = "Apex") %>%
  mutate(Jurisdiction = as.character(Jurisdiction))

cumulativeDeaths <- read.csv(paste0(syncroSimDataDir, outputFiles[which(grepl("deaths-cumulative-model-output", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Deaths") %>%
  mutate(Source = "Apex") %>%
  mutate(Jurisdiction = as.character(Jurisdiction))

dailyInfected <- read.csv(paste0(syncroSimDataDir, outputFiles[which(grepl("infected-daily-model-output", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Infections") %>%
  mutate(Source = "Apex") %>%
  mutate(Jurisdiction = as.character(Jurisdiction))

cumulativeInfected <- read.csv(paste0(syncroSimDataDir, outputFiles[which(grepl("infected-cumulative-model-output", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Infections") %>%
  mutate(Source = "Apex") %>%
  mutate(Jurisdiction = as.character(Jurisdiction))

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
    if(as.Date(i,"%Y_%m_%d")<as.Date("2020_06_06","%Y_%m_%d")){
          data %<>% select(location_name, date, deaths_mean, deaths_lower, deaths_upper, totdea_mean, totdea_lower, totdea_upper, est_infections_mean, est_infections_lower, est_infections_upper) %>% # Only keep columns of interest
      mutate(forecastDate = as.Date(gsub("_", "-", i))) %>% # Add forecast date
      mutate(date = as.Date(date)) %>%
      mutate(location_name = as.character(location_name))
    }else{
      data %<>% select(location_name, date, deaths_mean_smoothed, deaths_lower_smoothed, deaths_upper_smoothed, totdea_mean_smoothed, totdea_lower_smoothed, totdea_upper_smoothed, est_infections_mean, est_infections_lower, est_infections_upper) %>% # Only keep columns of interest
        mutate(forecastDate = as.Date(gsub("_", "-", i))) %>% # Add forecast date
        mutate(date = as.Date(date)) %>%
        mutate(location_name = as.character(location_name))
      data = rename(data, deaths_mean = deaths_mean_smoothed)
      data = rename(data, deaths_lower = deaths_lower_smoothed)
      data = rename(data, deaths_upper = deaths_upper_smoothed)
      data = rename(data, totdea_mean = totdea_mean_smoothed)
      data = rename(data, totdea_lower = totdea_lower_smoothed)
      data = rename(data, totdea_upper = totdea_upper_smoothed)
    }

  }
  
  if(!exists("allData_IHME")){
    allData_IHME <- data
  }else{
    allData_IHME %<>% bind_rows(., data)
  }
}
rm(i, data)

# Format output
      # Daily deaths
dailyDeaths_IHME <- allData_IHME %>%
  select(location_name, date, deaths_mean, deaths_lower, deaths_upper, forecastDate) %>%
  rename(Jurisdiction = location_name,
         Date = date,
         Mean = deaths_mean,
         Lower = deaths_lower,
         Upper = deaths_upper,
         date_model_run = forecastDate) %>%
  arrange(Jurisdiction, date_model_run, Date) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Deaths") %>%
  mutate(Source = "IHME") %>%
  mutate(Jurisdiction = ifelse(Jurisdiction == "Canada", "Canada", paste0("Canada - ", Jurisdiction))) %>% # Standardize jurisdiction name
  filter(Jurisdiction %in% dailyDeaths$Jurisdiction) %>% # Only retain jurisdictions that are in the Apex datasets
  mutate(Jurisdiction = as.character(Jurisdiction))

      # Cumulative deaths
cumulativeDeaths_IHME <- allData_IHME %>%
  select(location_name, date, totdea_mean, totdea_lower, totdea_upper, forecastDate) %>%
  rename(Jurisdiction = location_name,
         Date = date,
         Mean = totdea_mean,
         Lower = totdea_lower,
         Upper = totdea_upper,
         date_model_run = forecastDate) %>%
  arrange(Jurisdiction, date_model_run, Date) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Deaths") %>%
  mutate(Source = "IHME") %>%
  mutate(Jurisdiction = ifelse(Jurisdiction == "Canada", "Canada", paste0("Canada - ", Jurisdiction))) %>% # Standardize jurisdiction name
  filter(Jurisdiction %in% dailyDeaths$Jurisdiction) %>% # Only retain jurisdictions that are in the Apex datasets
  mutate(Jurisdiction = as.character(Jurisdiction))

      # Daily infected
dailyInfected_IHME <- allData_IHME %>%
  select(location_name, date, est_infections_mean, est_infections_lower, est_infections_upper, forecastDate) %>%
  rename(Jurisdiction = location_name,
         Date = date,
         Mean = est_infections_mean,
         Lower = est_infections_lower,
         Upper = est_infections_upper,
         date_model_run = forecastDate) %>%
  filter(!is.na(Mean)) %>% # Remove NA rows
  arrange(Jurisdiction, date_model_run, Date) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Infections") %>%
  mutate(Source = "IHME") %>%
  mutate(Jurisdiction = ifelse(Jurisdiction == "Canada", "Canada", paste0("Canada - ", Jurisdiction))) %>% # Standardize jurisdiction name
  filter(Jurisdiction %in% dailyDeaths$Jurisdiction) %>% # Only retain jurisdictions that are in the Apex datasets
  mutate(Jurisdiction = as.character(Jurisdiction))

      # Cumulative infected
cumulativeInfected_IHME <- dailyInfected_IHME %>%
  arrange(Date) %>%
  group_by(Jurisdiction, date_model_run) %>%
  mutate(Mean = cumsum(Mean),
         Lower = cumsum(Lower),
         Upper = cumsum(Upper)) %>%
  ungroup() %>%
  arrange(Jurisdiction, date_model_run, Date) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Infections") %>%
  mutate(Source = "IHME") %>%
  filter(Jurisdiction %in% dailyDeaths$Jurisdiction) %>% # Only retain jurisdictions that are in the Apex datasets
  mutate(Jurisdiction = as.character(Jurisdiction))

#### Combine datasets ####
# General
data <- bind_rows(dailyDeaths, dailyInfected, cumulativeDeaths, cumulativeInfected, dailyDeaths_IHME, dailyInfected_IHME, cumulativeDeaths_IHME, cumulativeInfected_IHME) %>% # Bind data
  mutate(DataType = ifelse((Metric %in% c("Daily Deaths", "Cumulative Deaths")) & (Date < date_model_run), "Observed", "Modeled")) %>%
  mutate(Metric = ordered(Metric, levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths"))) %>%
  filter(!(DataType == "Observed" & Source == "IHME")) # Remove IHME observations

# Duplicate last observed date to make it also the first modeled date
firstModeledApex <- data %>%
  filter(DataType == "Observed") %>% # Keep only observations
  filter(Date == date_model_run - 1) %>% # Keep only data for the day before a model run
  mutate(DataType = "Modeled") %>% # Assign it as modeled data
  mutate(Lower = Mean, Upper = Mean) # Assign lower and upper bounds = mean

firstModeledIHME <- firstModeledApex %>%
  mutate(Source = "IHME") %>%
  filter(Jurisdiction %in% c(dailyDeaths_IHME$Jurisdiction, cumulativeDeaths_IHME$Jurisdiction, dailyInfected_IHME$Jurisdiction, cumulativeInfected_IHME$Jurisdiction)) %>%
  filter(date_model_run %in% dailyDeaths_IHME$date_model_run) # Only retain date_model_run that exist in the IHME dataset

# Add to master dataset
data %<>% bind_rows(., firstModeledApex, firstModeledIHME) %>%
  arrange(Metric, Jurisdiction, date_model_run, Date, Source) %>%
  mutate(DataTag = ifelse(DataType == "Observed", "Observed", ifelse(Source == "Apex", "Apex projection", "IHME projection"))) %>%
  mutate(DataTag = ordered(DataTag, level=c("IHME projection", "Apex projection", "Observed")))

# Filter out unecessary rows
obsDate <- data %>% # date_model_run to use for Observation data
  filter(Source == "Apex") %>%
  pull(date_model_run) %>%
  max()

data %<>% filter(!((DataType == "Observed") & (!date_model_run == obsDate))) # Remove observations for all but the most recent model

# Save
write.csv(data, file=paste0(getwd(),"/covid19canada/data/", "data.csv"), row.names = F)

#### Deploy the app ####
library(rsconnect)
options(rsconnect.http = "curl")
userName = readline(prompt="Enter rsconnect user name: ")
#userToken = readline(prompt="Enter rsconnect token: ")
#userSecret = readline(prompt="Enter rsconnect secret: ")

rsconnect::setAccountInfo(name=userName, token=userToken, secret=userSecret)

shinyAppDir = "C:/gitprojects/covid19simshiny/src/covid19canada"
rsconnect::deployApp(shinyAppDir)