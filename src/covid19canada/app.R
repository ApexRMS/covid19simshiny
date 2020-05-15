# Shiny application for SyncroSim epidemic package COVID-19 output.
# Copyright ? 2007-2020 Apex Resource Management Solutions Ltd. (ApexRMS). All rights reserved.
# The TERMS OF USE and END USER LICENSE AGREEMENT for this software can be found in the LICENSE file.

#### Workspace ####
# Packages
#remotes::install_github("rstudio/thematic")
#remotes::install_github("rstudio/shiny#2740")

library(tidyverse)
library(magrittr)
library(shiny)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(shinyWidgets)
library(cowplot)


library(thematic)
library(shinythemes)
library(shinyBS)

# Set the working directory to the script's folder (works only in RStudio)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Turn on automatic theming
thematic_on()
onStop(thematic_off)

# Input parameters
brewer.pal(n = 8, name = "Dark2")
sourceLineColor <- c("Observed"="#7B7B7B", "ApexRMS projection"="#7d1428", "IHME projection"="#7cb961")

# Output files
outputFiles <- list.files("data")

# Load data
# ApexRMS data
dailyDeaths <- read.csv(paste0("data/", outputFiles[which(grepl("deaths-daily-model-output", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Deaths") %>%
  mutate(Source = "ApexRMS") %>%
  mutate(Jurisdiction = as.character(Jurisdiction))

cumulativeDeaths <- read.csv(paste0("data/", outputFiles[which(grepl("deaths-cumulative-model-output", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Deaths") %>%
  mutate(Source = "ApexRMS") %>%
  mutate(Jurisdiction = as.character(Jurisdiction))

dailyInfected <- read.csv(paste0("data/", outputFiles[which(grepl("infected-daily-model-output", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Infections") %>%
  mutate(Source = "ApexRMS") %>%
  mutate(Jurisdiction = as.character(Jurisdiction))

cumulativeInfected <- read.csv(paste0("data/", outputFiles[which(grepl("infected-cumulative-model-output", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Infections") %>%
  mutate(Source = "ApexRMS") %>%
  mutate(Jurisdiction = as.character(Jurisdiction))

# IHME data
dailyDeaths_IHME <- read.csv(paste0("data/", outputFiles[which(grepl("IHME-deaths-daily", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Deaths") %>%
  mutate(Source = "IHME") %>%
  mutate(Jurisdiction = ifelse(Jurisdiction == "Canada", "Canada", paste0("Canada - ", Jurisdiction))) %>% # Standardize jurisdiction name
  filter(Jurisdiction %in% dailyDeaths$Jurisdiction) %>% # Only retain jurisdictions that are in the ApexRMS datasets
  mutate(Jurisdiction = as.character(Jurisdiction))

cumulativeDeaths_IHME <- read.csv(paste0("data/", outputFiles[which(grepl("IHME-deaths-cumulative", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Deaths") %>%
  mutate(Source = "IHME") %>%
  mutate(Jurisdiction = ifelse(Jurisdiction == "Canada", "Canada", paste0("Canada - ", Jurisdiction))) %>% # Standardize jurisdiction name
  filter(Jurisdiction %in% dailyDeaths$Jurisdiction) %>% # Only retain jurisdictions that are in the ApexRMS datasets
  mutate(Jurisdiction = as.character(Jurisdiction))

dailyInfected_IHME <- read.csv(paste0("data/", outputFiles[which(grepl("IHME-infected-daily", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Daily Infections") %>%
  mutate(Source = "IHME") %>%
  mutate(Jurisdiction = ifelse(Jurisdiction == "Canada", "Canada", paste0("Canada - ", Jurisdiction))) %>% # Standardize jurisdiction name
  filter(Jurisdiction %in% dailyDeaths$Jurisdiction) %>% # Only retain jurisdictions that are in the ApexRMS datasets
  mutate(Jurisdiction = as.character(Jurisdiction))

cumulativeInfected_IHME <- read.csv(paste0("data/", outputFiles[which(grepl("IHME-infected-cumulative", outputFiles))])) %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = "Cumulative Infections") %>%
  mutate(Source = "IHME") %>%
  mutate(Jurisdiction = ifelse(Jurisdiction == "Canada", "Canada", paste0("Canada - ", Jurisdiction))) %>% # Standardize jurisdiction name
  filter(Jurisdiction %in% dailyDeaths$Jurisdiction) %>% # Only retain jurisdictions that are in the ApexRMS datasets
  mutate(Jurisdiction = as.character(Jurisdiction))

# Format data
# General
data <- bind_rows(dailyDeaths, dailyInfected, cumulativeDeaths, cumulativeInfected, dailyDeaths_IHME, dailyInfected_IHME, cumulativeDeaths_IHME, cumulativeInfected_IHME) %>% # Bind data
  mutate(DataType = ifelse((Metric %in% c("Daily Deaths", "Cumulative Deaths")) & (Date < date_model_run), "Observed", "Modeled")) %>%
  mutate(Metric = ordered(Metric, levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths"))) %>%
  filter(!(DataType == "Observed" & Source == "IHME")) # Remove IHME observations

# Duplicate last observed date to make it also the first modeled date
firstModeledApexRMS <- data %>%
  filter(DataType == "Observed") %>% # Keep only observations
  filter(Date == date_model_run - 1) %>% # Keep only data for the day before a model run
  mutate(DataType = "Modeled") %>% # Assign it as modeled data
  mutate(Lower = Mean, Upper = Mean) # Assign lower and upper bounds = mean

firstModeledIHME <- firstModeledApexRMS %>%
  mutate(Source = "IHME") %>%
  filter(date_model_run %in% dailyDeaths_IHME$date_model_run) # Only retain date_model_run that exist in the IHME dataset

# Add to master dataset
data %<>% bind_rows(., firstModeledApexRMS, firstModeledIHME) %>%
  arrange(Metric, Jurisdiction, date_model_run, Date, Source) %>%
  mutate(DataTag = ifelse(DataType == "Observed", "Observed", ifelse(Source == "ApexRMS", "ApexRMS projection", "IHME projection"))) %>%
  mutate(DataTag = ordered(DataTag, level=c("IHME projection", "ApexRMS projection", "Observed")))

#### Helpers ####
forecastDates <- sort(unique(data$date_model_run))

defaultForecastDate <- max(data$date_model_run[which(data$Source == "ApexRMS")])

jurisdictions <- sort(unique(data$Jurisdiction))

obsDate <- data %>% # date_model_run to use for Observation data
  filter(Source == "ApexRMS") %>%
  pull(date_model_run) %>%
  max()

minDate <- min(data$Date[which(data$Source == "ApexRMS")])
maxDate <- max(data$Date[which(data$Source == "ApexRMS")])

whiteTheme <- theme(panel.background = element_rect(fill = "#f9f9f9"),
                    panel.border = element_rect(fill = NA, color = "grey75"),
                    axis.ticks = element_line(color = "grey55"),
                    panel.grid.major = element_line(color = "grey85", size = 0.2),
                    panel.grid.minor = element_line(color = "grey85", size = 0.2),
                    plot.title = element_text(hjust=0.5, size=18),
                    axis.title = element_text(size=18),
                    strip.text = element_text(size=18, color="white", margin=margin(t=10, b=10)),
                    strip.background = element_rect(fill="#7d1428"),
                    axis.text = element_text(size=14),
                    legend.key = element_rect(fill = NA),
                    legend.text = element_text(size=14,margin = margin(r = 30, unit = "pt")),
                    legend.title = element_blank())

#### UI ####
ui <- fluidPage(title = "COVID-19 SyncroSim",
                
                tags$head(tags$style(HTML("
                                          a {
                                          color: #7d1428;}
                                          
                                          a:hover{
                                          color: #7d1428;}
                                          
                                          .nav-pills>li.active>a, .nav-pills>li.active>a:hover, .nav-pills>li.active>a:focus{
                                          background-color: #7d1428;;
                                          color: #ffffff;
                                          padding-right: 10px;
                                          padding-left: 10px;}
                                          
                                          .well {
                                          background-color: #f9f9f9}"))),
                
                theme = shinythemes::shinytheme("flatly"),
                
                titlePanel(column(width = 12,
                                  a(img(src = "SyncroSim-Logo.png"), href="https://syncrosim.com"))),
                
                br(),
                
                sidebarLayout(sidebarPanel(width=3,
                                           dateInput("forecastDate",
                                                     label = "Forecast Date",
                                                     value = defaultForecastDate,
                                                     min = min(forecastDates),
                                                     max = max(forecastDates),
                                                     format = "yyyy-mm-dd",
                                                     startview = "month"),
                                           
                                           bsTooltip("forecastDate", "Choose date of model run and show forecasts", placement="right"),
                                           
                                           selectInput("juris",
                                                       label = "Select Jurisdiction",
                                                       choices = jurisdictions,
                                                       selected = jurisdictions[1]),
                                           
                                           bsTooltip("juris", "Select the jurisdiction to plot", placement="right"),
                                           
                                           fluidRow(column(8,
                                                           checkboxGroupInput("models",
                                                                              label = "Select Models",
                                                                              choices = c("ApexRMS" = "ApexRMS", "IHME" = "IHME"),
                                                                              selected = c("ApexRMS", "IHME"))),
                                                    column(2,
                                                           br(),
                                                           actionButton(inputId='ApexRMSLink',
                                                                        label="",
                                                                        icon = icon("info-circle"),
                                                                        onclick ="window.open('http://www.apexrms.com/covid19/', '_blank')",
                                                                        style = "color: white;
                                                                        background-color: #7d1428;
                                                                        height: 25px;
                                                                        width: 25px;
                                                                        padding: 0px;
                                                                        border-radius: 20%;
                                                                        border-width: 0px"),
                                                           
                                                           br(),br(),
                                                           
                                                           actionButton(inputId='IHMELink',
                                                                        label="",
                                                                        icon = icon("info-circle"),
                                                                        onclick ="window.open('http://www.healthdata.org/covid/', '_blank')",
                                                                        style = "color: white;
                                                                        background-color: #7cb961;
                                                                        height: 25px;
                                                                        width: 25px;
                                                                        padding: 0px;
                                                                        border-radius: 20%;
                                                                        border-width: 0px"))),
                                           
                                           bsTooltip("models", "Select one or more models to plot", placement="right"),
                                           
                                           bsTooltip("ApexRMSLink", "ApexRMS model details", placement="right"),
                                           
                                           bsTooltip("IHMELink", "IHME model details", placement="right"),
                                           
                                           materialSwitch("logY",
                                                          label = "Log Y axis",
                                                          value = F,
                                                          status = "primary",
                                                          width="100%"),
                                           
                                           bsTooltip("logY", "Plot data on a log scale", placement="right"),
                                           
                                           sliderInput("range", width="100%", label = "Date Range",
                                                       min = minDate, max = maxDate, value = c(minDate, maxDate), 
                                                       step = 1),
                                           
                                           bsTooltip("range", "Select a range of dates to plot forecasts", placement="right"),
                                           
                                           hr(),
                                           
                                           p(strong("Note that the simulation results presented here are simply a demonstration of the model framework, and should not be considered actual predictions for any of these jurisdictions."))),
                              
                              mainPanel(width=9,
                                        
                                        titlePanel(h2("COVID-19 Forecasts For Canada", align="center")),
                                        
                                        tabsetPanel(type = c("pills"),
                                                    tabPanel("Deaths",
                                                             fluidRow(column(12, align="center",
                                                                             plotOutput("deathChart", width="100%", height="900px")))),
                                                    tabPanel("Infections",
                                                             fluidRow(column(12, align="center",
                                                                             plotOutput("infectionChart", width="100%", height="900px"))))))))

#### Server ####
server <- function(input, output) {
  
  output$deathChart <- renderPlot({
    # Get most recent IHME model date
    lastIHMEdate <- data %>%
      filter(Source == "IHME") %>%
      filter(date_model_run <= input$forecastDate) %>%
      pull(date_model_run) %>%
      max()
    
    # Get oldest IHME model date
    oldestIHMEdate <- data %>%
      filter(Source == "IHME") %>%
      filter(Metric %in% c("Daily Deaths", "Cumulative Deaths")) %>%
      pull(date_model_run) %>%
      min()
    
    # Subset data based on user inputs
    dataSubset <- data %>% filter(Metric %in% c("Daily Deaths", "Cumulative Deaths")) %>% # Deaths only
      filter(Jurisdiction %in% input$juris) %>% # Only keep jurisdiction of interest
      filter(!((DataType == "Observed") & (!date_model_run == obsDate))) %>% # Remove observations for all but the most recent model
      filter(!((DataTag == "ApexRMS projection") & (!date_model_run == input$forecastDate))) %>% # Remove ApexRMS predictions for all but the model run of interest
      filter(!((DataTag == "IHME projection") & (!date_model_run == lastIHMEdate))) %>% # Remove IHME predictions for all but the model run of interest
      filter(!((DataType == "Modeled") & (!Source %in% input$models))) %>% # Only keep models of interest
      filter(Date >= input$range[1] & Date <= input$range[2]) %>% # Only keep dates of interest
      arrange(Metric, Jurisdiction, Date)
    
    # Produce main plot (without legend)
    plot <- ggplot(dataSubset, aes(x=Date, y=Mean)) + 
      geom_line(size=1, aes(color=DataTag)) +
      geom_ribbon(data=dataSubset[which(dataSubset$DataType=="Modeled"),], aes(ymin=Lower, ymax=Upper, fill=DataTag), alpha=0.4, color=NA, show.legend=F) +
      scale_color_manual(values=sourceLineColor) +
      scale_fill_manual(values=sourceLineColor) +
      guides(color=F, linetype=F) +
      scale_y_continuous(name="Number of people", labels=scales::label_comma(), trans=ifelse(input$logY, "log10", "identity")) +
      whiteTheme +
      facet_wrap(vars(Metric), scales="free_y", ncol=1) +
      theme(axis.title.x = element_blank(),
            plot.margin=unit(c(5,5,10,5),"pt"),
            strip.text = element_text(size=16))
    
    # Add error message if IHME data is missing
    if("IHME" %in% input$models){
      
      # If there is no earlier IHME model
      if(input$forecastDate < oldestIHMEdate){
        notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = input$range[1])
        notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
        plot <- plot +
          geom_text(data = notes,
                    aes(x=x, y=y),
                    color="#7B7B7B",
                    hjust=0,
                    vjust=1,
                    size=5,
                    label = paste0("No IHME projection available prior to ", oldestIHMEdate, "."))
        
        # If there is an earlier IHME model        
      }else if(!lastIHMEdate == input$forecastDate){
        notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = input$range[1])
        notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
        plot <- plot +
          geom_text(data = notes,
                    aes(x=x, y=y),
                    color="#7B7B7B",
                    hjust=0,
                    vjust=1,
                    size=5,
                    label = paste("No IHME projection available for the selected forecast date.\nShowing the IHME projection from", lastIHMEdate, "instead."))
      }
    }
    
    # Produce legend for data tags
    tagLegend <- ggplot(dataSubset, aes(x=Date, y=Mean, color=DataTag)) +
      geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=DataTag), alpha=0.4, color=NA) +
      geom_line(size=1) +
      scale_color_manual(values=sourceLineColor, labels=c("Observed"="Observed", "ApexRMS projection"='ApexRMS', "IHME projection"='IHME'), breaks = c("Observed", "ApexRMS projection", "IHME projection")) +
      scale_fill_manual(values=c("Observed"="#ffffff", "ApexRMS projection"="#7d1428", "IHME projection"="#7cb961"), labels=c("Observed"="Observed", "ApexRMS projection"='ApexRMS', "IHME projection"='IHME'), breaks = c("Observed", "ApexRMS projection", "IHME projection")) +
      whiteTheme +
      theme(legend.position = "top",
            legend.justification = "center",
            legend.margin=margin(0,0,0,0),
            legend.key.width = unit(2,"cm"))
    
    tagLegend <- get_legend(tagLegend)
    
    # Combine plot and legends
    p <- plot_grid(tagLegend, plot, ncol=1, rel_heights = c(1,30))
    return(p)
  })
  
  output$infectionChart <- renderPlot({
    validate(need(input$models, 'Select at least one model'))
    
    # Get most recent IHME model date
    lastIHMEdate <- data %>%
      filter(Source == "IHME") %>%
      filter(date_model_run <= input$forecastDate) %>%
      pull(date_model_run) %>%
      max()
    
    # Get oldest IHME model date
    oldestIHMEdate <- data %>%
      filter(Source == "IHME") %>%
      filter(Metric %in% c("Daily Infections", "Cumulative Infections")) %>%
      pull(date_model_run) %>%
      min()
    
    # Subset data based on user inputs
    dataSubset <- data %>% filter(Metric %in% c("Daily Infections", "Cumulative Infections")) %>% # Deaths only
      filter(Jurisdiction %in% input$juris) %>% # Only keep jurisdiction of interest
      filter(!((DataType == "Observed") & (!date_model_run == obsDate))) %>% # Remove observations for all but the most recent model
      filter(!((DataTag == "ApexRMS projection") & (!date_model_run == input$forecastDate))) %>% # Remove ApexRMS predictions for all but the model run of interest
      filter(!((DataTag == "IHME projection") & (!date_model_run == lastIHMEdate))) %>% # Remove IHME predictions for all but the model run of interest
      filter(!((DataType == "Modeled") & (!Source %in% input$models))) %>% # Only keep models of interest
      filter(Date >= input$range[1] & Date <= input$range[2]) %>% # Only keep dates of interest
      arrange(Metric, Jurisdiction, Date)
    
    # Produce main plot (without legend)
    plot <- ggplot(dataSubset, aes(x=Date, y=Mean, color=DataTag)) +
      geom_line(size=1) +
      geom_ribbon(data=dataSubset[which(dataSubset$DataType=="Modeled"),], aes(ymin=Lower, ymax=Upper, fill=DataTag), alpha=0.4, color=NA, show.legend=F) +
      scale_color_manual(values=sourceLineColor) +
      scale_fill_manual(values=sourceLineColor) +
      guides(color=F, linetype=F) +
      scale_y_continuous(name="Number of people", labels=scales::label_comma(), trans=ifelse(input$logY, "log10", "identity")) +
      whiteTheme +
      facet_wrap(vars(Metric), scales="free_y", ncol=1) +
      theme(axis.title.x = element_blank(),
            plot.margin=unit(c(5,5,10,5),"pt"),
            strip.text = element_text(size=16))
    
    # Add error message if IHME data is missing
    if("IHME" %in% input$models){
      
      # If there is no earlier IHME model
      if(input$forecastDate < oldestIHMEdate){
        notes <- data.frame(Metric = ordered(c("Daily Infections", "Cumulative Infections"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = input$range[1])
        notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
        plot <- plot +
          geom_text(data = notes,
                    aes(x=x, y=y),
                    color="#7B7B7B",
                    hjust=0,
                    vjust=1,
                    size=5,
                    label = paste0("No IHME projection available prior to ", oldestIHMEdate, "."))
        
        # If there is an earlier IHME model        
      }else if(!lastIHMEdate == input$forecastDate){
        notes <- data.frame(Metric = ordered(c("Daily Infections", "Cumulative Infections"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = input$range[1])
        notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
        plot <- plot +
          geom_text(data = notes,
                    aes(x=x, y=y),
                    color="#7B7B7B",
                    hjust=0,
                    vjust=1,
                    size=5,
                    label = paste("No IHME projection available for the selected forecast date.\nShowing the IHME projection from", lastIHMEdate, "instead."))
      }
    }
    
    # Produce legend for data tags
    tagLegend <- ggplot(dataSubset, aes(x=Date, y=Mean, color=DataTag)) +
      geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=DataTag), alpha=0.4, color=NA) +
      geom_line(size=1) +
      scale_color_manual(values=sourceLineColor, labels=c("Observed"="Observed", "ApexRMS projection"='ApexRMS (95% Confidence Interval)', "IHME projection"='IHME (95% Confidence Interval)'), breaks = c("Observed", "ApexRMS projection", "IHME projection")) +
      scale_fill_manual(values=c("Observed"="#ffffff", "ApexRMS projection"="#7d1428", "IHME projection"="#7cb961"), labels=c("Observed"="Observed", "ApexRMS projection"='ApexRMS (95% Confidence Interval)', "IHME projection"='IHME (95% Confidence Interval)'), breaks = c("Observed", "ApexRMS projection", "IHME projection")) +
      whiteTheme +
      theme(legend.position = "top",
            legend.justification = "center",
            legend.margin=margin(0,0,0,0),
            legend.key.width = unit(2,"cm"))
    
    tagLegend <- get_legend(tagLegend)
    
    # Combine plot and legends
    p <- plot_grid(tagLegend, plot, ncol=1, rel_heights = c(1,30))
    return(p)
  })
}

#### Run Shiny app ####
shinyApp(ui, server)