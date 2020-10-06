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
library(plotly)
library(lubridate)
library(RColorBrewer)
library(shinyWidgets)
library(cowplot)
library(grid)
library(scales)


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
sourceLineColor <- c("Observed"="#7B7B7B", "Apex projection"="#7d1428", "IHME projection"="#7cb961")

#### Data Loading ####

# Load observed data
data <- read_csv(file="data/data-obs.csv") %>%
  mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
  mutate(Metric = ordered(Metric, levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths"))) %>%
  mutate(DataTag = ordered(DataTag, level=c("IHME projection", "Apex projection", "Observed")))

# Keep a record of which modelled data files are available and loaded
#   \\d.*\\d is used to pull the date from file name
dataLoaded <- rep(FALSE, length(list.files("data", "data-20"))) %>%
  set_names(list.files("data", "data-20") %>% str_match("\\d.*\\d"))

# Load vector of dates with IHME model data
ihmeDates <- read_csv(file="data/ihme-dates.csv") %>% pull

# Define a function to load in model data as requested by the user
#   Note super assignment (<<-) used to update global variables
loadModeledData <- function(modelDate, addIHME = T){
  if(!(dataLoaded[modelDate])) {
    dataLoaded[modelDate] <<- TRUE
    data <<- read_csv(file= paste0("data/data-", modelDate, ".csv")) %>%
      mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
      mutate(Metric = ordered(Metric, levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths"))) %>%
      mutate(DataTag = ordered(DataTag, level=c("IHME projection", "Apex projection", "Observed"))) %>%
      bind_rows(data, .)

    # Also load the data file that provides the relevant IHME model
    # This can't be done if requesting a forecast prior to the first IHME model date
    if(addIHME && modelDate > ihmeDates[1]) {
      modelDate %>%
        as.Date %>%
        findInterval(ihmeDates) %>%    # returns index of the target date in ihmeDates
        ihmeDates[.] %>%               # subsets ihmeDates using piped index
        as.character %>%
        loadModeledData(F)             # F is used to terminate recursion
     }
  }
}

# Load most recent models
loadModeledData(dataLoaded %>% tail(1) %>% names)

#### Helpers ####
forecastDates <- dataLoaded %>% names %>% as.Date

defaultForecastDate <- tail(forecastDates, 1)

jurisdictions <- sort(unique(data$Jurisdiction))

oldestIHMEdate_Deaths <- as.Date('2020-04-22')

oldestIHMEdate_Infections <- as.Date('2020-05-10')

minDate <- min(data$Date[which(data$Source == "Apex")])
maxDate <- max(data$Date[which(data$Source == "Apex")])

whiteTheme <- theme(panel.background = element_rect(fill = "#f9f9f9"),
                    panel.border = element_rect(fill = NA, color = "grey75"),
                    axis.ticks = element_line(color = "grey55"),
                    panel.grid.major = element_line(color = "grey85", size = 0.2),
                    panel.grid.minor = element_line(color = "grey85", size = 0.2),
                    plot.title = element_text(hjust=0.5, size=12),
                    axis.title = element_text(size=12),
                    strip.text = element_text(size=12, color="white", margin=margin(t=10, b=10)),
                    strip.background = element_rect(fill="#7d1428"),
                    axis.text = element_text(size=10),
                    legend.key = element_rect(fill = NA),
                    legend.text = element_text(size=14, margin = margin(r = 30, unit = "pt")),
                    legend.title = element_blank())

mycss <- "
.mycheckbox .shiny-input-container {
  display: inline-block;
  width: auto;
  height: 0px;
  padding: 0px;
  margin-top: -2em;
}"

# myTrans <- function(x){
#   y <- log10(x+1)
#   return(y)
# }
# 
# inv.myTrans <- function(x){
#   y <- 10^(x)-1
#   return(y)
# }

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
                
                #titlePanel(column(width = 12,
                #                  a(img(src = "SyncroSim-Logo.png"), href="https://syncrosim.com"))),
                
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
                                           
                                           tags$style(mycss),
                                           
                                           p(strong("Select Models")),
                                           
                                           span(class="mycheckbox", checkboxInput("Apex", "Apex  - ", value=T)),
                                           
                                           actionButton(inputId='ApexLink',
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
                                           
                                           span(class="mycheckbox", checkboxInput("IHME", "IHME - ", value=F)),
                                           
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
                                                                        border-width: 0px"),
                                           
                                           bsTooltip("models", "Select one or more models to plot", placement="right"),
                                           
                                           bsTooltip("ApexLink", "Apex model details", placement="right"),
                                           
                                           bsTooltip("IHMELink", "IHME model details", placement="right"),
                                           
                                           br(),br(),
                                           
                                           # materialSwitch("logY",
                                           #                label = "Log Y axis",
                                           #                value = F,
                                           #                status = "primary",
                                           #                width="100%"),
                                           # 
                                           # bsTooltip("logY", "Plot data on a log scale", placement="right"),
                                           # 
                                           dateRangeInput("range", width="100%", label = "Date Range",
                                                          start = "2020-03-15", end = maxDate, 
                                                          min = minDate, max = maxDate,
                                                          format = "M d"),
                                           
                                           bsTooltip("range", "Select a range of dates to plot forecasts", placement="right"),
                                           
                                           hr(),
                                           
                                           downloadButton("downloadData", 
                                                          "Download Data", 
                                                          style = "color: white;
                                                          background-color: SteelBlue"),
                                           br(),br(),
                                           
                                           p("Powered by ",
                                             a("SyncroSim", 
                                               href = "https://syncrosim.com/"))),
                              
                              mainPanel(width=9,
                                        
                                        titlePanel(h2("COVID-19 Forecasts For Canada", align="center")),
                                        
                                        tabsetPanel(type = c("pills"),
                                                    tabPanel("Deaths",
                                                             fluidRow(column(12, align="center",
                                                                             plotOutput("deathLegend", width="100%", height="40px"),
                                                                             plotlyOutput("deathChart", width="100%", height="650px")))),
                                                    tabPanel("Infections",
                                                             fluidRow(column(12, align="center",
                                                                             plotOutput("infectionLegend", width="100%", height="40px"),
                                                                             plotlyOutput("infectionChart", width="100%", height="650px"))))))))

#### Server ####
server <- function(input, output) {
  
  output$deathLegend <- renderPlot({
    
    # Get selected models
    models <- c("Apex", "IHME")
    models <- models[c(input$Apex, input$IHME)]
    
    # Get most recent IHME model date
    lastIHMEdate <- data %>%
      filter(Source == "IHME") %>%
      filter(date_model_run <= input$forecastDate) %>%
      pull(date_model_run) %>%
      max()
    
    # Subset data based on user inputs
    dataSubset <- data %>% filter(Metric %in% c("Daily Deaths", "Cumulative Deaths")) %>% # Deaths only
      filter(Jurisdiction %in% input$juris) %>% # Only keep jurisdiction of interest
      filter(!((DataTag == "Apex projection") & (!date_model_run == input$forecastDate))) %>% # Remove Apex predictions for all but the model run of interest
      filter(!((DataTag == "IHME projection") & (!date_model_run == lastIHMEdate))) %>% # Remove IHME predictions for all but the model run of interest
      filter(!((DataType == "Modeled") & (!Source %in% models))) %>% # Only keep models of interest
      filter(Date >= input$range[1] & Date <= input$range[2]) %>% # Only keep dates of interest
      arrange(Metric, Jurisdiction, Date)
    
    # Validate
    validate(need(!(((nrow(dataSubset) == 0) || (length(unique(dataSubset$Date)) == 1))), paste0('No data available for the selected date range. \n Please widen the range of dates considered.')))
    
    # Produce legend for data tags
    tagLegend <- ggplot(dataSubset, aes(x=Date, y=Mean, color=DataTag)) +
      geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=DataTag), alpha=0.4, color=NA) +
      geom_line(size=1) +
      scale_color_manual(values=sourceLineColor, labels=c("Observed"="Observed", "Apex projection"='Apex', "IHME projection"='IHME'), breaks = c("Observed", "Apex projection", "IHME projection")) +
      scale_fill_manual(values=c("Observed"="#ffffff", "Apex projection"="#7d1428", "IHME projection"="#7cb961"), labels=c("Observed"="Observed", "Apex projection"='Apex', "IHME projection"='IHME'), breaks = c("Observed", "Apex projection", "IHME projection")) +
      whiteTheme +
      theme(legend.position = "top",
            legend.justification = "center",
            legend.margin=margin(0,0,0,0),
            legend.key.width = unit(2,"cm"))
    
    tagLegend <- get_legend(tagLegend)
    
    # Print legend
    grid.draw(tagLegend)
  })
  
  output$deathChart <- renderPlotly({
    # Check requested data is loaded
    loadModeledData(input$forecastDate %>% as.character)

    text <- F
    
    # Get selected models
    models <- c("Apex", "IHME")
    models <- models[c(input$Apex, input$IHME)]
    
    # Get most recent IHME model date
    lastIHMEdate <- data %>%
      filter(Source == "IHME") %>%
      filter(date_model_run <= input$forecastDate) %>%
      pull(date_model_run) %>%
      max()
    
    # Subset data based on user inputs
    dataSubset <- data %>% filter(Metric %in% c("Daily Deaths", "Cumulative Deaths")) %>% # Deaths only
      filter(Jurisdiction %in% input$juris) # Only keep jurisdiction of interest
    
    oldestApexdate_juris <- dataSubset %>%
      filter((DataType == "Modeled") & (Source == "Apex")) %>%
      pull(date_model_run) %>%
      min()
    
    dataSubset %<>% filter(!((DataTag == "Apex projection") & (!date_model_run == input$forecastDate))) %>% # Remove Apex predictions for all but the model run of interest
      filter(!((DataTag == "IHME projection") & (!date_model_run == lastIHMEdate))) %>% # Remove IHME predictions for all but the model run of interest
      filter(!((DataType == "Modeled") & (!Source %in% models))) %>% # Only keep models of interest
      filter(Date >= input$range[1] & Date <= input$range[2]) %>% # Only keep dates of interest
      arrange(Metric, Jurisdiction, Date)
    
    dataSubset <- dataSubset[,colSums(is.na(dataSubset))<nrow(dataSubset)]
    
    # Validate
    validate(need(!(((nrow(dataSubset) == 0) || (length(unique(dataSubset$Date)) == 1))), paste0('')))
    
    # Produce main plot (without legend)
    plot <- ggplot(dataSubset, aes(x=Date, y=Mean)) + 
      geom_line(aes(color=DataTag)) +
      scale_color_manual(values=sourceLineColor) +
      scale_fill_manual(values=sourceLineColor) +
      scale_y_continuous(name="") +
      guides(color=F, linetype=F) +
      whiteTheme +
      facet_wrap(vars(Metric), scales="free_y", ncol=1) +
      theme(axis.title.x = element_blank(),
            plot.margin=unit(c(5,5,10,5),"pt"),
            strip.text = element_text(size=14, vjust = 0.5, margin = margin(0.1,0,0.1,0,"cm")),
            legend.position='none',
            panel.spacing = unit(0, "lines"))
    
    # Add geom_ribbon if there is modeled data
    if("Modeled" %in% dataSubset$DataType){
      plot <- plot +
        geom_ribbon(data=dataSubset[which(dataSubset$DataType=="Modeled"),], aes(ymin=Lower, ymax=Upper, fill=DataTag), alpha=0.4, color=NA, show.legend=F)
    }
    
    # Add error message if data is missing
    if(length(models)>0){
      
      if(("Apex" %in% models) && ("IHME" %in% models)){
        # If there is no earlier Apex model for the jurisdiction
        if(input$forecastDate < oldestApexdate_juris){
          # If there is no IHME model for the jurisdiction
          if(!input$juris %in% data$Jurisdiction[which(data$Source == 'IHME')]){
            notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
            notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
            plot <- plot +
              geom_text(data = notes,
                        aes(x=x, y=y),
                        color="#7B7B7B",
                        hjust=0,
                        vjust=1,
                        size=0.01,
                        label = paste0("No Apex projection available prior to\n", oldestApexdate_juris, " for the selected jurisdiction.\nThe IHME does not model this jurisdiction."))
            text <- T
            
            # If there is no earlier IHME model for the jurisdiction
          }else if(input$forecastDate < oldestIHMEdate_Deaths){
            notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
            notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
            plot <- plot +
              geom_text(data = notes,
                        aes(x=x, y=y),
                        color="#7B7B7B",
                        hjust=0,
                        vjust=1,
                        size=0.01,
                        label = paste0("No Apex projection available prior to ", oldestApexdate_juris, ".\nNo IHME projection available prior to ", oldestIHMEdate_Deaths, "."))
            text <- T
            
            # If there is an earlier IHME model for the jurisdiction
          }else if(!lastIHMEdate == input$forecastDate){
            notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
            notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
            plot <- plot +
              geom_text(data = notes,
                        aes(x=x, y=y),
                        color="#7B7B7B",
                        hjust=0,
                        vjust=1,
                        size=0.01,
                        label = paste("No Apex projection available prior to ", oldestApexdate_juris, ".\nNo IHME projection available \nfor the selected forecast date. \nShowing", lastIHMEdate, "instead."))
            text <- T
          }
        }else{
          # If there is no IHME model for the jurisdiction
          if(!input$juris %in% data$Jurisdiction[which(data$Source == 'IHME')]){
            notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
            notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
            plot <- plot +
              geom_text(data = notes,
                        aes(x=x, y=y),
                        color="#7B7B7B",
                        hjust=0,
                        vjust=1,
                        size=0.01,
                        label = paste0("No IHME projection available \nfor the selected jurisdiction."))
            text <- T
            
            # If there is no earlier IHME model for the jurisdiction
          }else if(input$forecastDate < oldestIHMEdate_Deaths){
            notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
            notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
            plot <- plot +
              geom_text(data = notes,
                        aes(x=x, y=y),
                        color="#7B7B7B",
                        hjust=0,
                        vjust=1,
                        size=0.01,
                        label = paste0("No IHME projection available prior to ", oldestIHMEdate_Deaths, "."))
            text <- T
            
            # If there is an earlier IHME model for the jurisdiction
          }else if(!lastIHMEdate == input$forecastDate){
            notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
            notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
            plot <- plot +
              geom_text(data = notes,
                        aes(x=x, y=y),
                        color="#7B7B7B",
                        hjust=0,
                        vjust=1,
                        size=0.01,
                        label = paste("No IHME projection available \nfor the selected forecast date. \nShowing", lastIHMEdate, "instead."))
            text <- T
          }
        }
        
      }else if("Apex" %in% models){
        # If there is no earlier Apex model for the jurisdiction
        if(input$forecastDate < oldestApexdate_juris){
          notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
          notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
          plot <- plot +
            geom_text(data = notes,
                      aes(x=x, y=y),
                      color="#7B7B7B",
                      hjust=0,
                      vjust=1,
                      size=0.01,
                      label = paste0("No Apex projection available prior to\n", oldestApexdate_juris, " for the selected jurisdiction."))
          text <- T
        }
        
      }else if("IHME" %in% models){
        # If there is no IHME model for the jurisdiction
        if(!input$juris %in% data$Jurisdiction[which(data$Source == 'IHME')]){
          notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
          notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
          plot <- plot +
            geom_text(data = notes,
                      aes(x=x, y=y),
                      color="#7B7B7B",
                      hjust=0,
                      vjust=1,
                      size=0.01,
                      label = paste0("No IHME projection available \nfor the selected jurisdiction."))
          text <- T
          
          # If there is no earlier IHME model for the jurisdiction
        }else if(input$forecastDate < oldestIHMEdate_Deaths){
          notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
          notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
          plot <- plot +
            geom_text(data = notes,
                      aes(x=x, y=y),
                      color="#7B7B7B",
                      hjust=0,
                      vjust=1,
                      size=0.01,
                      label = paste0("No IHME projection available prior to ", oldestIHMEdate_Deaths, "."))
          text <- T
          
          # If there is an earlier IHME model for the jurisdiction
        }else if(!lastIHMEdate == input$forecastDate){
          notes <- data.frame(Metric = ordered(c("Daily Deaths", "Cumulative Deaths"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
          notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
          plot <- plot +
            geom_text(data = notes,
                      aes(x=x, y=y),
                      color="#7B7B7B",
                      hjust=0,
                      vjust=1,
                      size=0.01,
                      label = paste("No IHME projection available \nfor the selected forecast date. \nShowing", lastIHMEdate, "instead."))
          text <- T
        }
      }
    }
    
    # Plotlyfy
    plot <- ggplotly(plot, tooltip=c("Date", "Mean", "Upper", "Lower")) %>%
      style(textposition = "bottom right") %>%
      config(displayModeBar = F)
    
    # Disable hovering on geom_text
    if(text){
      
      # If there is only observed data
      if(!"Modeled" %in% dataSubset$DataType){
        plot %<>% style(hoverinfo = "none", traces = c(3,4))
        
      # If there is only modeled data
      }else if(!"Observed" %in% dataSubset$DataType){
        
        # If there is one type of modeled data
        if(length(unique(dataSubset$DataTag[which(!dataSubset$DataType == 'Observed')])) == 1){
          plot %<>% style(hoverinfo = "none", traces = c(5,6))
          
        # If there are two types of modeled data
        }else{
          plot %<>% style(hoverinfo = "none", traces = c(9,10))
          
        }
      # If there is observed and modeled data
      }else{
        
        # If there is one type of modeled data
        if(length(unique(dataSubset$DataTag[which(!dataSubset$DataType == 'Observed')])) == 1){
          plot %<>% style(hoverinfo = "none", traces = c(7,8))
          
          # If there are two types of modeled data
        }else{
          plot %<>% style(hoverinfo = "none", traces = c(11,12))
        }
      }
    }
    
    # Print plot
    plot
  })
  
  output$infectionLegend <- renderPlot({
    
    # Get selected models
    models <- c("Apex", "IHME")
    models <- models[c(input$Apex, input$IHME)]
    
    # Get most recent IHME model date
    lastIHMEdate <- data %>%
      filter(Source == "IHME") %>%
      filter(date_model_run <= input$forecastDate) %>%
      pull(date_model_run) %>%
      max()
    
    # Subset data based on user inputs
    dataSubset <- data %>% filter(Metric %in% c("Daily Infections", "Cumulative Infections")) %>% # Deaths only
      filter(Jurisdiction %in% input$juris) # Only keep jurisdiction of interest
    
    oldestApexdate_juris <- dataSubset %>%
      filter((DataType == "Modeled") & (Source == "Apex")) %>%
      pull(date_model_run) %>%
      min()
    
    dataSubset %<>% filter(!((DataTag == "Apex projection") & (!date_model_run == input$forecastDate))) %>% # Remove Apex predictions for all but the model run of interest
      filter(!((DataTag == "IHME projection") & (!date_model_run == lastIHMEdate))) %>% # Remove IHME predictions for all but the model run of interest
      filter(!((DataType == "Modeled") & (!Source %in% models))) # Only keep models of interest
    
    dataSubset_allDates <- dataSubset
    
    dataSubset %<>% filter(Date >= input$range[1] & Date <= input$range[2]) %>% # Only keep dates of interest
      arrange(Metric, Jurisdiction, Date)
    
    dataSubset <- dataSubset[,colSums(is.na(dataSubset))<nrow(dataSubset)]
    
    # Validate
    validate(need(models, 'Select at least one model.'))
    validate(need(!((nrow(dataSubset_allDates) == 0) && (input$juris %in% data$Jurisdiction[which(data$Source == 'IHME')])), paste0(' \nNo projection available for the selected forecast date, jurisdiction, and model combination. Please select different input parameters.\n \nApex projections for the selected jurisdiction are available starting ', oldestApexdate_juris, '.\n IHME infection projections for the selected jurisdiction are available starting ', oldestIHMEdate_Infections, '.')))
    validate(need(!(nrow(dataSubset_allDates) == 0), paste0(' \nNo projection available for the selected forecast date, jurisdiction, and model combination. Please select different input parameters.\n \nApex projections for the selected jurisdiction are available starting ', oldestApexdate_juris, '.\n The IHME does not model the selected jurisdiction.')))
    validate(need(!(((nrow(dataSubset) == 0) || (length(unique(dataSubset$Date)) == 1))), paste0('No data available for the selected date range. \n Please widen the range of dates considered.')))
    validate(need(!(models=="IHME" && !input$juris %in% data$Jurisdiction[which(data$Source == "IHME")]), 'No IHME projection available for the selected jurisdiction.\nPlease select a different jurisdiction and/or a different model.'))
    validate(need(!(models=="IHME" && input$forecastDate < oldestIHMEdate_Infections), paste('No IHME projection available prior to', oldestIHMEdate_Infections, 'for infections.\nPlease select a different forecast date and/or a different model.')))
    
    # Produce legend for data tags
    tagLegend <- ggplot(dataSubset, aes(x=Date, y=Mean, color=DataTag)) +
      geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=DataTag), alpha=0.4, color=NA) +
      geom_line(size=1) +
      scale_color_manual(values=sourceLineColor, labels=c("Observed"="Observed", "Apex projection"='Apex', "IHME projection"='IHME'), breaks = c("Observed", "Apex projection", "IHME projection")) +
      scale_fill_manual(values=c("Observed"="#ffffff", "Apex projection"="#7d1428", "IHME projection"="#7cb961"), labels=c("Observed"="Observed", "Apex projection"='Apex', "IHME projection"='IHME'), breaks = c("Observed", "Apex projection", "IHME projection")) +
      whiteTheme +
      theme(legend.position = "top",
            legend.justification = "center",
            legend.margin=margin(0,0,0,0),
            legend.key.width = unit(2,"cm"))
    
    tagLegend <- get_legend(tagLegend)
    
    # Print legend
    grid.draw(tagLegend)
  })
  
  output$infectionChart <- renderPlotly({
    text <- F
    
    # Get selected models
    models <- c("Apex", "IHME")
    models <- models[c(input$Apex, input$IHME)]
    
    # Get most recent IHME model date
    lastIHMEdate <- data %>%
      filter(Source == "IHME") %>%
      filter(date_model_run <= input$forecastDate) %>%
      pull(date_model_run) %>%
      max()
    
    # Subset data based on user inputs
    dataSubset <- data %>% filter(Metric %in% c("Daily Infections", "Cumulative Infections")) %>% # Deaths only
      filter(Jurisdiction %in% input$juris) # Only keep jurisdiction of interest
    
    oldestApexdate_juris <- dataSubset %>%
      filter((DataType == "Modeled") & (Source == "Apex")) %>%
      pull(date_model_run) %>%
      min()
    
    dataSubset %<>% filter(!((DataTag == "Apex projection") & (!date_model_run == input$forecastDate))) %>% # Remove Apex predictions for all but the model run of interest
      filter(!((DataTag == "IHME projection") & (!date_model_run == lastIHMEdate))) %>% # Remove IHME predictions for all but the model run of interest
      filter(!((DataType == "Modeled") & (!Source %in% models))) # Only keep models of interest
    
    dataSubset_allDates <- dataSubset
    
    dataSubset %<>% filter(Date >= input$range[1] & Date <= input$range[2]) %>% # Only keep dates of interest
      arrange(Metric, Jurisdiction, Date)
    
    dataSubset <- dataSubset[,colSums(is.na(dataSubset))<nrow(dataSubset)]
    
    # Validate
    validate(need(models, ''))
    validate(need(!((nrow(dataSubset_allDates) == 0) && (input$juris %in% data$Jurisdiction[which(data$Source == 'IHME')])), ''))
    validate(need(!(nrow(dataSubset_allDates) == 0), ''))
    validate(need(!(((nrow(dataSubset) == 0) || (length(unique(dataSubset$Date)) == 1))), ''))
    validate(need(!(models=="IHME" && !input$juris %in% data$Jurisdiction[which(data$Source == "IHME")]), ''))
    validate(need(!(models=="IHME" && input$forecastDate < oldestIHMEdate_Infections), ''))
    
    # Produce main plot (without legend)
    plot <- ggplot(dataSubset, aes(x=Date, y=Mean)) +
      geom_line(aes(color=DataTag)) +
      scale_color_manual(values=sourceLineColor) +
      scale_fill_manual(values=sourceLineColor) +
      scale_y_continuous(name="") +
      guides(color=F, linetype=F) +
      whiteTheme +
      facet_wrap(vars(Metric), scales="free_y", ncol=1) +
      theme(axis.title.x = element_blank(),
            plot.margin=unit(c(5,5,10,5),"pt"),
            strip.text = element_text(size=14, vjust = 0.5, margin = margin(0.1,0,0.1,0,"cm")),
            legend.position='none',
            panel.spacing = unit(0, "lines"))
    
    # Add geom_ribbon if there is modeled data
    if("Modeled" %in% dataSubset$DataType){
      plot <- plot +
        geom_ribbon(data=dataSubset[which(dataSubset$DataType=="Modeled"),], aes(ymin=Lower, ymax=Upper, fill=DataTag), alpha=0.4, color=NA, show.legend=F)
    }
    
    # Add error message if data is missing
    if("IHME" %in% models){
      
      # If there is no IHME model for the jurisdiction
      if(!input$juris %in% data$Jurisdiction[which(data$Source == 'IHME')]){
        notes <- data.frame(Metric = ordered(c("Daily Infections", "Cumulative Infections"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
        notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
        plot <- plot +
          geom_text(data = notes,
                    aes(x=x, y=y),
                    color="#7B7B7B",
                    hjust=0,
                    vjust=1,
                    size=0.01,
                    label = paste0("No IHME projection available \nfor the selected jurisdiction."))
        
        text <- T
        
        # If there is no earlier IHME model for the jurisdiction
      }else if(input$forecastDate < oldestIHMEdate_Infections){
        notes <- data.frame(Metric = ordered(c("Daily Infections", "Cumulative Infections"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
        notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
        plot <- plot +
          geom_text(data = notes,
                    aes(x=x, y=y),
                    color="#7B7B7B",
                    hjust=0,
                    vjust=1,
                    size=0.01,
                    label = paste0("No IHME projection available prior to ", oldestIHMEdate_Infections, "."))
        text <- T
        
        # If there is an earlier IHME model for the jurisdiction        
      }else if(!lastIHMEdate == input$forecastDate){
        notes <- data.frame(Metric = ordered(c("Daily Infections", "Cumulative Infections"), levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")), x = min(dataSubset$Date))
        notes$y <- sapply(notes$Metric, function(x) max(max(dataSubset$Upper[which(dataSubset$Metric == x)], na.rm=T), max(dataSubset$Mean[which(dataSubset$Metric == x)], na.rm=T)))
        plot <- plot +
          geom_text(data = notes,
                    aes(x=x, y=y),
                    color="#7B7B7B",
                    hjust=0,
                    vjust=1,
                    size=0.01,
                    label = paste("No IHME projection available \nfor the selected forecast date. \nShowing", lastIHMEdate, "instead."))
        text <- T
      }
    }
    
    # Plotlyfy
    plot <- ggplotly(plot, tooltip=c("Date", "Mean", "Upper", "Lower")) %>%
      style(textposition = "bottom right") %>%
      config(displayModeBar = F)
    
    # Disable hovering on geom_text
    if(text){
      
      # If there is one type of modeled data
      if(length(unique(dataSubset$DataTag[which(!dataSubset$DataType == 'Observed')])) == 1){
        plot %<>% style(hoverinfo = "none", traces = c(5,6))
        
      # If there are two types of modeled data
      }else{
        plot %<>% style(hoverinfo = "none", traces = c(9,10))
      }
    }
    
    # Print plot
    plot
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("covid-19-forecast-data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
}

#### Run Shiny app ####
shinyApp(ui, server)
