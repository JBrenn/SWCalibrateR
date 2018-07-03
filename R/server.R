if (!require("ggplot2")) install.packages("ggplot2")
if (!require("Cairo")) install.packages("Cairo")
if (!require("robustbase")) install.packages("robustbase")

library(ggplot2)
library(Cairo)
library(robustbase)
library(tidyverse)


source("CAL_doreg.R")
source("CAL_doreg_data.R")
source("CAL_updateRData.R")
source("CAL_updatedb.R")
source("dB_getSWC.R")
source("dB_readStationData.R")
source("fitSMDM.R")
source("lm_eq.R")
#load("SensorVSample.RData")
#data<-read.csv("SensorVSample_new.csv",sep=",",dec=".")

server <- function(input, output,session) {
  
  #filedata <- reactiveValues()
  
  datafile <- reactive({
    
    infile <- input$datafile
    
    if (is.null(infile)){
       #User has not uploaded a file yet. Use NULL to prevent observeEvent from triggering
    temp<-read.csv(file.path(getwd(),"SensorVSample_new.csv"),sep=",",dec=".")
    return(temp)
    
    }else{
    temp <- read.csv(infile,sep=",",dec=".")
    return(temp)
    }
    
   # assign('data',temp,envir=.GlobalEnv)
 })
  
  
  
  observe({ #data<-datafile
  updateSelectInput(session, "Project", choices = c("ALL",datafile()$project %>% levels) ) 
})
  
  observe({#data<-datafile
  updateSelectInput(session, "Landuse", choices = c("ALL",datafile()$landuse %>% levels) ) 
})
  
  observe({#data<-datafile
  updateSelectInput(session, "Station", choices = c("ALL",datafile()$station %>% levels) ) 
})
  
  observe({#data<-datafile
  updateSelectInput(session, "Date", choices = c("ALL",datafile()$date_obs %>% levels) ) 
})
  
  observe({#data<-datafile
  updateSelectInput(session, "SensorType", choices = c("ALL",datafile()$sensorType %>% levels) ) 
})
  
  observe({#data<-datafile
  updateSelectInput(session, "SensorName", choices = c("ALL",datafile()$sensorName %>% levels) ) 
})
  observe({#data<-datafile
  updateSelectInput(session, "SoilType", choices = c("ALL",datafile()$soilType %>% levels) ) 
})
  
  # For storing which rows have been excluded
  vals <- reactiveValues(#data<-datafile
    keeprows = rep(TRUE, nrow(datafile()))
  )
  
  output$table <- renderDataTable({
    data<-datafile
    if (input$Project=="ALL")  project <- NA else project <- input$Project
    if (input$Landuse=="ALL")  landuse <- NA else landuse <- input$Landuse
    if (input$Station=="ALL")  station <- NA else station <- input$Station
    if (input$Date=="ALL")  date <- NA else date <- input$Date
    if (input$Depth=="ALL")  depth <- NA else depth <- input$Depth
    if (input$SensorType=="ALL")  SensorType <- NA else SensorType <- input$SensorType
    if (input$SensorName=="ALL")  SensorName <- NA else SensorName <- input$SensorName
    if (input$SoilType=="ALL")  SoilType <- NA else SoilType <- input$SoilType
      
    data <- CAL_doreg_data(data = data, project = project, station = station, landuse = landuse, date_obs = date, 
                           depth = depth, sensorType = SensorType, sensorName = SensorName, soilType=SoilType, preserveStr = T)
    
    data$row.name <- rownames(data)
    
    data[!is.na(data[,1]),]
    
    }, list(pageLength = 20, lengthMenu = c(20, 30, 50, 100)) )
  
  output$plot1 <- renderPlot({
    data<-datafile
    if (input$Project=="ALL")  project <- NA else project <- input$Project
    if (input$Landuse=="ALL")  landuse <- NA else landuse <- input$Landuse
    if (input$Station=="ALL")  station <- NA else station <- input$Station
    if (input$Date=="ALL")  date <- NA else date <- input$Date
    if (input$Depth=="ALL")  depth <- NA else depth <- input$Depth
    if (input$SensorType=="ALL")  SensorType <- NA else SensorType <- input$SensorType
    if (input$SensorName=="ALL")  SensorName <- NA else SensorName <- input$SensorName
    if (input$SoilType=="ALL")  SoilType <- NA else SoilType <- input$SoilType
      
    data <- CAL_doreg_data(data = data, project = project, station = station, landuse = landuse, date_obs = date, 
                           depth = depth, sensorType = SensorType, sensorName = SensorName, soilType=SoilType, preserveStr = T)
  
    data$ID <- rownames(data)
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- data[ vals$keeprows, , drop = FALSE]
    exclude <- data[!vals$keeprows, , drop = FALSE]
    
    if (input$facet) {
      
      p <- ggplot(keep[!is.na(keep)[,1],], aes(x = meanstation, y = meansample, label=ID)) +
        geom_abline(intercept = 0, slope = 1, colour = "white") + 
        geom_point(data = exclude, fill = NA, color = "black", alpha = 0.25) +
        coord_cartesian(xlim = c(0, .60), ylim = c(0, .60)) + 
        facet_grid(depth ~ landuse)
      
    } else {
      
      if (input$Zoom) {xlim <- ylim <- c(0, .6); xypos <- .55} else {xlim <- ylim <- c(0, .85); xypos <- .8}
      
      p <- ggplot(keep, aes(x = meanstation, y = meansample, label=ID)) +
        geom_abline(intercept = 0, slope = 1, colour = "white") + 
        geom_text(x = xypos, y = xypos, label = "y = x", color = "white") +
        geom_text(x = 0.05, y = 0.05, label = "y = x", color = "white") +
        geom_point(data = exclude, fill = NA, color = "black", alpha = 0.25) +
        coord_cartesian(xlim = xlim, ylim = ylim)
    }
    
    if (input$facet)
    {
      if (input$robust) {
        p <- p +  geom_smooth(method = fitSMDM, fullrange = TRUE, color = "grey")
      } else {
        p <- p + geom_smooth(method = lm, fullrange = TRUE, color = "grey")
      }
    } else {
      if (input$robust) {
        p <- p +  geom_smooth(method = fitSMDM, fullrange = TRUE, color = "grey") +
          geom_text(x = 0.45, y = 0.05, label = lm_eqn(keep, method="rlm"), parse = TRUE, size=6.5)
      } else {
        p <- p + geom_smooth(method = lm, fullrange = TRUE, color = "grey") + 
          geom_text(x = 0.45, y = 0.05, label = lm_eqn(keep, method="lm"), parse = TRUE, size=6.5)
      }
    }

    if (input$Rownames) {
      p <- p + geom_text()
    } else {
      p <- p + geom_point()
    }
    
    p
    
  })
  
  output$plot2 <- renderPlot({
    data<-datafile
    if (input$Project=="ALL")  project <- NA else project <- input$Project
    if (input$Landuse=="ALL")  landuse <- NA else landuse <- input$Landuse
    if (input$Station=="ALL")  station <- NA else station <- input$Station
    if (input$Date=="ALL")  date <- NA else date <- input$Date
    if (input$Depth=="ALL")  depth <- NA else depth <- input$Depth
    if (input$SensorType=="ALL")  SensorType <- NA else SensorType <- input$SensorType
    if (input$SensorName=="ALL")  SensorName <- NA else SensorName <- input$SensorName
    if (input$SoilType=="ALL")  SoilType <- NA else SoilType <- input$SoilType
      
    data <- CAL_doreg_data(data = data, project = project, station = station, landuse = landuse, date_obs = date, 
                           depth = depth, sensorType = SensorType, sensorName = SensorName, soilType=SoilType, preserveStr = T)
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- data[ vals$keeprows, , drop = FALSE]
    exclude <- data[!vals$keeprows, , drop = FALSE]
    
    if (input$robust) {
      fit_rlm <- fitSMDM(formula = meansample ~ meanstation, data = keep)
    } else {
      fit_rlm <- lm(formula = meansample ~ meanstation, data = keep)
    }

    
    op <- par(mfrow=c(2,2))
      plot(fit_rlm, c(1,2,4,5))
    par(op)  
    
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(data, input$plot1_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(data, input$plot1_brush, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(data))
  })
  
}
