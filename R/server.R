server <- function(input, output) {
  
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(data))
  )
  
  output$table <- renderDataTable({
    
    if (input$Project=="ALL")  project <- NA else project <- input$Project
    if (input$Landuse=="ALL")  landuse <- NA else landuse <- input$Landuse
    if (input$Station=="ALL")  station <- NA else station <- input$Station
    if (input$Date=="ALL")  date <- NA else date <- input$Date
    if (input$Depth=="ALL")  depth <- NA else depth <- input$Depth
    if (input$SensorType=="ALL")  SensorType <- NA else SensorType <- input$SensorType
    if (input$SensorName=="ALL")  SensorName <- NA else SensorName <- input$SensorName
    
    data <- CAL_doreg_data(data = data, project = project, station = station, landuse = landuse, date_obs = date, 
                           depth = depth, sensorType = SensorType, sensorName = SensorName, preserveStr = T)
    
    data$row.name <- rownames(data)
    
    data[!is.na(data[,1]),]
    
    }, list(pageLength = 20, lengthMenu = c(20, 30, 50, 100)) )
  
  output$plot1 <- renderPlot({
    
    if (input$Project=="ALL")  project <- NA else project <- input$Project
    if (input$Landuse=="ALL")  landuse <- NA else landuse <- input$Landuse
    if (input$Station=="ALL")  station <- NA else station <- input$Station
    if (input$Date=="ALL")  date <- NA else date <- input$Date
    if (input$Depth=="ALL")  depth <- NA else depth <- input$Depth
    if (input$SensorType=="ALL")  SensorType <- NA else SensorType <- input$SensorType
    if (input$SensorName=="ALL")  SensorName <- NA else SensorName <- input$SensorName
    
    data <- CAL_doreg_data(data = data, project = project, station = station, landuse = landuse, date_obs = date, 
                           depth = depth, sensorType = SensorType, sensorName = SensorName, preserveStr = T)
  
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
    
    if (input$Project=="ALL")  project <- NA else project <- input$Project
    if (input$Landuse=="ALL")  landuse <- NA else landuse <- input$Landuse
    if (input$Station=="ALL")  station <- NA else station <- input$Station
    if (input$Date=="ALL")  date <- NA else date <- input$Date
    if (input$Depth=="ALL")  depth <- NA else depth <- input$Depth
    if (input$SensorType=="ALL")  SensorType <- NA else SensorType <- input$SensorType
    if (input$SensorName=="ALL")  SensorName <- NA else SensorName <- input$SensorName
    
    data <- CAL_doreg_data(data = data, project = project, station = station, landuse = landuse, date_obs = date, 
                           depth = depth, sensorType = SensorType, sensorName = SensorName, preserveStr = T)
    
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
  
#   output$plot3 <- renderDygraph({
#     zoo_data <- datasetInput()
#     zoo::index(zoo_data) <- as.POSIXct(zoo::index(zoo_data))
#     
#     dygraph(zoo_data) %>%
#       dyRoller() %>%
#       dyRangeSelector()
#     
#   })
  
  output$plot3 <- renderPlot({
    zoo_data <- datasetInput()
    #zoo::index(zoo_data) <- as.POSIXct(zoo::index(zoo_data))
    
    nrpairs <- length(names(zoo_data)) /2
    
    #zoo_data <- na.omit(zoo_data)
    #zoo_data_roll <- rollmean(zoo_data,4*24)
    
    color <- c(rep(rgb(1,0,0,.3), times=nrpairs), rep(rgb(0,0,1,.3), times=nrpairs))
    
    zoo::plot.zoo(zoo_data, plot.type="single", col=color, lwd=2, bty="n", ylim=c(0,.6), 
         ylab="Soil Moisture Content", xlab="")
    legend("bottomright", legend = c("uncalibrated series", "calibrated series"), col=c(rgb(1,0,0,.5), rgb(0,0,1,.5)), lwd=2, bty = "n")
    
  })
  
  datasetInput <- reactive({
    if (input$Project=="ALL")  project <- NA else project <- input$Project
    if (input$Landuse=="ALL")  landuse <- NA else landuse <- input$Landuse
    if (input$Station=="ALL")  station <- NA else station <- input$Station
    if (input$Date=="ALL")  date <- NA else date <- input$Date
    if (input$Depth=="ALL")  depth <- NA else depth <- input$Depth
    if (input$SensorType=="ALL")  SensorType <- NA else SensorType <- input$SensorType
    if (input$SensorName=="ALL")  SensorName <- NA else SensorName <- input$SensorName
    
    data <- CAL_doreg_data(data = data, project = project, station = station, landuse = landuse, date_obs = date, 
                           depth = depth, sensorType = SensorType, sensorName = SensorName, preserveStr = T)
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- data[ vals$keeprows, , drop = FALSE]
    exclude <- data[!vals$keeprows, , drop = FALSE]
    
    if (input$robust) {
      fit_rlm <- fitSMDM(formula = meansample ~ meanstation, data = keep)
    } else {
      fit_rlm <- lm(formula = meansample ~ meanstation, data = keep)
    }
    
    # test sql lite db
    # connect to db in data folder of project
    pkg_path <- path.package("SMCcalibration")
    #setwd(file.path(pkg_path,"data"))
    db = dbConnect(SQLite(), dbname=file.path(pkg_path,"data","swc.sqlite"))
    
    
    # get station data
    swc_st_    <- dbReadTable(conn = db, name = input$StationTs)
    swc_st     <- swc_st_[,-1]
    # close conn
    dbDisconnect(db)
    
    # only depth set
    if (input$DepthTs!="ALL" & input$SensorNameTs=="ALL")
      swc_data <- swc_st[,grepl(input$DepthTs,names(swc_st))]
    
    # only sensor set
    if (input$DepthTs=="ALL" & input$SensorNameTs!="ALL") 
      swc_data <- swc_st[,grepl(input$SensorNameTs,names(swc_st))]
    
    # depth & sensor set
    if (input$DepthTs!="ALL" & input$SensorNameTs!="ALL")
      swc_data <- swc_st[,grepl(paste(input$SensorNameTs,input$DepthTs,sep="_"),names(swc_st))]
   
    # depth & sensor set
    if (input$DepthTs=="ALL" & input$SensorNameTs=="ALL") swc_data <- swc_st
  
    swc_cal  <-  coef(fit_rlm)[1] + coef(fit_rlm)[2] *swc_data   
    
    swc_cal <- round(swc_cal, 3)
    
    swc_zoo <- zoo::zoo(cbind(swc_data,swc_cal), chron::chron(swc_st_$datetime))
    if (dim(swc_zoo)[2] > 2) {
      names(swc_zoo) <- paste(names(swc_zoo), c(rep("uncal",length(names(swc_zoo))/2), rep("cal",length(names(swc_zoo))/2)), sep="_") 
    } else {
      names(swc_zoo) <- paste("SWC_",input$SensorNameTs,"_",input$DepthTs,c("_uncal","_cal"),sep="")
    }

    swc_zoo
  })
  
  output$downloadData <- downloadHandler(filename = function() { 
    paste(input$StationTs,input$SensorNameTs,input$DepthTs, ".csv", sep="") 
    },
    content = function(file) {
      data <- datasetInput()
      date <- as.Date(zoo::index(data))
      time <- substr(zoo::index(data),11,18)
      data <- data.frame(date=date, time=time, coredata(data))
      write.csv(data, file, sep=",", quote=F, row.names = F) 
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