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
        #geom_smooth(method = lm, fullrange = TRUE, shape = 21, color = "grey") +
        geom_smooth(method = fitSMDM, fullrange = TRUE, shape = 21, color = "grey") +
        geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
        facet_grid(depth ~ landuse)
      
    } else {
      
      p <- ggplot(keep, aes(x = meanstation, y = meansample, label=ID)) +
        geom_abline(intercept = 0, slope = 1, colour = "white") + 
        geom_text(x = 0.55, y = 0.55, label = "y = x", color = "white") +
        geom_text(x = 0.05, y = 0.05, label = "y = x", color = "white") +
        #geom_smooth(method = lm, fullrange = TRUE, shape = 21, color = "grey") +
        geom_smooth(method = fitSMDM, fullrange = TRUE, shape = 21, color = "grey") +
        geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
        geom_text(x = 0.45, y = 0.05, label = lm_eqn(keep, method="rlm"), parse = TRUE) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
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
    
    fit_rlm <- fitSMDM(formula = meansample ~ meanstation, data = keep)
    
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