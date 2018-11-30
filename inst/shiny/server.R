#' @title server for shiny app
#' @description server for soil moisture calibration shiny app
#' @param input input data set
#' @param output output list
#' @param session session info
#' @examples 
#' runApp(ui, server)
#' @rdname server
#' @export 
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_abline geom_point coord_cartesian geom_text geom_smooth
#' @importFrom leaflet renderLeaflet awesomeIcons leaflet addProviderTiles addAwesomeMarkers addMeasure addLayersControl layersControlOptions
#' @importFrom leaflet.extras addSearchOSM
#' 
#' 
# --------------------------
# load package dependencies and data
library(SWCalibrateR)
data("data")
# --------------------------
# shiny server
server <- function(input, output, session) {
  
# wraping reactive expression 
  datafile <- reactive({
    
    infile <- input$datafile
    
# Define data source and read     
    if (is.null(infile)) {
# User has not uploaded a file yet. 
# Use NULL to prevent observeEvent from triggering.
# Read example data   
    #data("SensorVSample")
    #temp <- data  
    temp <- read.csv(
      file.path(system.file("data", package = "SWCalibrateR"), "data.csv"), 
      sep=",", dec=".")
    return(temp)
    } else {
    temp <- read.csv(infile$datapath, sep=",", dec=".")
    return(temp)
    }
    
   # assign('data',temp,envir=.GlobalEnv)
 })
  
# reactive observer for input data set
  # Soil depth
  observe({
  updateSelectInput(session, "Soil.depth",
    choices = datafile()$Soil.depth %>% unique %>% as.numeric) 
  })
  # Project
  observe({
  updateSelectInput(session, "Project.ID", 
    choices = datafile()$Project.ID %>% levels) 
  })
  # Land use
  observe({
  updateSelectInput(session, "Landuse", 
    choices = datafile()$Landuse %>% levels) 
  })
  # Station
  observe({
  updateSelectInput(session, "Station.ID", 
    choices = datafile()$Station.ID %>% levels) 
  })
  # Date of observation
  observe({
  updateSelectInput(session, "Date", 
    choices = datafile()$Date %>% levels) 
  })
  # Sensor Type
  observe({
  updateSelectInput(session, "Sensor.type", 
    choices = datafile()$Sensor.type %>% levels) 
  })
  # Sensor Name
  observe({
  updateSelectInput(session, "Sensor.ID", 
    choices = datafile()$Sensor.ID %>% levels) 
  })
  # Soil Type
  observe({
  updateSelectInput(session, "Soil.type", 
    choices = datafile()$Soil.type %>% levels) 
  })
  
# For storing which rows have been excluded by "toggle points".
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(data))
  )
  
# Create new data object (reactive)
  data <- reactive ({
    # Project ID
    if ( length(input$Project.ID) == 0 ) {  
      sproject <- c(NA, datafile()$Project.ID %>% levels)
    } else {
      sproject <- input$Project.ID
    }
    # Landuse
    if ( length(input$Landuse) == 0 ) {
      slanduse <- c(NA, datafile()$Landuse %>% levels)
    } else {
      slanduse <- input$Landuse
    }
    # Station
    if ( length(input$Station.ID) == 0 ) {
      sstation <- c(NA, datafile()$Station.ID %>% levels)
    } else {
      sstation <- input$Station.ID
    }
    # Date
    if ( length(input$Date) == 0 ) {
      sdate <- c(NA, datafile()$Date %>% levels)
    } else {
      sdate <- input$Date
    }
    # Soil depth
    if ( length(input$Soil.depth) == 0 ) {
      sdepth  <- c(NA, datafile()$Soil.depth %>% unique %>% as.numeric)
    } else {
      sdepth  <- input$Soil.depth
    }
    # Sensor Type
    if ( length(input$Sensor.type) == 0 ){
      ssensorType   <- c(NA, datafile()$Sensor.type %>% levels)
    } else {
      ssensorType  <- input$Sensor.type
    }
    # Sensor Name
    if ( length(input$Sensor.ID) == 0 ){
      ssensorName  <- c(NA, datafile()$Sensor.ID %>% levels)
    } else {
      ssensorName  <- input$Sensor.ID
    }
    # Soil Type
    if ( length(input$Soil.type) == 0 ){
      ssoilType  <- c(NA, datafile()$Soil.type %>% levels)
    } else {
      ssoilType  <- input$Soil.type
    }
# filter data object, due to user choice and give back as data.frame    
    data <- datafile() %>% dplyr::filter(Project.ID %in% sproject,
      Station.ID %in% sstation, Landuse %in% slanduse,
      Date %in% sdate,  Sensor.type %in% ssensorType,
      Sensor.ID %in% ssensorName, Soil.type %in% ssoilType,
      Soil.depth %in% sdepth)
    return(as.data.frame(data))
  })
  
# Data table to output. 
  output$table <- renderDataTable({
    # define data
    data <- data()
    # # round to 2 digits
    data$Sensor.VWC <- round(data$Sensor.VWC, 2)
    data$Sample.VWC <- round(data$Sample.VWC, 2)
    data$Latitude   <- round(data$Latitude,   2)
    data$Longitude  <- round(data$Longitude,  2)
    data$Altitude   <- round(data$Altitude,   0)
    # rownames 
    data$row.ID     <- rownames(data)
    # delete NA 
    data[!is.na(data[,1]),]
    
    }, list(pageLength = 20, lengthMenu = c(20, 30, 50, 100)) )

# plot1 to output (data and model vis)    
  output$plot1 <- renderPlot({
    # define data
    data <- data()
    data$ID <- rownames(data)
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- data[ vals$keeprows, , drop = FALSE]
    exclude <- data[!vals$keeprows, , drop = FALSE]
    
    # zoom in if input$Zoom = TRUE
    if (input$Zoom) {
      # set limits for zoom
      xlim <- ylim <- c(0, .6); xypos <- .55
      } else {
      # set limits for no zoom
      xlim <- ylim <- c(0, .85); xypos <- .8
    }
    # ggplot Sensor.VWC vs. Sample.VWC
    p <- ggplot2::ggplot(keep, 
      ggplot2:: aes(x = Sensor.VWC, y = Sample.VWC, label = ID)) +
      # abline white x=y line
      ggplot2::geom_abline(intercept = 0, slope = 1, colour = "white") + 
      # write text x=y
      ggplot2::geom_text(x = xypos, y = xypos, label = "y = x", 
        color = "white") +
      ggplot2::geom_text(x = 0.05, y = 0.05, label = "y = x", 
        color = "white") +
      # plot points - scatter with exclude data set
      ggplot2::geom_point(data = exclude, fill = NA, color = "black", 
        alpha = 0.25) +
      # set limits of cartesian coordinate system, defined above
      ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
      ggplot2::xlab("Sensor VWC [%vol/%vol]") + 
      ggplot2::ylab("Sample VWC [%vol/%vol]")

    # MM-type regressor: SWCalibrateR::fitSMDM
    if (input$robust) {
      p <- p +  
        # add robust linear model to ggplot
        ggplot2::geom_smooth(method = fitSMDM, fullrange = TRUE,
          color = "grey") +
        # add model fun, estimated equation
        ggplot2::geom_text(x = 0.45, y = 0.05, 
          label = lm_eq(keep, method="rlm"), 
          parse = TRUE, size=6.5)
    # OLS method: stats::lm 
    } else {
      p <- p + 
        # add linear model (OLS) to ggplot
        ggplot2:: geom_smooth(method = lm, fullrange = TRUE, color = "grey") + 
        # add model fun, estimated equation
        ggplot2::geom_text(x = 0.45, y = 0.05, 
          label = lm_eq(keep, method="lm"), 
          parse = TRUE, size=6.5)
    }
    # show rownames
    if (input$Rownames) {
      # add rownames to ggplot
      p <- p + ggplot2::geom_text()
    } else {
      # add data points to ggplot
      p <- p + ggplot2::geom_point()
    }
    # return ggplot p
    p
  })
  
  #plot2 (model diagnostics)
  output$plot2 <- renderPlot({
    # define data
    data <- data()
    # Plot the kept and excluded points as two separate data sets
    keep    <- data[ vals$keeprows, , drop = FALSE]
    exclude <- data[!vals$keeprows, , drop = FALSE]
    # MM-type regressor: SWCalibrateR::fitSMDM
    if (input$robust) {
      # robust model estimation
      fit_rlm <- fitSMDM(formula = Sample.VWC ~ Sensor.VWC, data = keep)
      # caption for MM-type dignostic plots
      caps = c("Standardized residuals vs. Robust Distances", 
        "Normal Q-Q vs. Residuals", "Response vs. Fitted Values", 
        "Residuals vs Fitted", "Scale-Location")
      # define visualization paramter
      op <- par(mfrow=c(2,2))
      # Residuals vs Fitted
      plot(fit_rlm, 4, caption = caps)
      # Normal Q-Q vs. Residuals
      plot(fit_rlm, 2, caption = caps)
      # Scale-Location
      plot(fit_rlm, 5, caption = caps)
      # Standardized residuals vs. Robust Distances
      plot(fit_rlm, 1, caption = caps)
      # return to standard par
      par(op) 
    # OLS method: stats::lm 
    } else {
      # linear model estimatin (OLS)
      fit_rlm <- lm(formula = Sample.VWC ~ Sensor.VWC, data = keep)
      # define visualization paramter
      op <- par(mfrow=c(2,2))
      # plot model dignostics - OLS
      plot(fit_rlm, which=c(1,2,3,5), 
        caption = list("Residuals vs Fitted", "Normal Q-Q vs. Residuals",
        "Scale-Location", "Cook's distance", "Residuals vs Leverage",
        expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))
      # return to standard par
      par(op) 
    }
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    # define data
    data<-data()
    # find points that are near a click/hover/double-click
    res <- nearPoints(data, input$plot1_click, allRows = TRUE)
    # keep rows with points not clicked
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    # define data
    data <- data()
    # find rows of data that are selected by a brush
    res <- brushedPoints(data, input$plot1_brush, allRows = TRUE)
    # keep rows with points not selected by a brush
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    # define data
    data <- data()
    # reset selection to all points
    vals$keeprows <- rep(TRUE, nrow(data))
  })
  
    # add leaflet map to output
    output$map<- leaflet::renderLeaflet({
      # define data
      stations <- data()
      # if there are geographic coordinates defined in data selection
      if (any(names(stations) %in% c("Latitude", "Longitude"))) {
        c1 <- leaflet::awesomeIcons(icon = "ios-close", iconColor = "black", 
          library = "ion", markerColor = "blue")
        # plot leaflet map 
        m <- leaflet::leaflet() %>% 
          # add open street map search
          leaflet.extras::addSearchOSM() %>%
          # add open street map
          leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "OSM") %>% 
          # ad satellite image
          leaflet::addProviderTiles("Esri.WorldImagery", group = "SAT") %>%
          # mark stations by lat/lon 
          leaflet::addAwesomeMarkers(lng = stations$Longitude %>% as.character %>% as.numeric, 
            lat = stations$Latitude %>% as.character %>% as.numeric, icon = c1, 
            popup = paste("Name:", stations$Station.ID, "<br>","Landuse:", stations$Landuse,
              "<br>", "Altitude:", stations$Altitude)) %>%
          # add measure in meters
          leaflet::addMeasure(position = "topleft", primaryLengthUnit = "meters") %>%
          leaflet::addLayersControl(baseGroups = c("OSM","SAT"), 
            options = leaflet::layersControlOptions(collapsed = FALSE), 
            position = "topleft")
        # return map m
        m
    }
 })
}
