#' @title server for shiny app
#' @description server for soil moisture calibration shiny app
#' @param input input data set
#' @param output output list
#' @param session session info
#' @return
#' @details
#' @examples 
#' runApp(ui, server)
#' @seealso 
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{geom_point}},\code{\link[ggplot2]{coord_cartesian}},\code{\link[ggplot2]{facet_grid}},\code{\link[ggplot2]{geom_label}},\code{\link[ggplot2]{geom_smooth}}
#'  \code{\link[leaflet]{leafletOutput}},\code{\link[leaflet]{awesomeIcons}},\code{\link[leaflet]{leaflet}},\code{\link[leaflet]{addProviderTiles}},\code{\link[leaflet]{addAwesomeMarkers}},\code{\link[leaflet]{addMeasure}},\code{\link[leaflet]{addLayersControl}}
#'  \code{\link[leaflet.extras]{addSearchOSM}}
#' @rdname server
#' @export 
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_abline geom_point coord_cartesian facet_grid geom_text geom_smooth
#' @importFrom leaflet renderLeaflet awesomeIcons leaflet addProviderTiles addAwesomeMarkers addMeasure addLayersControl layersControlOptions
#' @importFrom leaflet.extras addSearchOSM

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
    temp <- read.csv(file.path(getwd(), "../data/data.csv"), sep=",", dec=".")
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
  updateSelectInput(session, "Depth", 
    choices = datafile()$depth %>% unique %>% as.numeric) 
  })
  # Project
  observe({
  updateSelectInput(session, "Project", 
    choices = datafile()$project %>% levels) 
  })
  # Land use
  observe({
  updateSelectInput(session, "Landuse", 
    choices = datafile()$landuse %>% levels) 
  })
  # Station
  observe({
  updateSelectInput(session, "Station", 
    choices = datafile()$station %>% levels) 
  })
  # Date of observation
  observe({
  updateSelectInput(session, "Date", 
    choices = datafile()$date_obs %>% levels) 
  })
  # Sensor Type
  observe({
  updateSelectInput(session, "SensorType", 
    choices = datafile()$sensorType %>% levels) 
  })
  # Sensor Name
  observe({
  updateSelectInput(session, "SensorName", 
    choices = datafile()$sensorName %>% levels) 
  })
  # Soil Type
  observe({
  updateSelectInput(session, "SoilType", 
    choices = datafile()$soilType %>% levels) 
  })
  
# For storing which rows have been excluded by "toggle points".
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(data))
  )
  
# Create new data object (reactive)
  data <- reactive ({
    # Project
    if ( length(input$Project) == 0 ) {  
      sproject <- c(NA, datafile()$project %>% levels)
    } else {
      sproject <- input$Project
    }
    # Landuse
    if ( length(input$Landuse) == 0 ) {
      slanduse <- c(NA, datafile()$landuse %>% levels)
    } else {
      slanduse <- input$Landuse
    }
    # Station
    if ( length(input$Station) == 0 ) {
      sstation <- c(NA, datafile()$station %>% levels)
    } else {
      sstation <- input$Station
    }
    # Date
    if ( length(input$Date) == 0 ) {
      sdate <- c(NA, datafile()$date %>% levels)
    } else {
      sdate <- input$Date
    }
    # Soil depth
    if ( length(input$Depth) == 0 ) {
      sdepth  <- c(NA, datafile()$depth %>% unique %>% as.numeric)
    } else {
      sdepth  <- input$Depth
    }
    # Sensor Type
    if ( length(input$SensorType) == 0 ){
      ssensorType   <- c(NA, datafile()$sensorType  %>% levels)
    } else {
      ssensorType  <- input$SensorType
    }
    # Sensor Name
    if ( length(input$SensorName) == 0 ){
      ssensorName  <- c(NA, datafile()$sensorName  %>% levels)
    } else {
      ssensorName  <- input$SensorName
    }
    # Soil Type
    if ( length(input$SoilType) == 0 ){
      ssoilType  <- c(NA, datafile()$soilType  %>% levels)
    } else {
      ssoilType  <- input$SoilType
    }
# filter data object, due to user choice and give back as data.frame    
    data <- datafile() %>% dplyr::filter(project %in% sproject, 
      station %in% sstation, landuse %in% slanduse, date_obs %in% sdate, 
      sensorType %in% ssensorType, sensorName %in% ssensorName, 
      soilType %in% ssoilType, depth %in% sdepth)
    return(as.data.frame(data))
  })
  
# Data table to output. 
  output$table <- renderDataTable({
    # define data
    data <- data()
    # rownames 
    data$row.name <- rownames(data)
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
    
    # facet ggplot
    if (input$facet) {
      # ggplot meanstation vs. meansample
      p <- ggplot2::ggplot( keep[!is.na(keep)[,1], ],
        ggplot2::aes(x = meanstation, y = meansample, label=ID) ) +
        # abline white x=y line
        ggplot2::geom_abline(intercept = 0, slope = 1, colour = "white") + 
        # plot points - scatter with exclude data set
        ggplot2::geom_point(data = exclude, fill = NA, color = "black", alpha = 0.25) +
        # set limits of cartesian coordinate system (from 0 to 0.6)
        ggplot2::coord_cartesian(xlim = c(0, .60), ylim = c(0, .60)) + 
        # apply facet grid: Soil depth vs. Land use
        ggplot2::facet_grid(depth ~ landuse)
    # no facet grid ggplot  
    } else {
      # zoom in if input$Zoom = TRUE
      if (input$Zoom) {
        # set limits for zoom
        xlim <- ylim <- c(0, .6); xypos <- .55
      } else {
        # set limits for no zoom
        xlim <- ylim <- c(0, .85); xypos <- .8
      }
      # ggplot meanstation vs. meansample
      p <- ggplot2::ggplot(keep, 
        ggplot2:: aes(x = meanstation, y = meansample, label=ID)) +
        # abline white x=y line
        ggplot2::geom_abline(intercept = 0, slope = 1, colour = "white") + 
        # write text x=y
        ggplot2::geom_text(x = xypos, y = xypos, label = "y = x", color = "white") +
        ggplot2::geom_text(x = 0.05, y = 0.05, label = "y = x", color = "white") +
        # plot points - scatter with exclude data set
        ggplot2::geom_point(data = exclude, fill = NA, color = "black", alpha = 0.25) +
        # set limits of cartesian coordinate system, defined above
        ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
    }
    # facte grid or not
    if (input$facet)
    {
      # MM-type regressor: SMCcalibration::fitSMDM
      if (input$robust) {
        p <- p + 
          # add robust linear model to ggplot
          ggplot2:: geom_smooth(method = fitSMDM, fullrange = TRUE, 
                       color = "grey")
      # OLS method: stats::lm  
      } else {
        p <- p + 
          # add linear model (OLS) to ggplot
          ggplot2::geom_smooth(method = lm, fullrange = TRUE, color = "grey")
      }
    } else {
      # MM-type regressor: SMCcalibration::fitSMDM
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
    # MM-type regressor: SMCcalibration::fitSMDM
    if (input$robust) {
      # robust model estimation
      fit_rlm <- fitSMDM(formula = meansample ~ meanstation,
        data = keep)
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
      fit_rlm <- lm(formula = meansample ~ meanstation, data = keep)
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
      if (any(names(stations) %in% c("lat","lon"))) {
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
          leaflet::addAwesomeMarkers(lng = stations$lon %>% as.character %>% as.numeric, 
            lat = stations$lat %>% as.character %>% as.numeric, icon = c1, 
            popup = paste("Name:", stations$station, "<br>","Landuse:", 
              "<br>", "Project:", stations$project, "<br>", "Altitude:", 
              stations$alt)) %>%
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
