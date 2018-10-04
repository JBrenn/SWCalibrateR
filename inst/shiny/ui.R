# --------------------------
# shiny ui
ui <- shiny::fluidPage(
  
  shiny::sidebarLayout(fluid = T, position = "left",
                
                # Sidebar with a slider input
                shiny::sidebarPanel(width=2,
                  # multiple selection for:
                  # Project
                  shiny::selectInput("Project", label = shiny::h4("project"),
                    "placeholder1", multiple=T),
                  # Land use
                  shiny::selectInput("Landuse", label = shiny::h4("land use"),
                    "placeholder2",multiple=T),
                  # Soil depth
                  shiny::selectInput("Depth", label = shiny::h4("soil depth"),
                    "placeholder3",multiple=T),
                  # Station
                  shiny::selectInput("Station", label = shiny::h4("station"),  
                    "placeholder4",multiple=T),
                  # Date
                  shiny::selectInput("Date", label = shiny::h4("date"),  
                    "placeholder5",multiple=T),
                  # Soil Type
                  shiny::selectInput("SoilType", label = shiny::h4("soil type"),  
                    "placeholder6",multiple=T),
                  # Sensor Type
                  shiny::selectInput("SensorType", label = shiny::h4("sensor type"),
                    "placeholder7",multiple=T), 
                  # Sensor Name
                  shiny::selectInput("SensorName", label = shiny::h4("sensor name"),
                    "placeholder8",multiple=T),
                  shiny::br(),
                  # checkbox "robust estimates"
                  shiny::checkboxInput("robust", label = "robust estimates", 
                    value = FALSE),
                  shiny::br(),
                  # checkbox "facet grid"
                  shiny::checkboxInput("facet", label = "facet grid", 
                    value = FALSE),
                  # checkbox "show row.names"
                  shiny::checkboxInput("Rownames", label = "show row.names", 
                    value = FALSE),
                  # checkbox "zoom in"
                  shiny::checkboxInput("Zoom", label = "zoom in", value = FALSE),
                  # file input
                  shiny::fileInput('datafile', 'Choose CSV file', 
                    accept = c('text/csv', 
                      'text/comma-separated-values,text/plain', '.csv'))
                # end sidebar panel  
                ),
                # start main panem
                shiny::mainPanel(
                  shiny::tabsetPanel(
                    # 1 Model Fit
                    shiny::tabPanel("Model Fit", 
                      shiny::plotOutput("plot1", height = 800, width = 800,
                        click = "plot1_click", 
                        brush = shiny::brushOpts(id = "plot1_brush")),
                      shiny::actionButton("exclude_toggle", "Toggle points"),
                      shiny::actionButton("exclude_reset", "Reset")),
                    # 2 Model Diagnostics
                    shiny::tabPanel("Diagnostics", 
                      shiny::plotOutput("plot2", height = 1000, width = 1000,
                        click = "plot2_click", 
                        brush = shiny::brushOpts(id = "plot2_shiny::brush"))),
                    # 3 Data Table
                    shiny::tabPanel("Data Table",
                      shiny::br(),
                      shiny::dataTableOutput("table")),
                    # 4 Map
                    shiny::tabPanel("Map", 
                      leaflet::leafletOutput("map")),
                    # 5 Description
                    shiny::tabPanel("Description", 
                      shiny::br(),
                      shiny::h4("Side Panel for Data Subset"),
shiny::p("The left side panel enables subsetting of the data set. Use the 
integrated example data set to trial or choose a data set to upload. 
The data set requires the following column categories (column names in bold): 
(1) name of the research" , shiny::strong("project"),  
"(2) ", shiny::strong("station"), "name, 
geographic coordinates at station consisting of 
(3) ", shiny::strong("lat"),"itude and 
(4) ", shiny::strong("lon"),"gitude, 
(5) ", shiny::strong("alt"),"itude of station in m a.s.l. 
(6) ", shiny::strong("date_obs"),": date of observation, 
(7) ", shiny::strong("landuse")," type at station, 
(8) soil ", shiny::strong("depth")," of soil moisture measurement, 
(9) ", shiny::strong("soilType"),": soil type at station, 
(10) ", shiny::strong("sensorType"),": generic soil moisture sensor type and 
(11) ", shiny::strong("sensorName"),": specific soil moisture sensor name of taking in-situ measurement, 
(12) ", shiny::strong("meanstation"),": average VWC measured by soil moisture sensor 
(±1h time of observation) 
(13) ", shiny::strong("samples"),": soil core VWC 
(14) ", shiny::strong("meansample"),": average soil core VWC (by default three soil core replicates are sampled per station, date time and soil depth). 
After data is loaded a handful of these categories can be used to subset the data set. 
Multiple selection for all categorical variables is implemented. 
An option for ggplot's facet_grid functionality is included. Klick", shiny::em("facet grid")," and the data set will be shown grouped by landuse and soil depth 
in the Model Fit panel. 
One can enable", shiny::em("Show row.names"), "to easily choose specific 
outlier to toggle. 
Zooming to a range of 0 to 60 %vol VWC is possible."),
                      shiny::p("For a description of the data set have a look at",
                        shiny::code("?SMCcalibration::SensorVSample")),
                      shiny::p(""),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Model Fit"),
                      shiny::p("The data subset is visualised in a scatter plot. Moreover, the statistical model with the 95% confidence intervall for the estimates is ablined. Estimates are computed either with the lm() function or with robust statistics (SMDM fit from the", shiny::strong("robustbase"), "R-package). One can toogle outlying points by klicking one or mark multiple and apply", shiny::em("Toogle points"), ". A helpful descision tool for indicating possible outliers are the diagnostic plots on the next panel."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Diagnostics"),
                      shiny::p("Four diagostic plots for the lm or roblm object 
are visualised:"), 
  shiny::p("(1) Residuals vs. Fitted Values:
Do the model residuals have non-linear patterns? 
Besides non-linearity of the residuals, this plot inherits a first hint towards
unequal error variances, and outliers."),
  shiny::p("(2) Normal Q−Q vs. Residuals:
Are the model residuals normally distributed? 
Normal distribution of the residual is an underlying assumption for OLS. 
Strong deviations from the 1:1 line support the suspicion that this 
assumtion is violated."),
  shiny::p("(3) Scale-Location:
Are the model residuals spread equally along the ranges of predictors? 
Alike in plot 1 we check the assumption of equal variance in the residuals
(homoscedasticity) this time applying standardised residuals."),
  shiny::p("(4) Residuals vs. Leverage (lm) or Robust Distances (lmrob):
Which are the influential outliers? 
Unlike the other plots, here patterns are not relevant. 
We simple look for cases outside the dashed line, Cook’s distance, for lm.
For lmrob the plot is divided in four regions marked by dashed lines: 
(1) Regular , (2) Outlier, (3) Leverage, (4) Outlier and Leverage. 
For detail on the methods we refer to Rousseeuw and van Zomeren (1990)."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Data Table"),
                      shiny::p("This panel contains the data table of the 
data subset."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Map"),
                      shiny::p("This panel contains a leaflet map of the 
locations of the data subset.")
                      )
                    )
                  )
    )
  )
