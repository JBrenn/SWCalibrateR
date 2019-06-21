# --------------------------
# shiny ui
ui <- shiny::fluidPage(
  
  shiny::sidebarLayout(fluid = T, position = "left",
                
                # Sidebar with a slider input
                shiny::sidebarPanel(width=2,
                  # multiple selection for:
                  # Project
                  shiny::selectInput("Project.ID", label = shiny::h4("Project ID"),
                    "placeholder1", multiple=T),
                  # Land use
                  shiny::selectInput("Landuse", label = shiny::h4("Landuse"),
                    "placeholder2", multiple=T),
                  # Soil depth
                  shiny::selectInput("Soil.depth", label = shiny::h4("Soil depth"),
                    "placeholder3", multiple=T),
                  # Station
                  shiny::selectInput("Station.ID", label = shiny::h4("Station ID"),  
                    "placeholder4", multiple=T),
                  # Date
                  shiny::selectInput("Date", label = shiny::h4("Date"),  
                    "placeholder5", multiple=T),
                  # Soil Type
                  shiny::selectInput("Soil.type", label = shiny::h4("Soil type"),  
                    "placeholder6", multiple=T),
                  # Sensor Type
                  shiny::selectInput("Sensor.type", label = shiny::h4("Sensor type"),
                    "placeholder7", multiple=T), 
                  # Sensor Name
                  shiny::selectInput("Sensor.ID", label = shiny::h4("Sensor ID"),
                    "placeholder8", multiple=T),
                  shiny::br(),
                  # checkbox "robust estimates"
                  shiny::checkboxInput("robust", label = "robust estimates", 
                    value = FALSE),
                  shiny::br(),
                  # checkbox "show row.names"
                  shiny::checkboxInput("Rownames", label = "show row.ID", 
                    value = FALSE),
                  # checkbox "zoom in"
                  shiny::checkboxInput("Zoom", label = "zoom in", value = FALSE),
                  # file input
                  shiny::fileInput('datafile', 'Choose CSV file', 
                    accept = c('text/csv', 
                      'text/comma-separated-values,text/plain', '.csv')), 
                  shiny::br(),
                  absolutePanel(
                    bottom = 20,
                    left = 25,
                    draggable = F,
                    width='100%',
                    height='auto',
                    p(a(shiny::icon('github fa-2x'),href='https://github.com/JBrenn/SWCalibrateR',target='_blank')))
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
shiny::p("The left side panel enables subsetting of the data set. Familiarise yourself with the features the application offers by using the integrated example data set. Upload your own data set via the
", shiny::em("Choose CSV file"), "tab and you are ready to analyse it. Keep care about 
the data set headers! For orientation you can load the example data set in your workspace:" , shiny::code("data('data')"), "."), 
shiny::p("The data set does require the following categories (header names in bold): 
(1) name of the research"   , shiny::strong("Project ID"), ", 
(2) name of the measurement", shiny::strong("Station ID"), ", 
geographic coordinates of the station consisting of 
(3) " , shiny::strong("Latitude")," and 
(4) " , shiny::strong("Longitude"),", 
(5) " , shiny::strong("Altitude"),"of station in m a.s.l. 
(6) " , shiny::strong("Date"),"of observation [DD/MM/YYYY], 
(7) " , shiny::strong("Landuse"),"type at station, 
(8) " , shiny::strong("Soil depth"),"of soil moisture measurement in cm, 
(9) " , shiny::strong("Soil type:"),"soil type at station, 
(10) ", shiny::strong("Sensor Type:"),"generic soil moisture sensor type and 
(11) ", shiny::strong("Sensor ID:"),"specific soil moisture sensor name, measuring VWC in-situ, 
(12) ", shiny::strong("Sensor VWC:"),"average VWC measured by soil moisture sensor 
(±1h time of observation) 
(13) ", shiny::strong("Sample VWC:"),"average soil core VWC (by default three soil core replicates are sampled and averaged per station, date time and soil depth). VWC value range is [0;1]."), 
                      shiny::p("After data is loaded a handful of these categories can be used to subset the data set. Multiple selection for all categorical variables is implemented. 
An option for ggplot's facet_grid functionality is included. Klick", shiny::em("facet grid")," and the data set will be shown grouped by landuse and soil depth 
in the Model Fit panel. 
One can enable", shiny::em("Show row.names"), "to easily choose specific 
outlier to toggle. 
Zooming to a range of 0 to 0.6 [%vol/%vol] VWC is possible."),
                      shiny::p("For a description of the data set have a look at",
                        shiny::code("?SMCcalibration::SensorVSample"), ". You can load the example data set in R with",
                      shiny::code("data('data')"),"."),
                      shiny::p(""),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Model Fit"),
                      shiny::p("The data subset is visualised in a scatter plot. Moreover, the statistical model is ablined with its 95% confidence intervall for the estimate. The model is estimated either by means of ordinary-least squares (OLS fit with", shiny::a("stats::lm", href="https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lm"), ") or by robust statistics (SMDM fit with" , shiny::a("robustbase::lmrob", href="https://www.rdocumentation.org/packages/robustbase/versions/0.93-3/topics/lmrob"), "). One can toogle outlying points by klicking a single one or mark multiple and apply", shiny::em("Toogle points"), ". Outlier points may influence your model estimate. A helpful descision tool for indicating such points are the diagnostic plots on the next panel."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Diagnostics"),
                      shiny::p("Four diagostic plots for the lm or roblm object 
are visualised. All of them aim to answer a specific question we do have in mind when identifying influencing leverage outlier points."), 
  shiny::p("(1) Residuals vs. Fitted Values:
Do the model residuals have non-linear patterns? 
Besides non-linearity of the residuals, this plot inherits a first hint towards
unequal error variances, and outliers."),
  shiny::p("(2) Normal Q−Q vs. Residuals:
Are the model residuals normally distributed? 
Normal distribution of the residual is an underlying assumption for OLS. 
Strong deviations from the x=y line support the suspicion that this 
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
data subset. Sort and search is enabled for each category as well as global search."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Map"),
                      shiny::p("This panel contains a leaflet map of the 
locations of the data subset observation points.")
                      ),
                    #5 Impressum
                    shiny::tabPanel("Notice",
                      shiny::br(),
                      shiny::h4("Abstract"),
                      shiny::p("SWCalibrateR is a user-friendly web application. We designed SWCalibrateR to interactively estimate linear regression relationships of any couple of field data series. We specifically developed this toolbox to calibrate soil moisture sensors based on gravimetric soil moisture samples. The application has been implemented using R-shiny (https://shiny.rstudio.com/)."),
                      shiny::p("As a user you can upload your own dataset and dynamically filter it by categories like soil type, land use, soil depth and others. With SWCalibrateR you can visualise the filtered data scatter and the estimated linear model. You can diagnose your model estimate and thus easily remove outliers influencing your model estimate. SWCalibrateR handles robust estimates of linear models besides ordinary least square estimation. Additional features are an interactive data table view and mapping of the data points."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Funding statement"),
                      shiny::p("This work was supported by the farming consulting centre for fruticulture and viticulture “Südtiroler Beratungsring” and the research grant “MONALISA” of the Provincia Autonoma di Bolzano, Alto Adige, Ripartizione Diritto allo studio, Università e ricerca scientifica."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Keywords"),
                      shiny::p("R, Shiny, Soil moisture, Calibration, TDR"),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("How to cite"),
                      shiny::p("1. Brenner J, Genova G, Bertoldi G, Niedrist G and Della Chiesa, S (2019) SWCalibrateR: Interactive, Web – Based Calibration of Soil Moisture Sensors. Journal of Open Research Software, 7(1), p.20. ", shiny::a("DOI: 10.5334/jors.254.", href="https://doi.org/10.5334/jors.254")),
                      shiny::p("2. Brenner J, & Genova G. (2019, March 15). JBrenn/SWCalibrateR: version 1.1.0 (Version v1.1.0). Zenodo. ", shiny::a("DOI: 10.5281/zenodo.1745327.", href="http://doi.org/10.5281/zenodo.1745327"))
                      )
                    )
                  )
    )
  )
