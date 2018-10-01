# --------------------------
# preface
# install missing packages

if (!require("Cairo")) install.packages("Cairo")
if (!require("robustbase")) install.packages("robustbase")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("devtools")) install.packages("devtools")

#if (!require("SMCcalibration")) devtools::install_github("JBrenn/SMCcalibration")

#data("SensorVSample")
#rm(data)
#load("SensorVSample.RData")
#data<-read.csv("../data/SensorVSample_new.csv",sep=",",dec=".")

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
                      leafletOutput("map")),
                    # 5 Description
                    shiny::tabPanel("Description", 
                      shiny::br(),
                      shiny::h4("Side Panel for Data Subsetting"),
shiny::p("The left side panel enables subsetting of the data set. 
By default the whole unique data set is used. An option for ggplot's facet_grid 
functionality is included. Klick", shiny::em("facet grid"), "and the data set 
will be shown grouped by landuse and soil depth in the Model Fit panel. 
One can enable", shiny::em("Show row.names"), "to easily choose points to remove. 
Zooming to the data range of 0 to 60 %vol is also possible."),
                      shiny::p("For a description of the data set have a look at",
                        shiny::code("?SensorVSample")),
                      shiny::p(""),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("The Model Fit Panel"),
                      shiny::p("The data subset is visualised in a scatter plot. Moreover, the statistical model with the 95% confidence intervall for the estimates is ablined. Estimates are computed either with the lm() function or with robust statistics (SMDM fit from the", shiny::strong("robustbase"), "R-package). One can toogle outlying points by klicking one or mark multiple and apply", shiny::em("Toogle points"), ". A helpful descision tool for indicating possible outliers are the diagnostic plots on the next panel."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Diagnostics"),
                      shiny::p("Four diagostic plots for the roblm object are visualised: (1) Standardized residuals vs. Robust Distances, (2) Normal Q-Q vs. Residuals, 
(3) Residuals vs. Fitted Values, (4) Sqrt of abs(Residuals) vs. Fitted Values"),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Data Table"),
                      shiny::p("This panel contains the data table of the 
                        data subset."),
                      shiny::br(),
                      shiny::hr(),
                      shiny::h4("Map"),
                      shiny::p("This panel contains a leaflet map of the locations
                        of the data subset.")
                      )
                    )
                  )
    )
  )
