if (!require("Cairo")) install.packages("Cairo")
if (!require("robustbase")) install.packages("robustbase")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("devtools")) install.packages("devtools")

#if (!require("SMCcalibration")) devtools::install_github("JBrenn/SMCcalibration")

data("SensorVSample")
#rm(data)
#load("SensorVSample.RData")
#data<-read.csv("../data/SensorVSample_new.csv",sep=",",dec=".")

ui <- shiny::fluidPage(
  
  shiny::sidebarLayout(fluid = T, position = "left",
                
                # Sidebar with a slider input
                shiny::sidebarPanel(width=2,
                             
                             
                             
                             
                             #data$depth %>% unique %>% as.numeric  
                             #data$project %>% levels
                             #data$landuse %>% levels
                             #data$station %>% levels
                             #data$date_obs %>% levels
                             #data$sensorType %>% levels 
                             #data$sensorName %>% levels 
                             #data$soilType %>% levels
                             
                             
                             shiny::selectInput("Project", label = shiny::h4("project"),"placeholder1",  
                                         multiple=T),  
                             #shiny::selectInput("Project", label = shiny::h4("project"), 
                             #           choices = list("ALL","matsch","monalisa")),
                             
                             shiny::selectInput("Landuse", label = shiny::h4("land use"),  "placeholder2",multiple=T),               
                             #shiny::selectInput("Landuse", label = shiny::h4("land use"), 
                             #           choices = list("ALL","appleorchards","meadow","pasture","forest","grassland")),
                             
                             shiny::selectInput("Depth", label = shiny::h4("soil depth"),"placeholder3",multiple=T),#choices = list("ALL","5","20","40")
                             
                             #shiny::selectInput("Station", label = shiny::h4("station"), 
                             #           choices = list("ALL","B1","B2","B3","domef1500","domes1500","eppanberg","girlan","gries","I1","I3",
                             #                        "kaltern","lana6","latsch1","latsch3","latscshiny::h4","M1","M3","M4","M5","M6",
                             #                        "M7","nals","nemef1500","nemes1500","neumarkt","P1","P2","P3","S2","S4",
                             #                       "S5","stpauls","terlanalt","terlanneu","tramin13","unterrain","vimef2000","vimes2000","XS1")),
                             
                             shiny::selectInput("Station", label = shiny::h4("station"),  "placeholder4",multiple=T),           
                             
                             shiny::selectInput("Date", label = shiny::h4("date"),  "placeholder5",multiple=T),
                             #shiny::selectInput("Date", label = shiny::h4("date"), 
                             #           choices = list("ALL","2013","2014","2015")),
                             
                             shiny::selectInput("SoilType", label = shiny::h4("soil type"),  "placeholder6",multiple=T),              
                             #shiny::selectInput("SoilType", label = shiny::h4("soil type"), 
                             #           choices = list("ALL","sandy","loamy","clay")),           
                             
                             shiny::selectInput("SensorType", label = shiny::h4("sensor type"),  "placeholder7",multiple=T),           
                             #shiny::selectInput("SensorType", label = shiny::h4("sensor type"), 
                             #           choices = list("ALL","CS655","Decagon10HS","OnsetSMB005")),
                             
                             shiny::selectInput("SensorName", label = shiny::h4("sensor name"),  "placeholder8",multiple=T),            
                             #shiny::selectInput("SensorName", label = shiny::h4("sensor name"), selected = "SensorMean",
                             #           choices = list("ALL","SensorMean","A","B","C","CI","LSp","LBL","CSt","T","L","LSt","CSn","TSt","LS","TSn")),
                             
                             shiny::br(),
                             
                             shiny::checkboxInput("robust", label = "robust estimates", value = FALSE),
                             
                             shiny::br(),
                             
                             shiny::checkboxInput("facet", label = "facet grid", value = FALSE),
                             shiny::checkboxInput("Rownames", label = "show row.names", value = FALSE),
                             shiny::checkboxInput("Zoom", label = "zoom in", value = FALSE),
                             shiny::fileInput('datafile', 'Choose CSV file',
                                       accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))#,
                             #shiny::selectInput("upload_file", label = "use an uploaded file?",choices=c("default","uploaded"))#, value = FALSE
                             
                ),
                
                shiny::mainPanel(
                  shiny::tabsetPanel(
                    shiny::tabPanel("Model Fit",
         
         #fluidRow(
         #column(width = 6,
                shiny::plotOutput("plot1", height = 800, width = 800,
                           click = "plot1_click",
                           brush = shiny::brushOpts(
                             id = "plot1_brush"
                           ))
                # )
                #   ,
                #column(width = 4,
                # leafletOutput("map")
                # )
                ,
                # column(width = 4,
                shiny::actionButton("exclude_toggle", "Toggle points")
                #      )
                ,
                #column(width = 4,
                shiny::actionButton("exclude_reset", "Reset")
         #)
#)


),
                    shiny::tabPanel("Map Table", 
                             leafletOutput("map")),
                    
                    shiny::tabPanel("Diagnostics", shiny::plotOutput("plot2", height = 1000, width = 1000,
                                                       click = "plot2_click",
                                                       brush = shiny::brushOpts(
                                                         id = "plot2_shiny::brush"
                                                       ))),
                    shiny::tabPanel("Data Table", 
                             shiny::br(),
                             shiny::dataTableOutput("table")),
                    
                    shiny::tabPanel("Description", 
                             shiny::br(),
                             shiny::h4("Side Panel for Data Subsetting"),
                             shiny::p("The left side panel enables subsetting of the data set. By default the whole unique data set is used. An option for ggplot's facet_grid functionality is included. Klick", shiny::em("facet grid"), "and the data set will be shown grouped by landuse and soil depth in the Model Fit panel. One can enable", shiny::em("Show row.names"), "to easily choose points to remove. Zooming to the data range of 0 to 60 %vol is also possible."),
                            shiny::p("For a description of the data set have a look at", shiny::code("?SensorVSample")),
                             shiny::p(""),
                             shiny::br(),
                             shiny::hr(),
                             shiny::h4("The Model Fit Panel"),
                             shiny::p("The data subset is visualised in a scatter plot. Moreover, the statistical model with the 95% confidence intervall for the estimates is ablined. Estimates are computed either with the lm() function or with robust statistics (SMDM fit from the", shiny::strong("robustbase"), "R-package). One can toogle outlying points by klicking one or mark multiple and apply", shiny::em("Toogle points"), ". A helpful descision tool for indicating possible outliers are the diagnostic plots on the next panel."),
                             shiny::br(),
                             shiny::hr(),
                             shiny::h4("The Diagnostic Panel"),
                             shiny::p("Four diagostic plots for the roblm object are visualised: (1) Standardized residuals vs. Robust Distances, (2) Normal Q-Q vs. Residuals, (3) Residuals vs. Fitted Values, (4) Sqrt of abs(Residuals) vs. Fitted Values"),
                             shiny::br(),
                             shiny::hr(),
                             shiny::h4("The Data Table Panel"),
                             shiny::p("The last panel contains the data table of the data subset."))
                  )
                )
  )
)
