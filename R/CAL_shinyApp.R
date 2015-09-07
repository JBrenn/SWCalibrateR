ui <- fluidPage(
  
  sidebarLayout(fluid = T, 
    
    # Sidebar with a slider input
    sidebarPanel(width=2,
                 
      selectInput("Project", label = h3("Project"), 
                  choices = list("ALL","matsch","monalisa")),
      
      selectInput("Landuse", label = h3("Landuse"), 
                  choices = list("ALL","appleorchards","meadow","pasture","forest","grassland")),
      
      selectInput("Depth", label = h3("Depth"), 
                  choices = list("ALL","5","20","40")),
      
      selectInput("Station", label = h3("Station"), 
                  choices = list("ALL","stpauls","eppanberg","kaltern","I1","I3","P1","P2","P3","B1","B2",
                                 "B3","M1","M4","M5","M6","M7","S2","S4","S5","lana6","nals","terlanalt","terlanneu",
                                 "M3","XS1","unterrain","gries","girlan","tramin13","neumarkt",
                                 "latsch1","latsch3","latsch4","nemef1500","domef1500","vimef2000","vimes2000")),
      
      selectInput("Date", label = h3("Date"), 
                  choices = list("ALL","2013","2014","2015")),
      
      selectInput("SensorType", label = h3("SensorType"), 
                  choices = list("ALL","CS655","Decagon10HS","OnsetSMB005")),
      
      selectInput("SensorName", label = h3("SensorName"), 
                  choices = list("ALL","CI","Lsp","LBL","Cst","T","L","Lst","Csn","Tst"))
      
    ),
  
  mainPanel(
           plotOutput("plot1", height = 800, width = 800,
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           ),
           
           actionButton("exclude_toggle", "Toggle points"),
           actionButton("exclude_reset", "Reset")
    )
  )
)

server <- function(input, output) {
  
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(data))
  )
  
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
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- data[ vals$keeprows, , drop = FALSE]
    exclude <- data[!vals$keeprows, , drop = FALSE]
    
    ggplot(keep, aes(meanstation, meansample)) +
      geom_abline(intercept = 0, slope = 1, colour = "white") + 
      geom_text(x = 0.55, y = 0.55, label = "y = x", color = "white") +
      geom_text(x = 0.05, y = 0.05, label = "y = x", color = "white") +
      geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, shape = 21, color = "black") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      geom_text(x = 0.45, y = 0.05, label = lm_eqn(keep), parse = TRUE) +
      coord_cartesian(xlim = c(0, 0.6), ylim = c(0,0.6))
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