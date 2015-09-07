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

