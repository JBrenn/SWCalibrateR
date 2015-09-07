library(shiny)
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

#setwd("/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv//Projekte/MONALISA/05_Arbeitsbereiche/BrJ/05_calibrationSWC")
#setwd("/home/jbr/Schreibtisch/05_calibrationSWC")

source("lm_eq.R")
source("shinyApp.R")
source("FUN_doreg_data.R")

#choose data
data <- read.csv("SUMMARY_TOTAL.csv")
data <- unique(data[,-8])

# data <- doreg_data(data = data, station = "B1")
# linMod <- doreg(data = data, station = "B1", pdf = F)

shinyApp(ui, server)
