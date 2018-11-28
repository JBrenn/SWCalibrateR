# resampling / generalising data set
# for project and station names, land cover classes

# load original dataset
load("./data-raw/SensorVSample.RData")

#1 generalise project names
# levels
numberlevels <- 1:length(levels(data$`Project ID`))
# new names / levels
newlevels <- paste("Project", stringr::str_pad(numberlevels, 2, pad = "0"), sep=" ")
# add to current levels
levels(data$`Project ID`) <- c( levels(data$`Project ID`), newlevels)
# unique data frame subset entries
uniquedata <- unique(data$`Project ID`)
# run over # of unique enntries
ii <- 1:length( uniquedata )
# and change old to new levels / values
for (i in ii)
  data$`Project ID`[data$`Project ID` == uniquedata[i]] <- newlevels[i]
# reduce levels
data$`Project ID` <- factor(data$`Project ID`)

#2 generalise station names
# levels
numberlevels <- 1:length(levels(data$`Station ID`))
# new names / levels
newlevels <- paste("Station", stringr::str_pad(numberlevels, 2, pad = "0"), sep=" ")
# add to current levels
levels(data$`Station ID`) <- c( levels(data$`Station ID`), newlevels)
# unique data frame subset entries
uniquedata <- unique(data$`Station ID`)
# run over # of unique enntries
ii <- 1:length( uniquedata )
# and change old to new levels / values
for (i in ii)
  data$`Station ID`[data$`Station ID` == uniquedata[i]] <- newlevels[i]
# reduce levels
data$`Station ID` <- factor(data$`Station ID`)

#3 generalise land use
# new names / levels
newlevels <- c("Pastures", "Fruit trees and berry plantations", "Vineyards",
  "Broad-leaved forest", "Coniferous forest", "Mixed forest", "Natural grassland", 
  "Sparsely vegetated areas", "Transitional woodland/shrub")
# add new levels
levels(data$Landuse) <- c(levels(data$Landuse), newlevels)
# new land use class for each station
# unique data frame subset entries
uniquedata <- unique(data$`Station ID`)
# run over # of unique station entries
ii <- 1:length( uniquedata )
for (i in ii)
    data$Landuse[data$`Station ID` == uniquedata[i]] <- sample(x = newlevels, 
      size = 1, replace = TRUE)
# reduce levels
data$Landuse <- factor(data$Landuse)

#4 generalise sensor name 
# change "SensorMean\t" -> "SensorMean"
data$`Sensor ID`[data$`Sensor ID`=="SensorMean\t"] <- "SensorMean"
# levels
numberlevels <- 1:length(levels(data$`Sensor ID`))
# new names / levels
newlevels <- newlevels <- paste("Sensor", stringr::str_pad(numberlevels, 2, pad = "0"), sep=" ")
# add to current levels
levels(data$`Sensor ID`) <- c( levels(data$`Sensor ID`), newlevels)
# unique data frame subset entries
uniquedata <- unique(data$`Sensor ID`)
# run over # of unique enntries
ii <- 1:length( uniquedata )
# and change old to new levels / values
for (i in ii)
  data$`Sensor ID`[data$`Sensor ID` == uniquedata[i]] <- newlevels[i]
# reduce levels
data$`Sensor ID` <- factor(data$`Sensor ID`)

# reduce data set to 200 entries
data <- data[sample(x = 1:length(data$`Station ID`), 200), ]

# remove data row with NA data in SWC
data <- data[!(is.na(data$`Sensor VWC`) | is.na(data$`Sample VWC`)), ]

# save .RData in /data
devtools::use_data(data, overwrite = TRUE)

# write csv
write.csv(x = data, file = "data/data.csv", quote = FALSE, row.names = FALSE)

