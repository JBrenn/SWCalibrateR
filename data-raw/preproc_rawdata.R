# preproc raw data file

data <- read.csv("data-raw/data.csv.back")

# rename col
colnames(data) <- c("Project ID", "Station ID", "Landuse", "Date", "Soil depth", "Sensor type", "Sensor VWC", "Sample VWC", "Sensor ID", "Soil type", "Latitude", "Longitude", "Altitude")

# unique rows
data <- unique(data)

# write data to SensorVSamle.RData
save(list = "data", file = "data-raw/SensorVSample.RData")
