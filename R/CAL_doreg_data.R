CAL_doreg_data <- function (data, date_obs=NA, landuse=NA, depth=NA, station=NA, project=NA, sensorType = NA, sensorName=NA,soilType=NA, preserveStr=T)
{
  choices <- c(project=project, station=station, landuse=landuse, depth=depth, date_obs=date_obs, soilType=SoilType, sensorName=sensorName, sensorType=sensorType)
  choices.na <- is.na(choices)
  
  if (preserveStr) {
    
    for (i in 1:length(choices.na))
    {
      
      if (!choices.na[i])
      {
        
        if (names(choices)[i]=="date_obs") {
          bool <- !grepl(choices[i],as.character(data$date_obs))
          bool <- ifelse(is.na(bool), TRUE, bool)
          data[bool,] <- NA
        } else {
          bool <- data[names(choices)[i]]!=choices[i]
          bool <- ifelse(is.na(bool), TRUE, bool)
          data[bool,] <- NA
        }
        
      }
      
    }
    
  } else {
    
    for (i in 1:length(choices.na))
    {
      
      if (!choices.na[i])
      {
        
        if (names(choices)[i]=="date_obs") {
          data <- data[grepl(choices[i],as.character(data$date_obs)),]
        } else {
          data <- data[data[names(choices)[i]]==choices[i],]
        }
        
      }
      
    }
    
  }
 
  return(data) 
}  
