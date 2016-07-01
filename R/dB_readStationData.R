# function read station data

#ARGUMENTS
# path        path to data files
# header.file path and name headerfile
# station    
#e.g.
# path <- "/media/alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/M/M3/"
# header.file <- "/media/alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/M/header_M3.txt"

dB_readStationData <- function(path, header.file, station)
{
# load libraries
#  require(zoo)
#  require(chron)
  
# supress chron year abbreviation  
  options(chron.year.abb = FALSE)
  
# get file names  
  files <- dir(path)
  
# get header
  header <- as.character(read.table(header.file, header=FALSE, sep=",")[,1])
  header_org <- header

  station_gen <- substr(station,1,nchar(station)-4)

  if (station_gen=="BERAT") {
    skip <- 1; date_col=2; tz="Etc/GMT+1"; time_step=times("00:05:00")
  } else time_step <- times("00:15:00")
  
  if (station_gen=="SF") {
    skip <- 1; date_col=2; tz="Etc/GMT+1"
  }
  if (station_gen=="B") {
    skip <- 4; date_col=1; tz="Etc/GMT-2"
    header_final <- paste(substr(header.file, 1, nchar(header.file)-16), "header_final.txt", sep="")
    header_final <- as.character(read.table(header_final, header=FALSE)[,1])
  }
  if (station_gen=="P"| station_gen=="I") {
    skip <- 4; date_col=1; tz="Etc/GMT-1"
  }
  if (station_gen=="M" | station_gen=="S") {
    skip <- 1; date_col=2; tz="Etc/GMT+1"
    header_final <- paste(substr(header.file, 1, nchar(header.file)-16), "header_final.txt", sep="")
    header_final <- as.character(read.table(header_final, header=FALSE)[,1])
  }
  if (station_gen=="XS") {
    skip <- 1; date_col=2; tz="Etc/GMT+1"
    header_final <- paste(substr(header.file, 1, nchar(header.file)-17), "header_final.txt", sep="")
    header_final <- as.character(read.table(header_final, header=FALSE)[,1])
  }
  if (station=="S2") {
    skip <- 1; date_col=2; tz="Etc/GMT+2"
  }
  if (station_gen=="DOMEF" | station_gen=="DOMES" | station_gen=="DOPAS" | station_gen=="NEMEF") {
    skip <- 4; date_col=1; tz="Etc/GMT+1"
  }
 
# read data 
  data <- rep(NA,length(header))
  datetime <- chron(dates.  = "2012-02-02", times. = "01:00:00", 
                    format= c(dates = "y-m-d", times = "h:m:s"))

# do not work with POSIX as timezone is needed, better solution chron
#datetime <- as.POSIXct(strptime(x="2012-02-02 00:00", format="%Y-%m-%d %H:%M", tz=tz))

  for (i in files)
  {
    # change header where needed
    # M2 
    if (i == "M2 Station total_2014_07_07_TO_2014_11_14.csv"| i=="M2 Station total_2014_11_14_TO_2015_07_09.csv") {
      header.file_ <- paste(substr(header.file, 1, nchar(header.file)-16), "header_M2_2015.txt", sep="")
      header <- as.character(read.table(header.file_, header=FALSE)[,1])
    }
    # M3 
    if (i == "M3_total_2016.csv") {
      header.file_ <- paste(substr(header.file, 1, nchar(header.file)-16), "header_M3_2016.txt", sep="")
      header <- as.character(read.table(header.file_, header=FALSE)[,1])
    }
    
    nas <- c("NaN","7777","-888.88", "-999", "NAN","NA","-888.880", "None")
    
    # whole data frame
    if (i=="P3_YEAR_2016.csv") {
      dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                        na.strings = nas)
      dummy$V41 <- NA
      dummy$V42 <- NA
    } else if (i=="B3_2000_YEAR_2014.csv" | i=="B3_2000_YEAR_2015.csv" | 
        i=="B1_1000_YEAR_2016.csv" | i=="B2_1500_YEAR_2016.csv" | i=="B3_2000_YEAR_2016.csv") {
        dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, dec=".",
                           na.strings = nas)
        dummy <- dummy[,1:dim(data)[2]]
    } else if (i=="B3_2000_YEAR_2010.csv" | i=="B3_2000_YEAR_2012.csv") {
        dummy <- read.csv2(file.path(path,i), skip=skip, header=FALSE, 
                           na.strings = nas)
    } else if (i=="I1_YEAR_2014.csv") {
        dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                          na.strings = nas)
        dummy <- dummy[,-1]
        names(dummy) <- paste("V", 1:length(dummy), sep="")
    } else if (i=="M1_total_2014-2015.csv" | i=="P3_YEAR_2015.csv") {
        dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                            na.strings = nas)
        dummy <- dummy[,1:length(header)]
    } else if (i=="M5_total_2015.csv") {
      dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                        na.strings = nas)
      dummy$V12 <- NA; dummy$V13 <- NA
    } else {
        dummy <- read.csv(file.path(path,i), skip=skip, header=FALSE, 
                          na.strings = nas)
    } 
    
    if (exists("header.file_")) {
      # reorder data
      names(dummy) <- header
      dummy <- dummy[header_org]
      names(dummy) <- names(data)
      rm(header.file_)
    }
    
    if (length(header) > dim(dummy)[2])
    {
      for (i in (dim(dummy)[2]+1):length(header))
        dummy[,i] <- NA
      dummy <- as.data.frame(dummy)
    }
    
    # extract date and time
    
    # "%Y-%m-%d %H:%M:%S"
    if (substr(as.character(dummy[1,date_col]),5,5)=="-" & nchar(as.character(dummy[1,date_col]))==19)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = substr(dummy[,date_col],12,19), 
                          format= c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%Y-%m-%d %H:%M", tz=tz)) )
    # "%Y-%m-%d %H:%M"
    if (substr(as.character(dummy[1,date_col]),5,5)=="-" & nchar(as.character(dummy[1,date_col]))==16)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = paste(substr(dummy[,date_col],12,16),"00",sep=":"), 
                          format= c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%Y-%m-%d %H:%M", tz=tz) ))
    # "%Y/%m/%d %H:%M"
    if (substr(as.character(dummy[1,date_col]),3,3)=="/" & nchar(as.character(dummy[1,date_col]))==16)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = paste(substr(dummy[,date_col],12,16),"00",sep=":"), 
                          format= c(dates = "d/m/y", times = "h:m:s"),
                          out.format = c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%d/%m/%Y %H:%M", tz=tz) ))
    # "%Y/%m/%d %H:%M:%S"
    if (substr(as.character(dummy[1,date_col]),3,3)=="/" & nchar(as.character(dummy[1,date_col]))==19)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = substr(dummy[,date_col],12,19), 
                          format= c(dates = "d/m/y", times = "h:m:s"),
                          out.format = c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%d/%m/%Y %H:%M", tz=tz)) )
    
    if (substr(as.character(dummy[1,date_col]),3,3)=="." & nchar(as.character(dummy[1,date_col]))==16)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = paste(substr(dummy[,date_col],12,16),"00",sep=":"), 
                          format= c(dates = "d.m.y", times = "h:m:s"),
                          out.format = c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%d.%m.%Y %H:%M", tz=tz) ))
    if (substr(as.character(dummy[1,date_col]),3,3)=="." & nchar(as.character(dummy[1,date_col]))==19)
      datetime <- c(datetime,
                    chron(dates.  = substr(dummy[,date_col],1,10), 
                          times. = substr(dummy[,date_col],12,19), 
                          format= c(dates = "d.m.y", times = "h:m:s"),
                          out.format = c(dates = "y-m-d", times = "h:m:s")))
#as.POSIXct(strptime(x=dummy[,date_col], format="%d.%m.%Y %H:%M", tz=tz)) )
    
    # cut data in M7 2014-07-12 to 2014-09-12 - Hobo problems
    if (i == "M7 total 2009-2014.csv") 
    {
      cut_rows <- which(as.numeric(datetime) > as.numeric(chron(dates. = "07/12/2014", times. =  "00:00:00")) &
                        as.numeric(datetime) < as.numeric(chron(dates. = "09/12/2014", times. =  "23:45:00")))
      
      for (cols2NA in 3:13) {
        dummy[cut_rows,cols2NA] <- NA
      }
    }
    
    data <- rbind(data,dummy)
  }
  
  datetime <- datetime[-1]
  data <- data[-1,-c(1:date_col)]
  
  # remove datetime NAs
  nas <- which(is.na(datetime))
  
  if(length(nas) > 0) 
  {
    datetime <- datetime[-nas]
    data <- data[-nas,]
  }
  
  # name data with original header names
  names(data) <- header_org[-c(1:date_col)]
  
  if (exists("header_final")) {
    # add NA cols
    for (i in header_final) {
      if (i == "RECORD") {
        data$RECORD <- 1:(dim(data)[1])
      } else if (i == "TIMESTAMP") {
        data$TIMESTAMP <- as.numeric(datetime)
      } else if (! i %in% names(data)) {
        data[,as.character(i)] <- NA
      }
    }
      
    # reorder data
    data <- data[header_final]
    
    # remove NA cols
    for (i in header_final) {
      if (all(is.na(data[,as.character(i)]))) {
        data <- data[,-which(i == names(data))]
      } 
    }
  }
  
  # remove duplicate datetimes
  # data_ <- data[-anyDuplicated(time(data))]
  
  # create regular zoo object
  zoo.data <- zoo(x=data, order.by=datetime)
  
  # make regular zoo.object
  if(!is.regular(zoo.data, strict = TRUE))
  {
    print("data set not strictly regular, look for double dates...")
    # make regular
    g <- zoo(x = NA, seq(head(index(zoo.data),1), tail(index(zoo.data),1), by=time_step))
    zoo.data <- merge(g,zoo.data)[,-1]
  }
  
  return(zoo.data)
}