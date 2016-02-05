
# library("DataBaseAlpEnvEURAC")
# library(zoo)
# library(chron)


CAL_updatedb <- function(stations, 
                         path2data = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/", 
                         inCloud = "/home/jbr/ownCloud/data/")
{
  # connect to db in data folder of project
  if (is.null(inCloud)) {
    pkg_path <- path.package("SMCcalibration") 
    db = dbConnect(SQLite(), dbname=file.path(pkg_path,"data","swc.sqlite"))
  } else {
    db = dbConnect(SQLite(), dbname=file.path(inCloud,"swc.sqlite"))
  }

  for (i in stations)
  {
    stationchr <- substr(i, 1, nchar(i)-1)

    if (stationchr == "XS") stationchr <- "S"
    
    stationnr  <- as.integer(substr(i, nchar(i), nchar(i)))
    
    print(paste("updating SWC data of station", i, sep=" "))
    
    path2files = file.path(path2data,stationchr,i)
    header.file = file.path(path2data,stationchr,paste("header_",i,".txt",sep=""))
    
    data <- dB_getSWC(path2files, header.file, station = stationchr, station_nr = stationnr, calibrate = F, 
                      minVALUE = 0, maxVALUE = 1, aggregation = "n")
    
    if(any(names(data)=="core5")) names(data)[which(names(data)=="core5")] <- "SWC_A_z5"
    if(any(names(data)=="core20")) names(data)[which(names(data)=="core20")] <- "SWC_A_z20"
    
    df <- data.frame(datetime=index(data),coredata(data))
    
    # update litesql
    dbWriteTable(conn=db, name=i,
                 value=df, row.names = NA, overwrite = TRUE, append = FALSE,
                 field.types = NULL)
 
  }
  
  # list tables in db
  print("Tables in data base:")
  print(dbListTables(db))
  
  dbDisconnect(db)
}



# get data column
# datetime <- sqliteQuickColumn(db,"P2","datetime")
# SWC_z5_A <- sqliteQuickColumn(db,"P2","SWC_z5_A")
# 
# SWC_z5_A <- sqliteQuickColumn(db,"B1","SWC_LS_z5")

# does a table exist
#dbExistsTable(conn = db, name = "B1")


