
# library("DataBaseAlpEnvEURAC")
# library(zoo)
# library(chron)


CAL_updatedb <- function(stations, inGIT="/home/jbr/GitHub/SMCcalibration/data/")
{
  # connect to db in data folder of project
  if (is.null(inGIT)) {
    pkg_path <- path.package("SMCcalibration")
    setwd(file.path(pkg_path,"data"))
    db = dbConnect(SQLite(), dbname="swc.sqlite")
  } else {
    db = dbConnect(SQLite(), dbname="swc.sqlite")
  }

  
  # B stations
  # B1
  if (any(grepl("B1",stations)))
  {
    path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/B1/"
    header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/header_B1.txt"
    
    B1 <- dB_getSWC(path2files, header.file, station = "B", station_nr = 1, calibrate = F, minVALUE = 0, maxVALUE = 1, aggregation = "n")
    df_B1 <- data.frame(datetime=time(B1),coredata(B1))
    
    # update litesql
    dbWriteTable(conn=db, name="B1",
                 value=df_B1, row.names = NA, overwrite = TRUE, append = FALSE,
                 field.types = NULL)
  }
  
  if (any(grepl("B2",stations)))
  {
    # B2
    path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/B2/"
    header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/header_B2.txt"
    
    B2 <- dB_getSWC(path2files, header.file, station = "B", station_nr = 2, calibrate = F, minVALUE = 0, maxVALUE = 1, aggregation = "n")
    df_B2 <- data.frame(datetime=time(B2),coredata(B2))
    
    # update litesql
    dbWriteTable(conn=db, name="B2",
                 value=df_B2, row.names = NA, overwrite = TRUE, append = FALSE,
                 field.types = NULL)
  }
  
  if (any(grepl("B3",stations)))
  {
    # B3
    path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/B3/"
    header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/B/header_B3.txt"
    
    B3 <- dB_getSWC(path2files, header.file, station = "B", station_nr = 3, calibrate = F, minVALUE = 0, maxVALUE = 1, aggregation = "n")
    df_B3 <- data.frame(datetime=time(B3),coredata(B3))
    
    # update litesql
    dbWriteTable(conn=db, name="B3",
                 value=df_B3, row.names = NA, overwrite = TRUE, append = FALSE,
                 field.types = NULL)
  }
  
  # I stations
  
  if (any(grepl("I3",stations)))
  {
    # I1
    path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/I/I1/"
    header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/I/header_I1.txt"
    
    I1 <- dB_getSWC(path2files, header.file, station = "I", station_nr = 1, calibrate = F, minVALUE = 0, maxVALUE = 1, aggregation = "n")
    df_I1 <- data.frame(datetime=time(I1),coredata(I1))
    
    # update litesql
    dbWriteTable(conn=db, name="I1",
                 value=df_I1, row.names = NA, overwrite = TRUE, append = FALSE,
                 field.types = NULL)
  }
  
  if (any(grepl("I3",stations)))
  {
    # I3
    path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/I/I3/"
    header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/I/header_I3.txt"
    
    I3 <- dB_getSWC(path2files, header.file, station = "I", station_nr = 3, calibrate = F, minVALUE = 0, maxVALUE = 1, aggregation = "n")
    df_I3 <- data.frame(datetime=time(I3),coredata(I3))
    
    # update litesql
    dbWriteTable(conn=db, name="I3",
                 value=df_I3, row.names = NA, overwrite = FALSE, append = FALSE,
                 field.types = NULL)
    
  }
  
  # P stations
  if (any(grepl("P1",stations)))
  {
    # P1
    path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/P/P1/"
    header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/P/header_P.txt"
    
    P1 <- dB_getSWC(path2files, header.file, station = "P", station_nr = 1, calibrate = F, minVALUE = 0, maxVALUE = 1, aggregation = "n")
    df_P1 <- data.frame(datetime=time(P1),coredata(P1))
    
    # update litesql
    dbWriteTable(conn=db, name="P1",
                 value=df_P1, row.names = NA, overwrite = TRUE, append = FALSE,
                 field.types = NULL)
    
  }
  
  if (any(grepl("P2",stations)))
  {
    # P2
    path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/P/P2/"
    header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/P/header_P2.txt"
    
    P2 <- dB_getSWC(path2files, header.file, station = "P", station_nr = 2, calibrate = F, minVALUE = 0, maxVALUE = 1, aggregation = "n")
    df_P2 <- data.frame(datetime=time(P2),coredata(P2))
    
    # update litesql
    dbWriteTable(conn=db, name="P2",
                 value=df_P2, row.names = NA, overwrite = TRUE, append = FALSE,
                 field.types = NULL)
  }
  
  if (any(grepl("P2",stations)))
  {
    # P3
    path2files = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/P/P3/"
    header.file = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/P/header_P.txt"
    
    P3 <- dB_getSWC(path2files, header.file, station = "P", station_nr = 3, calibrate = F, minVALUE = 0, maxVALUE = 1, aggregation = "n")
    df_P3 <- data.frame(datetime=time(P3),coredata(P3))
    
    dbWriteTable(conn=db, name="P3",
                 value=df_P3, row.names = NA, overwrite = TRUE, append = FALSE,
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


