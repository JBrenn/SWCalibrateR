\name{CAL_updatedb}
\alias{CAL_updatedb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
update SQLite database
}
\description{
Writing / Updating SQLite database which is used for downloading data with the SWC calibration tool (shiny app). 
}
\usage{
CAL_updatedb(stations, path2data, inCloud)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stations}{
character vector, station to update, e.g. c("B1", "B2", "B3")
}
  \item{path2data}{
character, path to SWC data, default: path from local linux machine to data on AlpEnv server.
}
  \item{inCloud}{
character, path where database is stored, if NULL database will be stored in package directory of \emph{SMCcalibration} where it is needed for plugging in the calibration tool, default: ownCloud directory data, download link.
}
}
\details{}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (stations, path2data = "/run/user/1000/gvfs/smb-share:server=abz02fst.eurac.edu,share=alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/", 
    inCloud = "/home/jbr/ownCloud/data/") 
{
    if (is.null(inCloud)) {
        pkg_path <- path.package("SMCcalibration")
        db = dbConnect(SQLite(), dbname = file.path(pkg_path, 
            "data", "swc.sqlite"))
    }
    else {
        db = dbConnect(SQLite(), dbname = file.path(inCloud, 
            "swc.sqlite"))
    }
    for (i in stations) {
        stationchr <- substr(i, 1, nchar(i) - 1)
        if (stationchr == "XS") 
            stationchr <- "S"
        stationnr <- as.integer(substr(i, nchar(i), nchar(i)))
        print(paste("updating SWC data of station", i, sep = " "))
        path2files = file.path(path2data, stationchr, i)
        header.file = file.path(path2data, stationchr, paste("header_", 
            i, ".txt", sep = ""))
        data <- dB_getSWC(path2files, header.file, station = stationchr, 
            station_nr = stationnr, calibrate = F, minVALUE = 0, 
            maxVALUE = 1, aggregation = "n")
        if (any(names(data) == "core5")) 
            names(data)[which(names(data) == "core5")] <- "SWC_A_z5"
        if (any(names(data) == "core20")) 
            names(data)[which(names(data) == "core20")] <- "SWC_A_z20"
        df <- data.frame(datetime = index(data), coredata(data))
        dbWriteTable(conn = db, name = i, value = df, row.names = NA, 
            overwrite = TRUE, append = FALSE, field.types = NULL)
    }
    print("Tables in data base:")
    print(dbListTables(db))
    dbDisconnect(db)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
