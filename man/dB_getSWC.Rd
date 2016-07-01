\name{dB_getSWC}
\alias{dB_getSWC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get regular soil moisture time series from EURAC micro-meteo station (Mazia/Matsch)
%%  ~~function to do ... ~~
}
\description{
Retrieve regular soil moisture time series from EURAC micro-meteo station located in Mazia/Matsch
}
\usage{
dB_getSWC(path2data, station, calibrate=FALSE, aggregation="n", 
          minVALUE=0.05, maxVALUE=0.50, clear_raw_data=FALSE, remove_freezing=FALSE, 
          write.csv=FALSE, path2write="./")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path2data}{
  path to meteo files
  }
   \item{station}{
  character, name of micro-meteo station, e.g. "B0002"
  }
  \item{calibrate}{
  boolean, default FALSE: use of uncalibrated SMC data; TRUE: calibrated SMC sensor data is used for comparision with simulated SMC, calibration parameters are retrieved from the package data via \emph{data(calibration)}
  }
  \item{aggregation}{
  character, time aggregation applied. "n" no aggregation of raw data,"h": hourly, "d": daily
  }
  \item{minVALUE}{
  numeric, minimum value of soil temperature for filtering, default = 5 vol\%
  }
  \item{maxVALUE}{
  numeric, maximum value of soil temperature for filtering, default = 50 vol\%
  }
  \item{clear_raw_data}{
  boolean, TRUE: clearing of raw data, not yet implemented, default = FALSE
  }
  \item{remove_freezing}{
  boolean, TRUE: freezing periods are remove from raw data; only possible for B, P and I stations, default = FALSE
  }
  \item{write.csv}{
  boolean, default = FALSE; TRUE: .csv file is written to path2write, FALSE: no .csv file is written
  }
   \item{path2write}{
  path data should be written to
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\enumerate{
  \item zoo object containing processed data
  \item file output containing processed data in .csv format, if write.csv = TRUE
}
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
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
calibration file is stored here: \file{//ABZ02FST.EURAC.EDU/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia/calibration.csv}
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}

