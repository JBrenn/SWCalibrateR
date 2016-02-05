\name{SensorVSample}
\alias{SensorVSample}
\docType{data}
\title{
SWC calibration data set
}
\description{
Data set for calibrationg SWC station data. A large number of station covering South Tyrolean terrain are included, a hotspot is the LTER site Matsch/Mazia valley.
}
\usage{data("SensorVSample")}
\format{
  A data frame with 598 observations on the following 10 variables.
  \describe{
    \item{\code{project}}{project name/id in which the data was sampled} 
    \item{\code{station}}{station name/id}
    \item{\code{landuse}}{landuse around station}
    \item{\code{date_obs}}{sampling date}
    \item{\code{depth}}{sampling depth} 
    \item{\code{sensorType}}{sensor type}
    \item{\code{meanstation}}{SWC data of station, mean of data plus/minus two hours from sampling date/time}
    \item{\code{samples}}{SWC data of soil samples, normally three replicates are sampled}
    \item{\code{samples}}{SWC data of soil samples, mean of the three replicates}
    \item{\code{sensorName}}{sensor name/id which is providing SWC data of station, either a single sensor (if variability  of sensors is high) or the mean of multiple sensors (SensorMean)}
    
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(SensorVSample)
str(SensorVSample)
}
\keyword{datasets}
