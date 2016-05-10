\name{SensorVSample}
\alias{SensorVSample}
\docType{data}
\title{
Calibration data sensor vs. soil samples
}
\description{
data.frame on calibration data sensor vs. soil sample for the LTsER site Matsch/Mazia (South Tyrol, Italy) and multiple sites all over South Tyrol (Projects: MONALISA, HiResAlp).
}
\usage{data("SensorVSample")}
\format{
  A data frame with 598 observations on the following 10 variables.
  \describe{
    \item{\code{project}}{a factor, project name}
    \item{\code{station}}{a factor, micrometeo station name}
    \item{\code{landuse}}{a factor, land use specification}
    \item{\code{date_obs}}{a factor, observation date}
    \item{\code{depth}}{a integer, observation soil depth}
    \item{\code{meanstation}}{a numeric, volumetric soil moisture content (SMC) of sensor / mean of station sensors}
    \item{\code{samples}}{a numeric, volumetric SMC of soil samples}
    \item{\code{date_obs}}{a numeric, mean volumetric SMC of soil samples}
    \item{\code{sensorName}}{a factor, name of sensor used to retrieve meanstation, \emph{SensorMean} refers to a mean value of sensors installed in the station}
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
data("SensorVSample")
str(data)
summary(data)
}
\keyword{datasets}
