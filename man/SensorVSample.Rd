\name{SensorVSample}
\alias{SensorVSample}
\docType{data}
\title{
Calibration data for soil moisture sensors.
}
\description{
data.frame for soil moisture sensor calibration for the LTsER site Matsch/Mazia (South Tyrol, Italy) and multiple sites all over South Tyrol 
(Projects: MONALISA, HiResAlp).
}
\usage{data("SensorVSample")}
\format{
  A data frame with 620 observations on the following 14 variables.
  \describe{
    \item{\code{project}}{a factor, project name}
    \item{\code{station}}{a factor,name of micro-meteorological station equiped with soil moisture sensors}
    \item{\code{landuse}}{a factor, land use specification at the station}
    \item{\code{date_obs}}{a factor, observation date}
    \item{\code{depth}}{a integer, observation soil depth}
    \item{\code{meanstation}}{a numeric, volumetric soil water content (VWC) of sensor / mean of station sensors at specific soil depth}
    \item{\code{samples}}{a numeric, VWC of soil samples}
    \item{\code{date_obs}}{a numeric, mean VWC of soil samples, by default three samples are generated for each soil depth during a field campaign.}
    \item{\code{sensorType}}{a factor, type of sensor used to measure VWC \code{meanstation}}
    \item{\code{sensorName}}{a factor, name of sensor used to measure VWC \code{meanstation}, \code{SensorMean} refers to an averaged VWC value computed from sensors installed at a specific soil depth at the station}
    \item{\code{soilType}}{a numeric, soil type at the station}
    \item{\code{lat}}{a numeric, geographic coordinates at the station, latitude}
    \item{\code{lon}}{a numeric, geographic coordinates at the station, longiude}
    \item{\code{alt}}{a numeric, altitude in m a.s.l. at the station}
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
