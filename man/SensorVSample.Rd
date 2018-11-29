\name{data}
\alias{data}
\docType{data}
\title{
Example calibration data for soil moisture sensors.
}
\description{
Example data set for soil moisture sensor calibration. Generally loads if shiny app is started. 
}
\usage{data("SensorVSample")}
\format{
  A data frame with 100 observations on the following 13 variables.
  \describe{
    \item{\code{Project ID}}{a factor, project name}
    \item{\code{Station ID}}{a factor,name of micro-meteorological station equiped with soil moisture sensors.}
    \item{\code{Landuse}}{a factor, land use specification at the station.}
    \item{\code{Date}}{a factor, observation date.}
    \item{\code{Soil depth}}{a integer, observation soil depth in [cm].}
    \item{\code{Sensor SWC}}{a numeric, volumetric soil water content (VWC) of sensor / mean of station sensors at specific soil depth, range [0;1].}
    \item{\code{Sample SWC}}{a numeric, mean VWC of soil samples, by default three samples are generated for each soil depth during a field campaign, range [0;1].}
    \item{\code{Sensor type}}{a factor, type of sensor used to measure VWC \code{Sensor SWC}}
    \item{\code{Sensor ID}}{a factor, name of sensor used to measure VWC \code{Sensor SWC}}
    \item{\code{Soil type}}{a numeric, soil type at the station.}
    \item{\code{Latitude}}{a numeric, geographic coordinates at the station, latitude.}
    \item{\code{Longitude}}{a numeric, geographic coordinates at the station, longiude.}
    \item{\code{Altitude}}{a numeric, altitude in m a.s.l. at the station.}
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
