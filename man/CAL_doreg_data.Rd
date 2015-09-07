\name{CAL_doreg_data}
\alias{CAL_doreg_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data for calibration SMC sensor vs. soil sample
}
\description{
Diminishes raw dataframe for SMC calibration sensor vs. soil sample.
}
\usage{
CAL_doreg(data, date_obs=NA, landuse=NA, depth=NA, station=NA, project=NA, 
          sensorType = NA, sensorName=NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  data frame used for calibration, get is via \emph{data()}
}
  \item{date_obs}{
  year of observations to include in calibration, e.g. "2015"
}
  \item{landuse}{
  landuse type to include in caliration, e.g. "appleorchards"
}
  \item{depth}{
 depth of observations to include in caliration, e.g. "20" [cm]
}
  \item{station}{
 station to include in caliration, e.g. "B2"
}
  \item{project}{
 project to include in caliration, e.g. "monalisa"
}
  \item{sensorType}{
 sensor type to include in caliration, e.g. "CS655"
}
  \item{sensorName}{
 sensor name to include in caliration, e.g. "Lsp"
}

}
  
\value{

Diminished dataframe according to argument choices.

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
Setting specific arguments to NA do not exclude any data from the input data frame.
Raw data is retrieved trough data("SensorVSample") (see example). 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\code{
data("SensorVSample")
out <- CAL_doreg_data(data = data, project = "monalisa", obs_date = "2015", 
                      landuse = "appleorchards")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
