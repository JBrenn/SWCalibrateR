\name{dB_readStationData}
\alias{dB_readStationData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get data from EURAC micro-meteo station (Mazia/Matsch)
}
\description{
Retrieve data from EURAC micro-meteo station located in Mazia/Matsch
}
\usage{
dB_readStationData(path, header.file, station)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
  path to meteo files
  }
  \item{header.file}{
  header file, absolute path and file name
  }
  \item{station}{
  character, full name of micro-meteo station, e.g. "B2"
  }
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
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
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
Meteo files are manually preprocessed; storage location: \file{//ABZ02FST.EURAC.EDU/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
