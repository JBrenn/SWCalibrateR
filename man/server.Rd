\name{server}
\alias{server}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shiny app interactive calibration}
\description{
Server for interactive calibration SMC sensor vs. Soil Sample}
\usage{
server(input, output)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
input to server
}
  \item{output}{
output from server
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
makes up shiny app with ui (see example)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\code{
# load data set 
  data("SensorVSample")

# run shiny app
  shinyApp(ui, server)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
