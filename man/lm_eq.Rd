\name{lm_eq}
\alias{lm_eq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear Model Equation and R-square
}
\description{
Computes and returns (as.character) linear model equation and R-square value}
\usage{
lm_eq(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ input dataframe including columns "meansample" (y) and "meanstation" (x)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
character of linear model equation and R-square value
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\code{
data("SensorVSample")
lm_eq(df = data)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
