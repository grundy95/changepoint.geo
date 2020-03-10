\name{class_input}
\alias{class_input}
\title{
	Input all required arguments into cpt.geo class - Only intended for developer use.
}
\description{
	This function helps to input all necessary information into the correct format for the \code{cpt.geo} class.

	This function is called when \code{class=TRUE}. This is not intended for use by regular users of the package. It is exported for developers to call directly for speed and convenience.

	WARNING: No checks on arguments are performed!
}
\usage{
class_input(data.set, distance, angle, penalty, pen.value, test.stat,
	    msl, nquantiles, dist.cpts, ang.cpts)
}
\arguments{
  \item{data.set}{
	  Data set used in multivariate changepoint analysis, see \code{\link{geomcp}} for further details.
}
  \item{distance}{
	  Vector containing the distance mappings
}
  \item{angle}{
	  Vector containing the angle mappings
}
  \item{penalty}{
	  Penalty used as a text string, see \code{\link{geomcp}} for further details.
}
  \item{pen.value}{
	  Numerical penalty value used in the univariate analysis of the mapped time series.
}
  \item{test.stat}{
	  The assumed test statistic/distribution of the univariate mapped time series as a text string. See \code{\link{geomcp}} for further details.
}
  \item{msl}{
	  Minimum segment length used in the analysis (positive integer).
}
  \item{nquantiles}{
	  Number of quantiles used in empirical distribution if using \code{test.stat='Empirical'}. 
}
  \item{dist.cpts}{
	  Changepoint locations in distance mappings.
}
  \item{ang.cpts}{
	  Changepoint locations in angle mappings.
}
}
\details{
	This function takes all the input required for the \code{cpt.geo} class and enters it into an object.

	This function is exported for developer use only. It does not perform any checks on inputs and is simply a convenience function for converting the output of the worker functions into a nice format for the \code{cpt.geo} class.
}
\value{
	An object of class \code{cpt.geo} filled with the given attributes.
}
\author{
	Thomas Grundy
}


\seealso{
	\code{\link{geomcp}},\code{\link{plot-methods}},\code{\linkS4class{cpt.geo}}
}
\examples{
#This function should only be used by developers, see its use in geomcp.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
\keyword{models}
\keyword{ts}
