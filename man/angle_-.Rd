\name{angle<-}
\alias{angle<-}
\alias{angle<--methods}
\title{
	Generic Function - angle<-
}
\description{
	Generic function
}
\usage{
angle(object) <- value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	  Object of class cpt.geo.
}
  \item{value}{
	  Replacement value.
}
}
\details{
	Generic function.
}
\value{
	Numeric vector of changepoints in distances.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{angle<--methods}}
}
\examples{
x <- new('cpt.geo')
angle(x) <- rnorm(100)
}
\keyword{methods}
\keyword{internal}

