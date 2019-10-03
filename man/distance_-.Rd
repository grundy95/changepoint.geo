\name{distance<-}
\alias{distance<-}
\alias{distance<--methods}
\title{
	Generic Function - distance<-
}
\description{
	Generic function
}
\usage{
distance(object) <- value
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
	\code{\link{distance<--methods}}
}
\examples{
x <- new('cpt.geo')
distance(x) <- rnorm(100)
}
\keyword{methods}
\keyword{internal}
