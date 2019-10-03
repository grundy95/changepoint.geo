\name{distance}
\alias{distance}
\alias{distance-methods}
\title{
	Generic Function - distance
}
\description{
	Generic Function
}
\usage{
distance(object)
}
\arguments{
  \item{object}{
	  Object of class cpt.geo.
}
}
\details{
	Generic function
}
\value{
	Numeric vector of changepoints in distances.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{distance-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
distance(x)	#retrieves the distance slot from x.
}
\keyword{methods}
\keyword{internal}

