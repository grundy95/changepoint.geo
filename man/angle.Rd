\name{angle}
\alias{angle}
\alias{angle-methods}
\title{
	Generic Function - angle
}
\description{
	Generic Function
}
\usage{
angle(object)
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
	\code{\link{angle-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
angle(x)	#retrieves the angle slot from x.
}
\keyword{methods}
\keyword{internal}

