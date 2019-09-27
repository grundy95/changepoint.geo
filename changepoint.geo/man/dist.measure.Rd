\name{dist.measure}
\alias{dist.measure}
\title{
	Generic Function - dist.measure
}
\description{
	Generic Function
}
\usage{
dist.measure(object)
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
	Character string of distance measure.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{dist.measure-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
dist.measure(x)	#retrieves the dist.measure slot from x.
}
\keyword{methods}
\keyword{internal}
