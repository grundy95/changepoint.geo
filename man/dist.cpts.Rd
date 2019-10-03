\name{dist.cpts}
\alias{dist.cpts}
\alias{dist.cpts-methods,dist.cpts}
\title{
	Generic Function - dist.cpts
}
\description{
	Generic Function
}
\usage{
dist.cpts(object)
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
	\code{\link{dist.cpts-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
dist.cpts(x)	#retrieves the dist.cpts slot from x.
}
\keyword{methods}
\keyword{internal}
