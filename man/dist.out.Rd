\name{dist.out}
\alias{dist.out}
\alias{dist.out-methods,dist.out}
\title{
	Generic Function - dist.out
}
\description{
	Generic Function
}
\usage{
dist.out(object)
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
	\code{\link{dist.out-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
dist.out(x)	#retrieves the dist.out slot from x.
}
\keyword{methods}
\keyword{internal}

