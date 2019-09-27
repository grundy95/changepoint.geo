\name{dist.ts}
\alias{dist.ts}
\title{
	Generic Function - dist.ts
}
\description{
	Generic Function
}
\usage{
dist.ts(object)
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
	ts object of distances.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{dist.ts-methods}}
}
\examples{
x <- new('cpt.geo',mapped.data=matrix(rnorm(100*2),ncol=2))#new cpt.geo object
dist.ts(x)	#retrieves the dist.ts slot from x.
}
\keyword{methods}
\keyword{internal}
