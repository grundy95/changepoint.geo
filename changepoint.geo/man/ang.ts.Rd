\name{ang.ts}
\alias{ang.ts}
\title{
	Generic Function - ang.ts
}
\description{
	Generic Function
}
\usage{
ang.ts(object)
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
	ts object of angles.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{ang.ts-methods}}
}
\examples{
x <- new('cpt.geo',mapped.data=matrix(rnorm(100*2),ncol=2))	#new cpt.geo object
ang.ts(x)	#retrieves the ang.ts slot from x.
}
\keyword{methods}
\keyword{internal}
