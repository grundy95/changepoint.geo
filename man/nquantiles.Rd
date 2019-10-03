\name{nquantiles}
\alias{nquantiles}
\alias{nquantiles-methods}
\title{
	Generic Function - nquantiles
}
\description{
	Generic Function
}
\usage{
nquantiles(object)
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
	Numeric value of number of quantiles used in empirical likelihood.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{nquantiles-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
nquantiles(x) <- 10
nquantiles(x)	#retrieves the nquantiles slot from x.
}
\keyword{methods}
\keyword{internal}
