\name{nquantiles<-}
\alias{nquantiles<-}
\alias{nquantiles<--methods}
\title{
	Generic Function - nquantiles<-
}
\description{
	Generic function
}
\usage{
nquantiles(object) <- value
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
	Numeric value of number of quantiles used in empirical likelihood.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{nquantiles<--methods}}
}
\examples{
x <- new('cpt.geo')
nquantiles(x) <- 10
}
\keyword{methods}
\keyword{internal}
