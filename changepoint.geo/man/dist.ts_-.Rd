\name{dist.ts<-}
\alias{dist.ts<-}
\title{
	Generic Function - dist.ts<-
}
\description{
	Generic function
}
\usage{
dist.ts(object) <- value
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
	ts object of distances.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{ang.cpts<--methods}}
}
\examples{
x <- new('cpt.geo')
dist.cpts(x) <- as.ts(rnorm(100))
}
\keyword{methods}
\keyword{internal}
