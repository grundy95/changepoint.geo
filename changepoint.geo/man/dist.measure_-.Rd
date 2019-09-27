\name{dist.measure<-}
\alias{dist.measure<-}
\title{
	Generic Function - dist.measure<-
}
\description{
	Generic function
}
\usage{
dist.measure(object) <- value
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
	Character string of distance measure.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{dist.measure<--methods}}
}
\examples{
x <- new('cpt.geo')
dist.measure(x) <- 'Euclidean'
}
\keyword{methods}
\keyword{internal}
