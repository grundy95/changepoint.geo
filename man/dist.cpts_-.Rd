\name{dist.cpts<-}
\alias{dist.cpts<-}
\alias{dist.cpts<--methods}
\title{
	Generic Function - dist.cpts<-
}
\description{
	Generic function
}
\usage{
dist.cpts(object) <- value
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
	Numeric vector of changepoints in distances.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{dist.cpts<--methods}}
}
\examples{
x <- new('cpt.geo')
dist.cpts(x) <- c(50,100)
}
\keyword{methods}
\keyword{internal}
