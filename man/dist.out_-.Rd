\name{dist.out<-}
\alias{dist.out<-}
\alias{dist.out<--methods}
\title{
	Generic Function - dist.out<-
}
\description{
	Generic function
}
\usage{
dist.out(object) <- value
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
	\code{\link{dist.out<--methods}}
}
\examples{
x <- new('cpt.geo')
dist.out(x) <- new('cpt')
}
\keyword{methods}
\keyword{internal}

