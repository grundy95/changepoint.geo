\name{data.set<-}
\alias{data.set<-}
\title{
	Generic Function - data.set<-
}
\description{
	Generic Function
}
\usage{
data.set(object) <- value
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
	n by p Matrix of data set.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{data.set<--methods}}
}
\examples{
x <- new('cpt.geo')
data.set(x) <- matrix(rnorm(100*10),ncol=10,nrow=100)
}
\keyword{methods}
\keyword{internal}
