\name{mapped.data<-}
\alias{mapped.data<-}
\title{
	Generic Function - mapped.data<-
}
\description{
	Generic function
}
\usage{
mapped.data(object) <- value
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
	n by 2 Matrix containing both distances and angles
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{mapped.data<--methods}}
}
\examples{
x <- new('cpt.geo')
mapped.data(x) <- matrix(rnorm(100*2),ncol=2,nrow=100)
}
\keyword{methods}
\keyword{internal}
