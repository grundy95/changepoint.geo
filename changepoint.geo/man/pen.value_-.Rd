\name{pen.value<-}
\alias{pen.value<-}
\title{
	Generic Function - pen.value<-
}
\description{
	Generic function
}
\usage{
pen.value(object) <- value
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
	Numeric value of the penalty used
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{pen.value<--methods}}
}
\examples{
x <- new('cpt.geo')
pen.value(x) <- 20
}
\keyword{methods}
\keyword{internal}
