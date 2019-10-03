\name{msl<-}
\alias{msl<-}
\alias{msl<--methods}
\title{
	Generic Function - msl<-
}
\description{
	Generic function
}
\usage{
msl(object) <- value
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
	Numeric value of minimum segment length.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{msl<--methods}}
}
\examples{
x <- new('cpt.geo')
msl(x) <- 30
}
\keyword{methods}
\keyword{internal}
