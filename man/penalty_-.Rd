\name{penalty<-}
\alias{penalty<-}
\alias{penalty<--methods}
\title{
	Generic Function - penalty<-
}
\description{
	Generic function
}
\usage{
penalty(object) <- value
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
	Character string of type of penalty used.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{penalty<--methods}}
}
\examples{
x <- new('cpt.geo')
penalty(x) <- 'Manual'
}
\keyword{methods}
\keyword{internal}
