\name{ang.ts<-}
\alias{ang.ts<-}
\title{
	Generic function - ang.ts<-
}
\description{
	Generic Function
}
\usage{
ang.ts(object) <- value
}
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
	ts object of angle measures.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{ang.ts<--methods}}
}
\examples{
x <- new('cpt.geo')
ang.ts(x) <- as.ts(rnorm(100))
}
\keyword{methods}
\keyword{internal}
