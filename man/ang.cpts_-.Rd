\name{ang.cpts<-}
\alias{ang.cpts<-}
\alias{ang.cpts<--methods}
\title{
	Generic Function - ang.cpts<-
}
\description{
	Generic function
}
\usage{
ang.cpts(object) <- value
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
	Numeric vector of changepoints in angle mapping.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{ang.cpts<--methods}}
}
\examples{
x <- new('cpt.geo')
ang.cpts(x) <- c(50,100)
}
\keyword{methods}
\keyword{internal}
