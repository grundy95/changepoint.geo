\name{ang.out<-}
\alias{ang.out<-}
\alias{ang.out<--methods}
\title{
	Generic Function - ang.out<-
}
\description{
	Generic function
}
\usage{
ang.out(object) <- value
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
	\code{\link{ang.out<--methods}}
}
\examples{
x <- new('cpt.geo')
ang.out(x) <- new('cpt')
}
\keyword{methods}
\keyword{internal}

