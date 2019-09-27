\name{test.stat<-}
\alias{test.stat<-}
\title{
	Generic Function - test.stat<-
}
\description{
	Generic function
}
\usage{
test.stat(object) <- value
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
	Character string of type of test statistic used.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{test.stat<--methods}}
}
\examples{
x <- new('cpt.geo')
test.stat(x) <- 'Normal'
}
\keyword{methods}
\keyword{internal}
