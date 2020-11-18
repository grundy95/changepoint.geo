\name{ang.out}
\alias{ang.out}
\alias{ang.out-methods}
\title{
	Generic Function - ang.out
}
\description{
	Generic Function
}
\usage{
ang.out(object)
}
\arguments{
  \item{object}{
	  Object of class cpt.geo.
}
}
\details{
	Generic function
}
\value{
	Numeric vector of changepoints in angle measure.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{ang.out-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
ang.out(x)	#retrieves the ang.out slot from x.
}
\keyword{methods}
\keyword{internal}

