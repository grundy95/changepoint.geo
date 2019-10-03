\name{ang.cpts}
\alias{ang.cpts}
\alias{ang.cpts-methods}
\title{
	Generic Function - ang.cpts
}
\description{
	Generic Function
}
\usage{
ang.cpts(object)
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
	\code{\link{ang.cpts-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
ang.cpts(x)	#retrieves the ang.cpts slot from x.
}
\keyword{methods}
\keyword{internal}
