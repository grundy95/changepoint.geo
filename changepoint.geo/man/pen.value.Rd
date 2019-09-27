\name{pen.value}
\alias{pen.value}
\title{
	Generic Function - pen.value
}
\description{
	Generic Function
}
\usage{
pen.value(object)
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
	Numeric value of penalty used.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{pen.value-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
pen.value(x)	#retrieves the pen.value slot from x.
}
\keyword{methods}
\keyword{internal}
