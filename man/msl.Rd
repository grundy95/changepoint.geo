\name{msl}
\alias{msl}
\alias{msl-methods}
\title{
	Generic Function - msl
}
\description{
	Generic Function
}
\usage{
msl(object)
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
	Numeric value of minimum segment length.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{msl-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
msl(x)	#retrieves the msl slot from x.
}
\keyword{methods}
\keyword{internal}
