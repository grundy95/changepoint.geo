\name{mapped.data}
\alias{mapped.data}
\title{
	Generic Function - mapped.data
}
\description{
	Generic Function
}
\usage{
mapped.data(object)
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
	n by 2 matrix of distance and angles. 
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{mapped.data-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
mapped.data(x)	#retrieves the mapped.data slot from x.
}
\keyword{methods}
\keyword{internal}
