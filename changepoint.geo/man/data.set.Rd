\name{data.set}
\alias{data.set}
\title{
	Generic Function - data.set
}
\description{
	Generic Function
}
\usage{
data.set(object)
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
	n by p matrix containing the data set.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{data.set-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
data.set(x)	#retrieves the data.set slot from x.
}
\keyword{methods}
\keyword{internal}
