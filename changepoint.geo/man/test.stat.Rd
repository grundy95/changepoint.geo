\name{test.stat}
\alias{test.stat}
\title{
	Generic Function - test.stat
}
\description{
	Generic Function
}
\usage{
test.stat(object)
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
	Character string of test statistic used.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{test.stat-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
test.stat(x)	#retrieves the test.stat slot from x.
}
\keyword{methods}
\keyword{internal}
