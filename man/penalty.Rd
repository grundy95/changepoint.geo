\name{penalty}
\alias{penalty}
\alias{penalty-methods}
\title{
	Generic Function - penalty
}
\description{
	Generic Function
}
\usage{
penalty(object)
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
	Character string of type of penalty used.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{penalty-methods}}
}
\examples{
x <- new('cpt.geo')	#new cpt.geo object
penalty(x)	#retrieves the penalty slot from x.
}
\keyword{methods}
\keyword{internal}
