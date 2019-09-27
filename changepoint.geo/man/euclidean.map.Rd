\name{euclidean.map}
\alias{euclidean.map}
\title{
	Euclidean Mapping
}
\description{
	Centralizes and maps the data to its euclidean distance and angle from a pre-specified vector.
}
\usage{
euclidean.map(X)
}
\arguments{
  \item{X}{
	  n by p matrix containing the data to be analyzed.
}
}
\details{
	The data is shifted to the top right quadrant of the space and then the euclidean distance and angle between each time vector and the vector of ones is calculated. These distances and angles are then returned in the form of a n by 2 matrix.
}
\value{
	An n by 2 matrix containing the distance and angles.
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{cpt.euclid}}
}
\examples{
X <- rbind(matrix(rnorm(100*50),ncol=50),matrix(rnorm(100*50,1),ncol=50))
ans <- euclidean.map(X)
ans
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
\keyword{internal}% __ONLY ONE__ keyword per line
