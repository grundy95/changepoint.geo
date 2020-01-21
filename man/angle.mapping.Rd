\name{angle.mapping}
\alias{angle.mapping}
\title{
	Principle angle mapping
}
\description{
	Calculates the principle angle of each time vector to the reference vector. Note data points are not translated in this function and error checking is not performed!
}
\usage{
angle.mapping(X,ref.vec.value=rep(1,length(X[1,])))
}
\arguments{
	\item{X}{
		A matrix containing the centralized data of size n by p.
	}
	\item{ref.vec.value}{
		A vector containing the reference vector from which the principle angle will be calculated.
	}
}
\details{
	This function calculates the principle angle between each time vector and the reference vector. Note the translation within \code{geomcp} happens outside of the function.
}
\value{
	A vector of length n is returned with the angle mapping for each time point.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{geomcp}}
}
\examples{
X <- rbind(matrix(rnorm(100*50,2),ncol=50),matrix(rnorm(100*50,2,4),ncol=50))
ref.vec.value <- rep(1,50)
ans <- angle.mapping(X,ref.vec.value)
ans
}
\keyword{methods}
\keyword{models}
\keyword{ts}
