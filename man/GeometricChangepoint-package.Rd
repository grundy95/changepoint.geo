\name{GeometricChangepoint-package}
\alias{,GeometricChangepoint-package,GeometricChangepoint}
\docType{package}
\title{
	Geometrically Inspired Multivariate Changepoint Detection
}
\description{Implements the high-dimensional changepoint detection method GeomCP and the related mappings used for changepoint detection. These methods view the changepoint problem from a geometrical viewpoint and aim to extract relevant geometrical features in order to detect changepoints. The geomcp(), function should be your first point of call.
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{GeometricChangepoint}
\packageIndices{GeometricChangepoint}
}
\author{
\packageAuthor{GeometricChangepoint}

Maintainer: \packageMaintainer{GeometricChangepoint}
}
\references{
GeomCP Algorithm: Grundy T, Killick R, Mihaylov G (2020) High-Dimensional Changepoint Detection via a Geometrically Inspired Mapping, \emph{arXiv e-prints arXiv:2001.05241}
}
\keyword{changepoint}
\keyword{geometry}
\seealso{
	\code{\link{geomcp}}
}
\examples{
X <- rbind(matrix(rnorm(100*50),ncol=50),matrix(rnorm(100*50,0,2),ncol=50))
ans <- geomcp(X)
summary(ans)
}
