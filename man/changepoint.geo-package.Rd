\name{changepoint.geo-package}
\alias{,changepoint.geo-package,changepoint.geo}
\docType{package}
\title{
	Geometrically Inspired Multivariate Changepoint Detection
}
\description{Implements the high-dimensional changepoint detection method GeomCP \insertCite{Grundy2020}{changepoint.geo} and the related mappings used for changepoint detection. These methods view the changepoint problem from a geometrical viewpoint and aim to extract relevant geometrical features in order to detect changepoints. The geomcp() function should be your first point of call.}
\details{
	\packageDESCRIPTION{changepoint.geo}
}
\author{
\packageAuthor{changepoint.geo}

Maintainer: \packageMaintainer{changepoint.geo}
}
\references{
	\insertRef{Grundy2020}{changepoint.geo}

	\insertRef{Killick2012}{changepoint.geo}
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
