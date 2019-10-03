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
Normal: Chen, J. and Gupta, A. K. (2000) \emph{Parametric statistical change point analysis}, Birkhauser

PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.

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
