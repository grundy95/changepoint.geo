\name{changepoint.geo-package}
\alias{changepoint.geo-package}
\alias{changepoint.geo}
\docType{package}
\title{
	Geometrically Inspired Changepoint Detection Methods
}
\description{
	Implements multivariate changepoint detection methods that are geometrically inspired. Users should start by looking at the documentation for cpt.euclid().
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{changepoint.geo}
\packageIndices{changepoint.geo}
}
\author{
\packageAuthor{changepoint.geo}

Maintainer: \packageMaintainer{changepoint.geo}
}
\references{
Normal: Chen, J. and Gupta, A. K. (2000) \emph{Parametric statistical change point analysis}, Birkhauser

PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.

}
\keyword{changepoint}
\keyword{geometry}
\seealso{
	\code{\link{cpt.euclid}}
}
\examples{
X <- rbind(matrix(rnorm(100*50),ncol=50),matrix(rnorm(100*50,0,2),ncol=50))
ans <- cpt.euclid(X)
summary(ans)
}
