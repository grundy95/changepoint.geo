\name{cpt.euclid}
\alias{cpt.euclid}
\title{
	Identifying Changepoints using the Euclidean Distance
}
\description{
	Maps the multivariate time series to each time vectors distance and angle from a pre-specified vector and identifies changepoints in the distances and angles.
}
\usage{
cpt.euclid(X, penalty = "MBIC", pen.value = 0, test.stat = "Normal", msl = 2, nquantiles = 0, class = TRUE)
}
\arguments{
  \item{X}{
	  A matrix containing the data within which you wish to find a changepoint. Each row is a new time point and each column is a different variable.
}
  \item{penalty}{
	  Choice of "None", "SIC", "BIC", "MBIC", "AIC","Hannan-Quinn","Asymptotic","Manual". If "Manual" is specified, the manual penalty is contained in the pen.value parameter. If "Asymptotic" is specified, the theoretical type 1 error is contained in the pen.value parameter. The predefined penalties listed DO count the changepoint as a parameter, postfix a 0 e.g "SIC0" to NOT count the changepoint as a parameter.
}
  \item{pen.value}{
	  The theoretical type 1 error e.g. 0.05 when using the Asymptotic penalty. The value of the penalty when using the "Manual" penalty option - this can be a numeric value or text giving the formula to use.
}
  \item{test.stat}{
	  The assumed test statistic/distribution of the mapped data. Currently only "Normal" and "Empricial" are supported.
}
  \item{msl}{
	  Positive integer giving the minimum segment length (no. of observations between changes), default is 1.
}
  \item{nquantiles}{
	  Only required for test.stat="Empricial". Number of quantiles to be used in estimating the empirical likelihood.
}
  \item{class}{
	  Logical. If TRUE then an object of class \code{cpt.geo} is returned.
}
}
\details{
	This function maps the given multivariate time series to each time vectors distance and angle form a pre-specifed vector. This pre-specified vector is the vector of ones (after the data has been shifted to the top right of the space by subtracting the minimum of each variable minus 1 from each time vector. The PELT method is then used to detect changepoints in these distances and angles which correspond to changes in the multivariate time series.
}
\value{
	i
	If \code{class=TRUE} then an object of S4 class "cpt.geo" is returned. The slots \code{dist.cpts} and \code{ang.cpts} return the changepoints identified in each measure. 
}
\references{
Normal: Chen, J. and Gupta, A. K. (2000) \emph{Parametric statistical change point analysis}, Birkhauser

PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\link{plot-methods}},\code{\linkS4class{cpt.geo}}
}
\examples{
X <- rbind(matrix(rnorm(100*50),ncol=50),matrix(rnorm(100*50,0,2),ncol=50))
ans <- cpt.euclid(X)
summary(ans)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
\keyword{models}% __ONLY ONE__ keyword per line
\keyword{ts}
