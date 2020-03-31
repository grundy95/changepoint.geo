\name{geomcp}
\alias{geomcp}
\title{
	Multivariate changepoint detection via two geometric mappings
}
\description{
	Implements the GeomCP algorithm \insertCite{Grundy2020}{changepoint.geo}. This algorithm performs two geometric mappings on multivariate time series based on the Euclidean distance and principle angle between each time vector and a pre-specified reference vector. The univariate changepoint detection method PELT, \insertCite{Killick2012}{changepoint.geo}, is then performed on the two mappings to identify changepoints which correspond to those in the original multivariate time series.
}
\usage{
geomcp(X, penalty = "MBIC", pen.value = 0, test.stat = "Normal", msl = 2,
       nquantiles = 1,MAD=FALSE,ref.vec='Default',ref.vec.value=0)
}
\arguments{
  \item{X}{
	  A matrix containing the data which is of size n by p. Each row is a new time point and each column is a different series.
}
  \item{penalty}{
	  Penalty choice for univariate changepoint analysis of mappings. Choice of "MBIC", "SIC", "BIC", "Hannan-Quinn","Manual". If "Manual" is specified, the manual penalty is contained in the pen.value parameter.
}
  \item{pen.value}{
	  The value of the penalty when using the "Manual" penalty option - this can be a numeric value or text giving the formula to use, see \code{\link[changepoint]{cpt.meanvar}} for more details.
}
  \item{test.stat}{
	  The assumed test statistic/distribution of the mapped data. Currently only "Normal" and "Empirical" are supported.
}
  \item{msl}{
	  Positive integer giving the minimum segment length (no. of observations between changes), default is 1.
}
  \item{nquantiles}{
	  Only required for test.stat="Empirical". Number of quantiles used in estimating the empirical likelihood.
}
  \item{MAD}{
	  Logical. If TRUE transforms each series by subtracting the median of the series and dividing by the median absolute deviation.
  }
  \item{ref.vec}{
	  Choice of "Default" or "Manual". If "Default" is specified the vector of ones is used as the reference vector. If "Manual" is selected, the manual reference vector is contained in the ref.vec.value parameter. 
  }
  \item{ref.vec.value}{
	  The vector that is used as the reference vector for calculating distances and angles from.
  }
}
\details{
	This function centralizes all time vectors using the given reference vector and then performs the distance and angle mappings from the reference vector. The univariate changepoint method PELT is then used to detect changepoints in the distance and angle mappings which correspond to changes in the multivariate time series.
}
\value{
	An object of S4 class "cpt.geo" is returned. The slots \code{dist.cpts} and \code{ang.cpts} return the changepoints identified in each measure. 
}
\references{
	\insertRef{Grundy2020}{changepoint.geo}

	\insertRef{Killick2012}{changepoint.geo}
}
\author{
	Thomas Grundy
}
\seealso{
	\code{\linkS4class{cpt.geo}},\code{\link[changepoint.geo]{plot-methods}}
}
\examples{
##Variance change in all series
set.seed(1)
X <- rbind(matrix(rnorm(100*50),ncol=50),matrix(rnorm(100*50,0,2),ncol=50))
ans <- geomcp(X)
summary(ans)
plot(ans)

##Mean change in 50% of series with a manual reference vector and non-parametric univariate 
##changepoint detection using 10 quantiles with a BIC penalty and min seg length of 5
set.seed(1)
Y <- rbind(matrix(rnorm(100*20),ncol=20),cbind(matrix(rnorm(100*10),ncol=10),
					       matrix(rnorm(100*10,1),ncol=10)))
res <- geomcp(Y,penalty='Manual',pen.value=30,test.stat='Empirical',nquantiles=10,ref.vec='Manual',
	      ref.vec.value=seq(1,10,length.out=20),msl=5)
summary(res)
plot(res)

##Different plot types for above example
#Plots mappings and changepoints
plot(res,plot.type='mappings') 
#Heatmap of data with changepoints not shown
plot(res,plot.type='full.data',changepoints=FALSE,scale.series=TRUE)
#Specific series with mappings and changepoints shown.
plot(res,plot.type='series',show.series=c(1,5,10),add.mappings=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}% use one of  RShowDoc("KEYWORDS")
\keyword{models}% __ONLY ONE__ keyword per line
\keyword{ts}
