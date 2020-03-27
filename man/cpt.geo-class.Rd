\name{cpt.geo-class}
\alias{cpt.geo-class}
\Rdversion{1.1}
\docType{class}
\alias{ang.cpts,cpt.geo-method}
\alias{ang.cpts<-,cpt.geo-method}
\alias{data.set,cpt.geo-method}
\alias{data.set<-,cpt.geo-method}
\alias{dist.cpts,cpt.geo-method}
\alias{dist.cpts<-,cpt.geo-method}
\alias{dist.cpts-methods,cpt.geo-method}
\alias{distance,cpt.geo-method}
\alias{distance<-,cpt.geo-method}
\alias{angle,cpt.geo-method}
\alias{angle<-,cpt.geo-method}
\alias{msl,cpt.geo-method}
\alias{msl<-,cpt.geo-method}
\alias{nquantiles,cpt.geo-method}
\alias{nquantiles<-,cpt.geo-method}
\alias{pen.value,cpt.geo-method}
\alias{pen.value<-,cpt.geo-method}
\alias{penalty,cpt.geo-method}
\alias{penalty<-,cpt.geo-method}
\alias{plot,cpt.geo-method}
\alias{show,cpt.geo-method}
\alias{summary,cpt.geo-method}
\alias{test.stat,cpt.geo-method}
\alias{test.stat<-,cpt.geo-method}

\title{Class \code{"cpt.geo"}}
\description{
	A class for cpt.geo objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cpt.geo", ...)}.
	\describe{
		\item{\code{new('cpt.geo',...)}:}{Creates a new object with class cpt.geo}
	}
}
\section{Slots}{
  \describe{
    \item{\code{data.set}:}{Object of class \code{"matrix"}, original data set to be analyzed for changepoints }
    \item{\code{distance}:}{Object of class \code{"numeric"}, distance mapping of each time vector.}
    \item{\code{angle}:}{Object of class \code{"numeric"}, angle mapping of each time vector.}
    \item{\code{penalty}:}{Object of class \code{"character"}, type of penalty used as a text string.}
    \item{\code{pen.value}:}{Object of class \code{"numeric"}, numeric value of penalty used.}
    \item{\code{test.stat}:}{Object of class \code{"character"}, test statistic used for the analysis.}
    \item{\code{msl}:}{Object of class \code{"numeric"}, minimum segment length (no. of observations between changepoints).}
    \item{\code{nquantiles}:}{Object of class \code{"numeric"}, number of quantiles used in the empirical distribution.}
    \item{\code{dist.cpts}:}{Object of class \code{"numeric"}, changepoint locations in the distances.}
    \item{\code{ang.cpts}:}{Object of class \code{"numeric"}, changepoint locations in the angles.}
    \item{\code{date}:}{Object of class \code{"character"}, date the analysis was run.}
    \item{\code{version}:}{Object of class \code{"character"}, version number of the package when analysis was run.}
  }
}
\section{Methods}{
  \describe{
    \item{ang.cpts}{\code{signature(object = "cpt.geo")}: retrieves ang.cpts slot.}
    \item{ang.cpts<-}{\code{signature(object = "cpt.geo")}: replaces ang.cpts slot. }
    \item{angle}{\code{signature(object = "cpt.geo")}: retrieves angle slot.}
    \item{angle<-}{\code{signature(object = "cpt.geo")}: replaces angle slot.}
    \item{data.set}{\code{signature(object = "cpt.geo")}: retrieves data.set slot. }
    \item{data.set<-}{\code{signature(object = "cpt.geo")}: replaces data.set slot. }
    \item{dist.cpts}{\code{signature(object = "cpt.geo")}: retrieves dist.cpts slot.}
    \item{dist.cpts<-}{\code{signature(object = "cpt.geo")}: replaces dist.cpts slot.}
    \item{distance}{\code{signature(object = "cpt.geo")}: retrieves distance slot.}
    \item{distance<-}{\code{signature(object = "cpt.geo")}: replaces distance slot. }
    \item{msl}{\code{signature(object = "cpt.geo")}: retrieves msl slot. }
    \item{msl<-}{\code{signature(object = "cpt.geo")}: replaces msl slot. }
    \item{nquantiles}{\code{signature(object = "cpt.geo")}: retrieves nquantiles slot. }
    \item{nquantiles<-}{\code{signature(object = "cpt.geo")}: replaces nquantiles slot.}
    \item{pen.value}{\code{signature(object = "cpt.geo")}: retrieves pen.value slot. }
    \item{pen.value<-}{\code{signature(object = "cpt.geo")}: replaces pen.value slot. }
    \item{penalty}{\code{signature(object = "cpt.geo")}: retrieves penalty slot.}
    \item{penalty<-}{\code{signature(object = "cpt.geo")}: replaces penalty slot.}
    \item{plot}{\code{signature(object = "cpt.geo")}: returns an object from ggplot of the distance and angle mappings.}
    \item{show}{\code{signature(object = "cpt.geo")}: prints details of the cpt.geo object including a summary.}
    \item{summary}{\code{signature(object = "cpt.geo")}: prints a summary of the cpt.geo object.}
    \item{test.stat}{\code{signature(object = "cpt.geo")}: retrieves test.stat slot.}
    \item{test.stat<-}{\code{signature(object = "cpt.geo")}: replaces test.stat slot.}
	 }
}
\author{
	Thomas Grundy
}

\seealso{
	\code{\link{geomcp}},\code{\link[changepoint.geo]{plot-methods}},\code{\link[=ggplot2]{ggplot}}
}
\examples{
showClass("cpt.geo")	#shows the structure of the cpt.geo class.

x=new('cpt.geo')	#creates a new object with the cpt.geo class defaults
dist.cpts(x)		#retrieves the dist.cpts slot from x
dist.cpts(x) <- c(50,100) #replaces the dist.cpts slot from x with c(50,100)

}
\keyword{classes}
\keyword{internal}
