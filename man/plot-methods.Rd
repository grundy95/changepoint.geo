\name{plot-methods}
\docType{methods}
\alias{plot-methods,plot}
\title{ ~~ Methods for Function \code{plot}   ~~}
\description{
 ~~ Methods for function \code{plot}  ~~
}
\section{Methods}{
\describe{
\item{\code{signature(x='ANY')}}{
	Generic plot function, see graphics package using ?plot.
}
\item{\code{signature(x = "cpt.geo")}}{
	Plots the angle and distance mappings with changepoints as vertical lines. There are 3 different plotting options controlled using \code{plot.type}. Default is "mappings" which plots the distance and angle mappings; "full.data" produces a heatmap of all the series; "series" plots specific series, use the argument \code{show.series} to give a vector containing the series to show and the logical argument \code{add.mappings} controls whether the mappings will also be shown. \code{changepoints} is a logical argument and controls whether the changepoints will be plotted or not. \code{xlab} and \code{ylab} control the axis labels. 
}
}}
\keyword{methods}
\keyword{plot}
\keyword{internal}
