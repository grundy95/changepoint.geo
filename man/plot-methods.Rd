\name{plot-methods}
\docType{methods}
\alias{plot-methods,plot,ANY-method}
%\alias{plot,cpt.geo-method}
\title{ ~~ Methods for Function \code{plot} in Package 'graphics'  ~~}
\description{
 ~~ Methods for function \code{plot} in package 'graphics'  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{
	Generic plot function, see graphics package description using ?plot.
}

\item{\code{signature(x = "cpt.geo")}}{
	Plots the mapped data (distances and angles) and with changepoints in each identified by vertical lines. Optional arguments to control the lines : \code{cpt.col} equivalent to \code{col} to change the color of the changepoint line; \code{cpt.width} equivalent to \code{lwd} to change the width of the changepoint line; \code{cpt.style} equivalent to \code{lty} to change the style of the line. 
}
}}
\keyword{methods}
\keyword{plot}
\keyword{internal}
