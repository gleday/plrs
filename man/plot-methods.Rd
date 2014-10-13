\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ANY}
\alias{plot.plrs}
\alias{plot,plrs,ANY-method}
\alias{plot,plrs.select,ANY-method}

\title{Plot functions in package 'plrs'}

\description{
Methods \code{plot} in package 'plrs'
}

\usage{
\method{plot}{plrs}(x, col.line = "black", col.pts = c("red", "blue","green2", "green4"),
col.cb = "yellow", xlim = c(floor(min(x@data$cghseg)),ceiling(max(x@data$cghseg))),
ylim = c(floor(min(x@data$expr)),ceiling(max(x@data$expr))),
pch = 16, lwd=4, cex = 1.2, xlab="", ylab="", main = "",                      
add = FALSE, lty = 1, lin = FALSE, ...)

}

\arguments{
  \item{x}{An object of class \code{\link{plrs-class}} or \code{\link{plrs.select-class}}}
  \item{col.line}{Color of the fitted line}
  \item{col.pts}{Vector of length 4, for colors associated with each state}
  \item{col.cb}{Color for the confidence band}
  \item{xlim}{The x limits of the plot}
  \item{ylim}{The y limits of the plot}
  \item{pch}{See \code{\link[graphics]{par}}}
  \item{lwd}{See \code{\link[graphics]{par}}}
  \item{cex}{See \code{\link[graphics]{par}}}
  \item{xlab}{Title of the x-axis}
  \item{ylab}{Title of the y-axis}
  \item{main}{Main title for the plot}
  \item{add}{If the plot should be added to the current device. Default is \code{FALSE}}
  \item{lty}{See \code{\link[graphics]{par}}}
  \item{lin}{Logical. Whether the simple linear model should also be plotted}
  \item{...}{Other arguments, see \code{\link[graphics]{par}}}
}

\section{Methods}{
	\describe{
		\item{\code{signature(x = "plrs")}}{Plot observed points and the fitted line}
				\item{\code{signature(x = "plrs.select")}}{Plot observed points and the fitted line of the selected model.}
	}
}

\details{
\code{plot.plrs} plots the observed points, the fitted line and potentially the confidence band.
}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}

