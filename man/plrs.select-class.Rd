\name{plrs.select-class}
\Rdversion{1.1}
\docType{class}
\alias{plrs.select-class}
\alias{print,plrs.select-method}
\alias{show,plrs.select-method}
\alias{summary,plrs.select-method}

\title{Class \code{plrs.select}}
\description{
An S4 class representing the output of the \code{\link{plrs.select}} function.
}

\section{Slots}{
  \describe{
    \item{\code{table}:}{Object of class \code{matrix} containing the criterion value for all models}
    \item{\code{model}:}{Object of class \code{plrs} containing the selected model}
    \item{\code{crit}:}{Object of class \code{character} containing the criterion used for model selection}
  }
}

\section{Methods}{
  \describe{
    \item{plot}{See \code{\link{plot.plrs}}}
    \item{print}{Print the object information}
    \item{show}{Print the object information}
    \item{summary}{Print a summary of the object information}
  }
}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}


