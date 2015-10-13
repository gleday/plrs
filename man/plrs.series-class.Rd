\name{plrs.series-class}
\Rdversion{1.1}
\docType{class}
\alias{plrs.series-class}
\alias{print,plrs.series-method}
\alias{show,plrs.series-method}
\alias{summary,plrs.series-method}

\title{Class \code{plrs.series}}
\description{
An S4 class representing the output of the \code{\link{plrs.series}} function.
}

\section{Slots}{
  \describe{
    \item{\code{coefficients}:}{Matrix containing coefficients of models}
    \item{\code{effects}:}{List containing effects}                                                                
    \item{\code{test}:}{Matrix containing results from testing.}
    \item{\code{general}:}{Matrix providing the distribution of the number genes and arrays regarding the copy number states}
    \item{\code{modelsType}:}{List providing models' type}
    \item{\code{call.arg}:}{List providing details on the type of models that have been fitted.}
  }
}

\section{Methods}{
  \describe{
    \item{print}{Print the object information}
    \item{show}{Print the object information}
    \item{summary}{Print a summary of the object information}
  }
}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}


