\name{plrs-class}
\Rdversion{1.1}
\docType{class}
\alias{plrs-class}
\alias{coef,plrs-method}
\alias{effects,plrs-method}
\alias{fitted,plrs-method}
\alias{knots,plrs-method}
\alias{model.matrix,plrs-method}

\alias{predict,plrs-method}
\alias{print,plrs-method}
\alias{residuals,plrs-method}
\alias{show,plrs-method}
\alias{summary,plrs-method}

\title{Class \code{plrs}}
\description{
An S4 class representing the output of the \code{\link{plrs}} function.
}

\section{Slots}{
  \describe{
    \item{\code{coefficients}:}{Object of class \code{numeric} containing spline coefficients}
    \item{\code{fitted.values}:}{Object of class \code{numeric} containing the fitted values}
    \item{\code{residuals}:}{Object of class \code{numeric} containing the residuals}
    \item{\code{X}:}{Object of class \code{matrix} containing the design matrix}
    \item{\code{data}:}{Object of class \code{list} containing input data}
    \item{\code{mdata}:}{Object of class \code{list} containing (possibly modified) data used to fit the model (See \code{\link{modify.conf}}).}
    \item{\code{QP}:}{Object of class \code{list} containing input elements used for quadratic programming. If the model is unconstrained this contains a light version of an \code{lm} object.}
    \item{\code{test}:}{Object of class \code{list} containing results from testing.}
    \item{\code{cb}:}{Object of class \code{list} containing lower and upper bounds for predicted values.}   
    \item{\code{selected}:}{Object of class \code{logical} indicating whether the model results from a selection procedure.}
    \item{\code{type}:}{Object of class \code{character} giving the type of model}
    \item{\code{call.arg}:}{Object of class \code{list} containing the input arguments (for reproducibility)}
  }
}

\section{Methods}{
  \describe{
    \item{coef}{Returns the coefficients}
    \item{criteria}{See \code{\link{criteria}}}
    \item{effects}{Returns matrix of effects}
    \item{fitted}{Returns the fitted values}
    \item{knots}{Returns the knots}
    \item{model.matrix}{Returns the design matrix}
    \item{plot}{See \code{\link{plot.plrs}}}
    \item{predict}{See \code{\link{predict.plrs}}}
    \item{print}{Print the object information}
    \item{residuals}{Returns the residuals}
    \item{show}{Print the object information}
    \item{summary}{Print a summary of the object information}
  }
}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}


