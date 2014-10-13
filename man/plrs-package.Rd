\name{plrs-package}
\alias{plrs-package}
\docType{package}

\title{Piecewise Linear Regression Splines (PLRS) for the association between DNA copy number and mRNA expression}

\description{
The present package implements a framework for modeling the relationship between DNA copy number and gene expression data using Piecewise Linear Regression Splines (PLRS). It includes (point and interval) estimation, model selection and testing procedures for such models (possibly under biologically motivated constraints).
}

\details{
The use of the present package can be divided into two approaches:\cr
\cr
1. Analysis of a single DNA-mRNA relationship\cr
\cr
Main functions are:\cr
\code{\link{plrs}}:	Fit a single plrs model.\cr
\code{\link{plrs.select}}: Model selection based on AIC, AICC, OSAIC or BIC.\cr
\code{\link{plrs.test}}:	Likelihood ratio test for a given plrs model.\cr
\code{\link{plrs.cb}}: Confidence bands for a plrs model.\cr
\cr
\cr
2. Analysis of multiple DNA-mRNA relationships sequentially\cr
\cr
Main function is:\cr
\code{\link{plrs.series}}: point and interval estimation, model selection and testing of DNA-mRNA association for a series of arrays.\cr
\cr
Note: This function extend the aforementioned univariate analysis genomewise in the same spirit as some functions of the \pkg{limma} package do. 
}

\author{
Gwenael G.R. Leday

Maintainer: Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}
}

\references{
Leday GGR, Van der Vaart AW, Van Wieringen WN, Van de Wiel MA. Modeling association between DNA copy number and gene expression with constrained piecewise linear regression splines. Accepted for publication. \emph{Ann Appl Stat}. (2012).
}

\keyword{copy number, gene expression, regression splines, model selection, constrained inference.}