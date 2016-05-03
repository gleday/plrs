\name{plrs.series}
\alias{plrs.series}

\title{
Fit plrs models for a series of arrays.
}

\description{
The function fits \code{plrs} models for a series of arrays. Model selection and testing procedures may be applied.
}

\usage{
plrs.series(expr, cghseg, cghcall=NULL,
probloss = NULL, probnorm = NULL, probgain = NULL, probamp = NULL,
control.model  = list(continuous = FALSE,
                      constr = TRUE,
                      constr.slopes = 2,
                      constr.intercepts = TRUE,
                      min.obs = 3,
                      discard.obs = TRUE),
control.select = list(crit = ifelse(control.model$constr, "osaic","aic")),
control.test   = list(testing = TRUE,
                      cb = FALSE,
                      alpha = 0.05),
control.output = list(save.models = FALSE,
                      save.plots = FALSE,
                      plot.lin = FALSE,
                      type = "jpeg"))
}

\arguments{
  \item{expr}{Either a matrix of expression profiles or an \code{\link[Biobase]{ExpressionSet}} object.}
  \item{cghseg}{Either a matrix of segmented copy number values or objects of class \code{\link[CGHbase]{cghSeg}} or \code{\link[CGHbase]{cghCall}}}
  \item{cghcall}{Matrix of called copy number}
  \item{probloss}{Matrix of call probabilities associated with state "loss". Default is \code{NULL}.}
  \item{probnorm}{Matrix of call probabilities associated with state "normal". Default is \code{NULL}.}
  \item{probgain}{Matrix of call probabilities associated with state "gain". Default is \code{NULL}.}
  \item{probamp}{Matrix of call probabilities associated with state "amplification". Default is \code{NULL}.}
  \item{control.model}{See details}
  \item{control.select}{See details}
  \item{control.test}{See details}
  \item{control.output}{See details}
}

\details{
If DNA and mRNA input data are matrices, rows should correspond to genes and columns to arrays.
Alternatively, expression data may be provided as an \code{\link[Biobase]{ExpressionSet}} object and aCGH data
as \code{\link[CGHbase]{cghSeg}} or \code{\link[CGHbase]{cghCall}} objects. A \code{\link[CGHbase]{cghCall}} object
contain all data from the calling step, thus arguments \code{probloss}, \code{probnorm}, \code{probnorm} and \code{probamp}
can be omitted. An object of class \code{\link[CGHbase]{cghSeg}} does not contain such data so only simple linear models
will be fitted.\cr
\cr
\code{control.model} allows the user to specify the type of model that has to be fitted. 
This must be a \code{list} with one or more of the following components:
\code{constr}, \code{constr.slopes}, \code{constr.intercepts}, \code{min.obs} and \code{discard.obs}.
See functions \code{\link{plrs}} and \code{\link{modify.conf}} for more details.\cr
\cr
\code{control.select} allows the user to specify whether model selection should be done and how.
This must be a \code{list} with a component named \code{crit}. See function \code{\link{plrs.select}} for more details.
If \code{control.select = NULL} then no model selection is done. \cr
\cr
\code{control.output} allows the user to plot and save each \code{plrs} model. This must be a list with components:\cr
\code{save.models}, a logical. This will create within the work directory a new directory named "plrsSeriesObjects" that will contain all objects.\cr
\code{save.plots}, a logical. This will create within the work directory a new directory named "plrsSeriesPlots" that will contains all saved plots.\cr
\code{plot.lin}, a logical. Whether the simple linear model should aslo be plotted.\cr
\code{type}, a character. Format of file. To pass through function \code{\link[grDevices]{savePlot}}.\cr
}

\value{An object of class \code{\link{plrs.series-class}}}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}

%\note{}
%\seealso{}

\examples{

# Simulate data
ngenes <- 10
narray <- 48
rna <- dnaseg <- dnacal <- matrix(NA, ngenes, narray)
idx <- sample(1:4, ngenes, replace=TRUE, prob=rep(1/4,4))
for(i in 1:ngenes){
	Sim <- plrs.sim(n=narray, states=idx[i], sigma=0.5)
	rna[i,] <- Sim$expr
	dnaseg[i,] <- Sim$seg
	dnacal[i,] <- Sim$cal
}


# Screening procedure with linear model
series <- plrs.series(expr = rna, cghseg = dnaseg, cghcall = NULL, control.select = NULL)

# Screening procedure with full plrs model
series <- plrs.series(expr = rna, cghseg = dnaseg, cghcall = dnacal, control.select = NULL)

# Model selection
series <- plrs.series(expr = rna, cghseg = dnaseg, cghcall = dnacal)

}

