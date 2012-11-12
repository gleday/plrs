\name{plrs.cb}
\alias{plrs.cb}

\title{Uniform confidence bands (CB) for plrs models}

\description{
Determine uniform confidence intervals for predicted values of a 'plrs' model.
}

\usage{
plrs.cb(object, alpha=0.05, newcgh=NULL)
}

\arguments{
  \item{object}{An object of class \code{\link{plrs-class}}.}
  \item{alpha}{Significance level}
  \item{newcgh}{Vector of segmented values. Support for building CB.}
}

\details{
The input object of class \code{\link{plrs-class}} has to result from function \code{\link{plrs.test}}.\cr

The problem of finding (at a given x) a confidence interval for the mean response is expressed as a
semi-definite optimization problem and solved using function \code{csdp} of package \pkg{Rcsdp}.
}

\value{
An object of class \code{\link{plrs-class} that contains CB information.}
}

\references{
Leday GGR, Van der Vaart AW, Van Wieringen WN, Van de Wiel MA. Modeling association between DNA copy number and gene expression with constrained piecewise linear regression splines. Accepted for publication. \emph{Ann Appl Stat}. (2012).
}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}

\seealso{
\code{\link{plrs.test}}
}

\examples{

# Simulate data
sim <- plrs.sim(n=80, states=4, sigma=0.5)

# Fit a model 
model <- plrs(expr=sim$expr, cghseg=sim$seg, cghcall=sim$cal)

# Confidence bands
model <- plrs.test(model)
model <- plrs.cb(model, alpha=0.05)
plot(model)
}

