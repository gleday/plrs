\name{plrs.test}
\alias{plrs.test}

\title{Likelihood ratio test for a \code{plrs} model}

\description{Test whether copy number has an effect on mRNA expression.}

\usage{
plrs.test(object, alpha=0.05)
}

\arguments{
  \item{object}{An object of class \code{\link{plrs-class}}}
  \item{alpha}{Significance level}
}

\details{
Two cases present themselves:\cr
\cr
1. The model is unconstrained. Thus, the model under the null hypothesis is the intercept
and an F-test is performed.\cr
\cr
2. The model is constrained and the following hypothesis are tested: \cr
H0: All constraints are actives (=)\cr
H1: At least one constraint is strict (>)\cr
Under H0, we always have the intercept model. Indeed, if \code{constr.slopes = 1} (or 2) and
\code{constr.intercepts = T}, then the only parameter free of inequality constraint is the 
overall intercept. If \code{constr.intercepts = F}, the local intercepts are additionnaly 
constrained to be 0 in order to obtain the intercept model under the null.
The likelihood ratio statistic (unknown variance) is asymptotically distributed as a
weighted mixture of Beta distribution (cf Gromping (2010)). Calculation of p-values is based on
functions \code{ic.weights} and \code{pbetabar} of package \pkg{ic.infer}. The package 
\pkg{mvtnorm} is also involved.\cr
\cr
In both cases the input model is taken as the model under the alternative.
}

\value{A \code{list} object with the following components:
  \item{stat}{Test statistic}
  \item{pvalue}{Calculated pvalue}
  \item{wt.bar}{Weights (if the model is constrained)}
  \item{df.bar}{Degrees of freedom.}
  \item{unconstr}{Unconstrained model of class \code{\link{plrs-class}}}
  \item{qbetabar}{(1-\code{alpha}) quantile of the beta mixture distribution}
  \item{alpha}{Significance level}
}

\references{
Gromping, U. (2010). Inference with linear equality and inequality constraints using
R: The package ic.infer. \emph{J Stat Softw}, 33(i10).
}

\author{Gwenael G.R. Leday  \email{g.g.r.leday@vu.nl}}

%\note{}
%\seealso{}
\examples{

# Simulate data
sim <- plrs.sim(n=80, states=2, sigma=0.5)

# Fit a model 
model <- plrs(expr=sim$expr, cghseg=sim$seg, cghcall=sim$cal)

# Testing
model <- plrs.test(model)
model

}

