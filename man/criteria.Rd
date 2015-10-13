\name{criteria}
\alias{criteria}
\alias{criteria,plrs-method}

\title{
  Compute AIC, AICC, BIC and OSAIC for a given \code{plrs} model.
}

\description{Extract AIC, AICC, BIC and OSAIC from an object of class \code{\link{plrs-class}}.}

\usage{
criteria(obj, crit = "all")
}

\arguments{
  \item{obj}{object of class \code{\link{plrs-class}}}
  \item{crit}{A \code{character} (vector) among \code{"aic"}, \code{"aicc"}, \code{"bic"}, \code{"osaic"} or \code{"all"}.}
}

\value{
  A \code{list} with the following components (if specified):
  \item{aic}{Akaike's information criterion}
  \item{aicc}{Small sample correction of AIC}
  \item{bic}{Bayesian Information Criterion}
  \item{osaic}{One-Sided AIC. See Hughes and King (2003) for more details.}
}

\references{
  Hughes, A. W. and King, M. L. (2003). Model selection using AIC in the presence of one-sided information. \emph{J Stat Plan Infer}, 115(2): 397 411.
}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}

%\note{}
%\seealso{}
\examples{

# Simulate data
sim <- plrs.sim(n=80, states=4, sigma=0.5)

# Fit
model <- plrs(expr=sim$expr, cghseg=sim$seg, cghcall=sim$cal)

criteria(model)

}
