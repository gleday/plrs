\name{plrs.select}
\alias{plrs.select}
\alias{plrs.select,ANY}

\title{Model selection}

\description{Selection of a model based on an information criterion (AIC, AICC, BIC or OSAIC).}

\usage{
plrs.select(object, crit = ifelse(object@call.arg$constr,"osaic","aic"))
}

\arguments{
  \item{object}{An object of class \code{\link{plrs-class}}}
  \item{crit}{Character corresponding to the criterion to use. See \code{\link{criteria}}.}
}

%\details{

%}

\value{An object of class \code{\link{plrs.select-class}}
}

\author{Gwenaël G.R. Leday}

%\note{}
%\seealso{}
%\examples{}
%\keyword{}
