\name{modify.conf}
\alias{modify.conf}

\title{Modify the configuration (of calls) of the plrs model}

\description{
This function changes the discrete copy number values for a given gene in order to 
force a minimum number of observations per state.
}

\usage{
modify.conf(cghcall, min.obs = 3, discard = TRUE)
}

\arguments{
  \item{cghcall}{Vector of called values}
  \item{min.obs}{Minimum number of observations per state}
  \item{discard}{Logical. Whether discrete states with few observations should be discarded from analysis.}
}

\details{
Consider that the number of observations of a given state is lower than \code{min.obs}, then:

- if \code{discard = FALSE}, observations are not discarded and a rearrangement of called values is carried out as follows.
The "normal" copy number state is taken as a reference. If the minimum number of observations is not obtained,
"losses" will be merged to "normals", "gains" to "normals" and "amplifications" to "gains".
Note that this modifies the configuration of the model. Thus, after fitting a model using \code{\link{plrs}},
original and modified data are stored in the resulting \code{\link{plrs-class}} object, respectively under
slots \code{data} and \code{mdata}.

- if \code{discard = TRUE}, states for which the number of observations is lower than \code{min.obs}
are discarded (replaced by NAs).

}

\value{
  \item{val}{Vector of new called values}
}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}

\note{
This function is implemented within function \code{\link{plrs}} and \code{\link{plrs.series}}.
}

\examples{
called <- sample(c(rep(-1,5),rep(0,15),rep(1,2),rep(2,1)))
table(called)
table(modify.conf(called, min.obs=3))
}
