\name{plrs}
\alias{plrs}
\alias{plrs,ANY}

\title{Fit a (constrained) piecewise linear regression spline}

\description{The function fits a piecewise linear regression spline to explain 
gene expression by the segmented DNA copy number. The called copy number values 
are used as a template for model building.}

\usage{
plrs(expr, cghseg, cghcall=NULL, probloss = NULL, probnorm = NULL,
probgain = NULL, probamp = NULL, knots = NULL, continuous = FALSE,
constr = TRUE, constr.slopes = 2, constr.intercepts = TRUE,
min.obs = 3, discard.obs = TRUE)
}

\arguments{
  \item{expr}{Vector of gene expression values}
  \item{cghseg}{Vector of segmented copy number values}
  \item{cghcall}{Vector of called copy number values. If not provided, we are reduced to a simple linear model.}
  \item{probloss}{Vector of call probabilities associated with state "loss". Default is \code{NULL}.}
  \item{probnorm}{Vector of call probabilities associated with state "normal". Default is \code{NULL}.}
  \item{probgain}{Vector of call probabilities associated with state "gain". Default is \code{NULL}.}
  \item{probamp}{Vector of call probabilities associated with state "amplification". Default is \code{NULL}.}
  \item{knots}{knots or change points. If \code{NULL} (default), there are estimated. See details.}
  \item{continuous}{Logical, whether the model is continuous (no jump) or not.}
  \item{constr}{Logical, whether the model is constrained or not. (this has been implemented to turn on and off easily the constraints)}
  \item{constr.slopes}{Type of non-negativity constraints applied on slopes. Either 1 or 2 (default). See details.}
  \item{constr.intercepts}{If \code{TRUE} (default) jumps from state to state are also constrained to be non-negative}
  \item{min.obs}{See \code{\link{modify.conf}}}
  \item{discard.obs}{See \code{\link{modify.conf}}}
}

\details{
If \code{cghcall=NULL}, discrete copy number values are omitted, which results in fitting a simple linear model.\cr
\cr
If \code{constr.slopes=1}, all slopes are constrained to be non-negative.
If \code{constr.slopes=2}, the slope associated with state "normal" is constrained to be non-negative and all others are forced to be at least equal to the latter.\cr
\cr
Two methods are implemented for the estimation of knots. If call probabilities are provided,
a knot is determined so that the sum of (the two adjacent) states membership probabilities is maximized.
Otherwise, this is defined as the midpoint of the interval between the two consecutive states.\cr

The constrained least squares problem is solved using function \code{solve.QP} of package \pkg{quadprog}.
}

\value{An object of class \code{\link{plrs-class}}}

%\references{}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}

%\note{}
%\seealso{}

\examples{

# Simulate data
sim <- plrs.sim(n=80, states=4, sigma=0.5)

# Fit a model 
model <- plrs(expr=sim$expr, cghseg=sim$seg, cghcall=sim$cal)
model

# Methods
coef(model)
effects(model)
fitted(model)
knots(model)
model.matrix(model)
plot(model)
predict(model, newcghseg=seq(0,5, length.out=100))   
residuals(model)
summary(model)
}



