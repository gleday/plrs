\name{plrs.sim}
\alias{plrs.sim}

\title{
Simulation of a plrs model
}

\description{
Simulation of a piecewise relationship.\cr
\cr
The function has been only implemented for convenience of simulations and R examples.\cr
}

\usage{
plrs.sim(n = 80, states = 4, sigma = 01, x = NULL)
}

\arguments{
  \item{n}{Number of simulated data points}
  \item{states}{Number of states for the model}
  \item{sigma}{Noise}
  \item{x}{Segmented values.}
}

\details{
To be written...
}

%\value{}

\author{Gwenael G.R. Leday \email{g.g.r.leday@vu.nl}}

\examples{

# Simulate 1-state model
sim <- plrs.sim(n=80, states=1, sigma=0.5)
model <- plrs(expr=sim$expr, cghseg=sim$seg, cghcall=sim$cal)
plot(model)

# Simulate 2-state model
sim <- plrs.sim(n=80, states=2, sigma=0.5)
model <- plrs(expr=sim$expr, cghseg=sim$seg, cghcall=sim$cal)
plot(model)

# Simulate 3-state model
sim <- plrs.sim(n=90, states=3, sigma=0.5)
model <- plrs(expr=sim$expr, cghseg=sim$seg, cghcall=sim$cal)
plot(model)

# Simulate 4-state model
sim <- plrs.sim(n=80, states=4, sigma=0.5)
model <- plrs(expr=sim$expr, cghseg=sim$seg, cghcall=sim$cal)
plot(model)

}
