% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEstimates.R
\name{getCGAMest}
\alias{getCGAMest}
\title{Constrained GAM (cgam) Risk Estimates}
\usage{
getCGAMest(
  outcome,
  score,
  numknots = 0,
  smoother = "s.incr",
  logscores = FALSE,
  fitonPerc = TRUE
)
}
\arguments{
\item{outcome}{Vector of binary outcome for each observation.}

\item{score}{Numeric vector of continuous predicted risk score.}

\item{numknots}{Numeric to specify the number of knots.
Passed to the \code{smoother} function. Defaults to 3.}

\item{smoother}{Character string to specify the smoother (from cgam package).
Defaults to "s.incr".}

\item{logscores}{Logical; if \code{TRUE}, fit gam on log scores. Defaults to \code{FALSE}.}

\item{fitonPerc}{Logical; if \code{TRUE}, fit gam on risk percentiles. Defaults to \code{TRUE}.}
}
\value{
A data frame with 4 columns
(score, score percentile, outcome, estimate).
}
\description{
Fits a Constrained Generalized Additive Model to estimate risk,
given a vector of binary outcomes and a vector of scores.
}
\examples{
# Read in example data
auroc <- read.csv(system.file("extdata", "sample.csv", package = "stats4phc"))
rscore <- auroc$predicted
truth <- as.numeric(auroc$actual)

tail(getCGAMest(outcome = truth, score = rscore), 10)

}
\seealso{
\code{\link[=getASISest]{getASISest()}} \code{\link[=getBINNEDest]{getBINNEDest()}} \code{\link[=getGAMest]{getGAMest()}} \code{\link[=getMSPLINEest]{getMSPLINEest()}} \code{\link[=getPAVAest]{getPAVAest()}}
}
