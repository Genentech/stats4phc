% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getEstimates.R
\name{getPAVAest}
\alias{getPAVAest}
\title{PAVA Risk Estimates}
\usage{
getPAVAest(
  outcome,
  score,
  weights = rep(1, length(outcome)),
  ties = "primary",
  low_events = NULL,
  low_nonevents = NULL,
  high_events = NULL,
  high_nonevents = NULL,
  hilo_obs = NULL
)
}
\arguments{
\item{outcome}{Vector of binary outcome for each observation.}

\item{score}{Numeric vector of continuous predicted risk score.}

\item{weights}{Vector of numerics to specify PAVA observation weighting.}

\item{ties}{String to specify how ties should be handled for PAVA.}

\item{low_events}{Numeric, specifying number of events in the lowest bin.}

\item{low_nonevents}{Numeric, specifying number of nonevents in the lowest bin.}

\item{high_events}{Numeric, specifying number of events in the highest bin.}

\item{high_nonevents}{Numeric, specifying number of nonevents in the highest bin.}

\item{hilo_obs}{Numeric, specifying number of observations in the highest and lowest bins.}
}
\value{
A data frame with 4 columns
(score, score percentile, outcome, estimate).
}
\description{
Determines isotonic regression estimates via pava, given a vector of binary outcomes,
and a vector of scores.
}
\examples{
# Read in example data
auroc <- read.csv(system.file("extdata", "sample.csv", package = "stats4phc"))
rscore <- auroc$predicted
truth <- as.numeric(auroc$actual)

tail(getPAVAest(outcome = truth, score = rscore), 10)

}
\seealso{
\code{\link[=getASISest]{getASISest()}} \code{\link[=getBINNEDest]{getBINNEDest()}} \code{\link[=getCGAMest]{getCGAMest()}} \code{\link[=getGAMest]{getGAMest()}} \code{\link[=getMSPLINEest]{getMSPLINEest()}}
}
