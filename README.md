# stats4phc <img src="man/figures/logo.png" align="right" width="260px" height="300px" />


Performance evaluation for the prognostic value of predictive models intended to 
support personalized healthcare (phc) when the outcomes of interest are binary. 
<a href="https://pubmed.ncbi.nlm.nih.gov/17982157/" target="_blank">Predictiveness curves</a>
are an insightful visualization to assess the inherent ability of such 
models to provide predictions to individual patients. Cumulative versions of predictiveness 
curves represent positive predictive values and 1 - negative predictive values and are also 
informative if the eventual goal is to use a cutoff for clinical decision making. 
In addition, predictiveness curves and their cumulative versions are naturally related to 
<a href="https://www.bmj.com/content/352/bmj.i6" target="_blank">net benefit</a>
performance metrics to assess clinical utility for phc. Finally, some authors have 
proposed a visualization that assesses both the prognostic value of predictive models and 
their performance as a classifier. This package provides a variety of functions for estimation 
and plotting of these performance evaluation curves and metrics.


## Installation

``` r
remotes::install_github(repo = "Genentech/stats4phc")
```

## Documentation

Please refer to https://genentech.github.io/stats4phc 
where you can see function reference as well as introduction vignette.

## Example

This is a basic example which demonstrates one of the plotting functions from the package, 
`riskProfile`:

``` r
library(stats4phc)

# Read in example data
auroc <- read.csv(system.file("extdata", "sample.csv", package = "stats4phc"))
rscore <- auroc$predicted_calibrated
truth <- as.numeric(auroc$actual)

# Default plot includes 1-NPV, PPV, and a predictiveness curve (PC) 
p1 <- riskProfile(outcome = truth, score = rscore)
# p1 is a list with "plot" and "data" elements, see `p1$plot` or `p1$data`

# You can select an estimation method with specific arguments
p2 <- riskProfile(
  outcome = truth,
  score = rscore,
  methods = list(
    "gam" = list(method = "gam", bs = "tp", k = 5, logscores = FALSE, fitonPerc = TRUE),
    "asis" = list(method = "asis"), # no arguments for this method
    "bin" = list(method = "binned", quantiles = 10, errorbar.sem = 1)
  )
)
# see `p2$plot` or `p2$data`
```
