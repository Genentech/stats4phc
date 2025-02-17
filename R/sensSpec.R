#' Sensitivity and specificity plot
#'
#' Sensitivity and specificity risk estimates
#' 
#' Given individual binary outcomes and scores, this function plots sensitivity and specificity 
#' (using each score as a cutoff) on their respective score percentiles.
#'
#' @inheritParams riskProfile
#' @param show.best Logical; Include best possible sensitivity and specificity? Defaults to `TRUE`.
#'
#' @inheritSection riskProfile Estimation
#'
#' @return A list containing the plot and data.
#'
#' @export
#'
#' @seealso [riskProfile()] [calibrationProfile()]
#'
#' [getPAVAest()] [getBINNEDest()] [getGAMest()] [getCGAMest()] [getMSPLINEest()]
#' [getASISest()]
#'
#' @examples
#' # Read in example data
#' auroc <- read.csv(system.file("extdata", "sample.csv", package = "stats4phc"))
#' rscore <- auroc$predicted_calibrated
#' truth <- as.numeric(auroc$actual)
#'
#' # Plot sensitivity and specificity
#' p1 <- sensSpec(outcome = truth, score = rscore)
#' p1$plot
#'
#' # Same with smoothed estimates
#' p2 <- sensSpec(outcome = truth, score = rscore, methods = c("asis", "gam"))
#' p2$plot
#'
sensSpec <- function(outcome,
                     score,
                     methods = "asis",
                     show.best = TRUE,
                     plot.raw = FALSE,
                     rev.order = FALSE) {

  # Argument checks
  checkmate::assert_flag(show.best)
  checkmate::assert_flag(plot.raw)
  checkmate::assert_flag(rev.order)

  # Check methods
  methods <- methodCheck(methods = methods)

  if (plot.raw) {
    xvar <- "score"
  } else {
    xvar <- "percentile"
  }

  # Standardize/Check outcome, scores
  op <- inputCheck(outcome = outcome, score = score)

  # Order Data by scores
  tempdf <- orderInputs(outcome = op$outcome, score = op$score, rev.order = rev.order)
  score <- tempdf$score
  outcome <- tempdf$outcome

  # Calculate spec and sens
  dat <- getEsts(outcome = outcome, score = score, methods = methods)$plotdata
  dat <- split(dat, dat$method) %>%
    lapply(\(d) nonParametricTR(outcome = d$outcome, score = d$estimate)) %>%
    bind_rows(.id = "method")
  
  if (!plot.raw) {
    dat <- add0thPercTR(dat)
  }

  # Plot
  p <- ggplot(dat) +
    geom_step(
      aes(x = .data[[xvar]], y = .data$value, colour = .data$method, linetype = .data$pf),
      direction = "hv"
    )

  # Add best possible sensitivity and specificity
  if (show.best) {
    prev <- mean(outcome)
    if (plot.raw) {
      best <- data.frame(percentile = ecdf(score)(score), score = score)
    } else {
      best <- data.frame(percentile = c(0, ecdf(score)(score)), score = c(NA, score))
    }
    best <- best %>%
      distinct() %>%
      mutate(
        Sensitivity = bestSens(perc = .data$percentile, prev = prev),
        Specificity = bestSpec(perc = .data$percentile, prev = prev)
      ) %>%
      tidyr::pivot_longer(
        cols = c("Sensitivity", "Specificity"), names_to = "pf", values_to = "value"
      ) %>%
      mutate(method = "best")
    p <- p +
      geom_line(
        aes(x = .data[[xvar]], y = .data$value, linetype = .data$pf, linewidth = "Best Possible"),
        data = best,
        colour = "gray70"
      ) +
      scale_linewidth_manual(values = c("Best Possible" = 0.5), name = NULL)
  }

  # Finalize plot
  p <- p +
    labs(
      title = "Sensitivity and Specificity Plot",
      x = ifelse(plot.raw, "Prediction Score", "Risk Percentile"),
      y = "True Positive / Negative Rate",
      colour = "Estimation Method",
      linetype = "Predictive Quantity"
    ) +
    scale_x_continuous(n.breaks = 6) +
    scale_y_continuous(n.breaks = 6) +
    scale_linetype_manual(values = c("Sensitivity" = "solid", "Specificity" = "dashed")) +
    scale_colour_hue(l = 45) +
    theme_bw() +
    theme(legend.key.width = unit(2, "line")) +
    guides(
      linetype = guide_legend(order = 1, override.aes = list(colour = "black")),
      colour = guide_legend(order = 2),
      linewidth = guide_legend(order = 3)
    )

  if (show.best) {
    dat <- bind_rows(dat, best)
  }

  return(list(plot = p, data = dplyr::as_tibble(dat)))
}
