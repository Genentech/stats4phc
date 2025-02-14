
# Calculate Predictive Value (PV) Estimates
getPVdata <- function(methods, outcome, score, pc.ests = NULL) {

  # Calculate estimates if not provided
  if (is.null(pc.ests)) {
    pc.ests <- getEsts(outcome = outcome, score = score, methods = methods)
    pc.data <- pc.ests$plotdata
  } else {
    pc.data <- pc.ests$plotdata
  }

  # locf and zip for pava and/or binned data
  if (any(pc.ests$idx.step)) {
    name.binned <- names(methods)[pc.ests$idx.binned]
    name.pava <- names(methods)[pc.ests$idx.pava]
    pc.data <- locf(pc.data, method.binned = name.binned, method.pava = name.pava)
  }

  # Calculate PV
  PV.data <- parametricPV(pc.data = pc.data)

  return(as.data.frame(PV.data))
}


# Calculates PV values from cutoff thresholds
nonParametricPV <- function(outcome, score) {

  prev <- mean(outcome, na.rm = TRUE)

  thresh.predictions <- lapply(score, function(x) as.numeric(score > x))

  ppv <- vapply(
    thresh.predictions[1:(length(score) - 1)],
    function(x) {
      tp <- sum(outcome == 1 & x == 1)
      fp <- sum(outcome == 0 & x == 1)
      tp / (tp + fp)
    },
    numeric(1)
  )

  npv <- vapply(
    thresh.predictions[1:(length(score) - 1)],
    function(x) {
      tn <- sum(outcome == 0 & x == 0)
      fn <- sum(outcome == 1 & x == 0)
      tn / (tn + fn)
    },
    numeric(1)
  )

  threshold.data <- data.frame(
    score = score,
    percentile = ecdf(score)(score),
    PPV = c(prev, ppv),
    NPV = c(npv, 1 - prev)
  ) %>%
    mutate(MNPV = 1 - .data$NPV) %>%
    tidyr::fill(
      all_of(c("PPV", "NPV", "MNPV")),
      .direction = "downup"
    )

  return(threshold.data)
}


# Calculates PV values based on pracma::cumtrapz
parametricPV <- function(pc.data) {

  # Calculate PVs
  PV.data <- pc.data %>%
    group_by(.data$method) %>%
    mutate(
      MNPV = pracma::cumtrapz(.data$percentile, .data$estimate)[, 1] / .data$percentile,
      NPV = 1 - .data$MNPV,
      PPV =
        (
          max(pracma::cumtrapz(.data$percentile, .data$estimate), na.rm = TRUE) -
            pracma::cumtrapz(.data$percentile, .data$estimate)[, 1]
        ) / (max(.data$percentile) - .data$percentile)
    ) %>%
    ungroup() %>%
    arrange(.data$method, .data$percentile)

  # Fill NAs (starting points)
  PV.data <- PV.data %>%
    group_by(.data$method) %>%
    mutate(
      MNPV = ifelse(.data$percentile == min(.data$percentile) & is.na(.data$MNPV), 0, .data$MNPV),
      NPV = ifelse(.data$percentile == min(.data$percentile) & is.na(.data$NPV), 1, .data$NPV)
    ) %>%
    as.data.frame()

  # Fill NAs (ending points)
  PV.data <- tidyr::fill(
    PV.data,
    all_of(c("estimate", "MNPV", "NPV", "PPV")),
    .direction = "down"
  )

  return(as.data.frame(PV.data))
}


# consistent colors for PVs
predictionColours <- function(x, show.best) {
  clrs <- c(
    "Best NPV" = "gray80",
    "NPV" = "#53B400",
    "Best PPV" = "gray65",
    "PPV" = "red",
    "Best PC" = "black",
    "PC" = "plum",
    "1-NPV" = "royalblue2",
    "Best 1-NPV" = "gray50"
  )
  if (show.best) {
    x <- c(x, paste("Best", x))
  }
  return(clrs[names(clrs) %in% x]) # keep the ordering above
}


# Calculates sensitivity and specificity (true positive / negative rate)
nonParametricTR <- function(outcome, score) {

  # Tresh_predictions are lists of 0,1's based on each finer_rscore as a cutpoint
  thresh.predictions <- lapply(score, function(x) as.numeric(score > x))

  # Calc sensitivities and specificities at each risk percentile threshold
  senses <- vapply(
    thresh.predictions[1:(length(score) - 1)],
    function(x) {
      sum(outcome == 1 & x == 1) / sum(outcome == 1)
    },
    numeric(1)
  )

  specs <- vapply(
    thresh.predictions[1:(length(score) - 1)],
    function(x) {
      sum(outcome == 0 & x == 0) / sum(outcome == 0)
    },
    numeric(1)
  )

  # Create a data.frame
  dat <- data.frame(
    score = c(min(score), score),
    percentile = c(0, ecdf(score)(score)),
    Sensitivity = c(1, senses, 0),
    Specificity = c(0, specs, 1)
  ) %>%
    tidyr::pivot_longer(
      cols = c("Sensitivity", "Specificity"),
      names_to = "pf",
      values_to = "value"
    )

  return(as.data.frame(dat))
}



adjPrevPerc <- function(perc, prev.new, cdf.case, cdf.control) {
  prev.new * cdf.case(perc) + (1 - prev.new) * cdf.control(perc)
}

adjPrevEst <- function(x, prev, prev.new) {
  f2 <- prev / (1 - prev)
  f3 <- (1 - prev.new) / prev.new
  1 / (((1 / x) - 1) * f2 * f3 + 1)
}

adjPrevPC <- function(dat, prev, prev.new, cdf.case, cdf.control) {
  mutate(
    dat,
    percentile = adjPrevPerc(
      perc = .data$score, prev.new = prev.new,
      cdf.case = cdf.case, cdf.control = cdf.control
    ),
    estimate = adjPrevEst(x = .data$estimate, prev = prev, prev.new = prev.new)
  )
}

adjPrevPV <- function(dat, prev, prev.new, cdf.case, cdf.control) {
  mutate(
    dat,
    percentile = adjPrevPerc(
      perc = .data$score, prev.new = prev.new,
      cdf.case = cdf.case, cdf.control = cdf.control
    ),
    MNPV = adjPrevEst(x = .data$MNPV, prev = prev, prev.new = prev.new),
    NPV = 1 - .data$MNPV,
    PPV = adjPrevEst(x = .data$PPV, prev = prev, prev.new = prev.new)
  )
}



bestPPV <- function(perc, prev) {
  ifelse(perc > 1 - prev, 1, prev / (1 - perc))
}

bestMNPV <- function(perc, prev) {
  ifelse(perc <= 1 - prev, 0, 1 - ((1 - prev) / perc))
}

bestSens <- function(perc, prev) {
  ifelse(perc <= 1 - prev, 1, (1 - perc) / prev)
}

bestSpec <- function(perc, prev) {
  ifelse(perc > 1 - prev, 1, perc / (1 - prev))
}
