#' Check Input Arguments
#'
#' Given outcomes, prediction scores, methods, checks for obvious issues,
#' and returns warnings if needed.
#' These may include unequal lengths, missing values, ensuring binary outcomes,
#' and numeric scores, etc.
#'
#' @inheritParams riskProfile
#'
#' @return A list of containing standardized outcomes and predicted scores.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' auroc <- read.csv(system.file("extdata", "sample.csv", package = "stats4phc"))
#' rscore <- auroc$predicted
#' truth <- as.numeric(auroc$actual)
#' inputCheck(truth, rscore)
#'
inputCheck <- function(outcome, score) {

  # Predscore - check numeric
  if (!is.numeric(score)) {
    stop("'score' vector must be represented as a numeric.")
  }

  # Outcome - logical to numeric
  if (is.logical(outcome)) {
    outcome <- as.numeric(outcome)
  }

  # Outcome - check numeric
  if (!is.numeric(outcome)) {
    stop("'outcome' vector needs to be a numeric vector.")
  }

  # Check matching lengths
  if (length(outcome) != length(score)) {
    stop("'outcome' and 'score' must have the same lengths.")
  }

  # Need at least 3 observations
  if (length(outcome) < 3) {
    stop("Need at least 3 observations.")
  }

  # DROP NA's for data pairs
  if (any(is.na(outcome)) || any(is.na(score))) {
    warning("Observations with NA's are dropped")
    ind <- intersect(
      which(complete.cases(outcome)),
      which(complete.cases(score))
    )
    outcome <- outcome[ind]
    score <- score[ind]
  }

  # Outcome - check incorrect values
  if (!all(outcome %in% 0:1)) {
    stop("'outcome' vector must be represented as binary 1 or 0.")
  }

  # Predscore - check low frequency of score values
  if (length(unique(score)) <= 5) {
    tbl <- table(score)
    if (any(tbl <= 3)) {
      warning(
        paste(
          "There is a low-occurrence value in `score` (", names(tbl)[tbl <= 3][1], ").",
          "The results may be unreliable."
        )
      )
    }
  }

  # Outcome - check low frequency
  if (any(prop.table(table(outcome)) <= 0.03)) {
    warning(
      paste(
        "There is a low frequency of one of the outcome classes (`prop.table(table(outcome))`).",
        "The results may be unreliable."
      )
    )
  }

  return(list(outcome = outcome, score = score))
}


#' Order Inputs
#'
#' Given a vector of prediction scores, and outcome, the function orders by score.
#' Option exists to reverse ordering, if lower scores correspond to higher rate of outcomes.
#'
#' @inheritParams riskProfile
#'
#' @return Returns an ordered and complete case dataframe of outcomes and score.
#'
#' @keywords internal
#' @noRd
#'
orderInputs <- function(outcome, score, rev.order = FALSE) {
  tempdf <- data.frame(score = score, outcome = outcome)
  if (rev.order) {
    tempdf$score <- -tempdf$score
  }
  tempdf <- tempdf[order(tempdf$score, tempdf$outcome), ]
  return(tempdf)
}

#' Method Check
#'
#' Usage:
#' 1. methods <- methodCheck(methods)
#' 2. getEstMethod(methods[[1]]) or getEstMethods(methods)
#' 3. getEst(methods, ...)
#'
#' @inheritParams  riskProfile
#'
#' @return list of named lists of method arguments
#'
#' @keywords internal
#' @noRd
#'
methodCheck <- function(methods) {

  # Drop empty strings
  if (any(methods == "")) {
    methods <- methods[methods != ""]
  }

  # If character is supplied
  if (is.character(methods)) {

    # Convert to lower-case
    orig.names <- methods
    methods <- tolower(methods)

    # Check uniqueness
    if (length(unique(methods)) != length(methods)) {
      stop("`methods` should be unique when specified as character.")
    }

    # Check available method
    if (!all(methods %in% names(est.funs()))) {
      stop(
        paste(
          "Supplied method is not yet available. Try selecting from (case insensitive): ",
          paste(shQuote(names(est.funs())), collapse = ", ")
        )
      )
    }

    # Convert to list
    methods <- structure(as.list(methods), names = orig.names)

    # Otherwise, if a list is provided:
    # list(gam1 = list(method = "gam", k = 3), pv = list(method = "pava", ...), etc)
    # or list(my_estimate = my_fun(outcome, score) {...})
  } else if (is.list(methods)) {

    # Check named list
    if (!checkmate::test_named(methods, type = "unique")) {
      stop("'methods' should be a uniquely named list.")
    }

    # Check and unify estimation method names
    methods <- lapply(methods, listMethodCheck)

  } else {
    stop(
      paste(
        "`methods` must be character",
        "or named list of estimation methods / user defined functions."
      )
    )
  }

  return(methods)
}

# x is a single method (element of an outer list)
listMethodCheck <- function(x) {
  UseMethod("listMethodCheck")
}

#' @export
listMethodCheck.list <- function(x) {

  # Check named list
  if (!checkmate::test_named(x, type = "unique")) {
    stop("Inner lists in the 'methods' argument must be named.")
  }

  # Check "method" element existing in the list
  if (is.null(x[["method"]]) || !is.character(x[["method"]])) {
    stop(
      paste(
        'All lists must have a "method" element specifying one of the predefined',
        "estimation functions as a string. Please select from:",
        paste(shQuote(names(est.funs())), collapse = ", ")
      )
    )
  }

  # Update method name
  x[["method"]] <- tolower(x[["method"]])

  # Check available method
  chck_available <- checkmate::test_subset(
    x[["method"]],
    choices = names(est.funs()), empty.ok = FALSE
  )
  if (!chck_available) {
    stop(
      paste0(
        "Supplied method '",
        x[["method"]],
        "' is not yet available. Try selecting from: ",
        paste(shQuote(names(est.funs())), collapse = ", ")
      )
    )
  }

  return(x)
}

#' @export
listMethodCheck.function <- function(x) {

  # Check input arguments
  if (!checkmate::test_function(x, args = c("outcome", "score"))) {
    stop(
      paste(
        "All user defined estimation functions need to take exactly two arguments:",
        "'outcome' and 'score'."
      )
    )
  }

  return(x)
}

#' @export
listMethodCheck.default <- function(x) {
  stop(
    paste(
      "The estimation method must be specified as a list or a function."
    )
  )
}

# compared to match.arg(..., several.ok = T),
# this does not allow NULL and returns a vector of the same length
matchArgSubset <- function(x, choices) {
  checkmate::assert_character(x, any.missing = FALSE, min.len = 1)
  out <- c()
  for (ag in x) {
    matched <- tryCatch(
      match.arg(ag, choices = choices),
      error = function(e) stop(sub("'arg'", paste0("'", ag, "'"), e))
    )
    out <- c(out, matched)
  }
  return(unique(out))
}


add0thPercPC <- function(x) {
  bind_rows(
    x,
    x %>%
      group_by(.data$method) %>%
      summarise(
        score = NA,
        percentile = 0,
        outcome = NA,
        estimate = .data$estimate[.data$percentile == min(.data$percentile)][1],
        pv = "PC",
        .groups = "drop"
      )
  ) %>%
    arrange(.data$method, .data$percentile)
}


add0thPercPV <- function(x) {
  bind_rows(
    x,
    x %>%
      group_by(.data$method, .data$pv) %>%
      summarise(
        score = NA,
        percentile = 0,
        estimate = NA,
        pvValue = dplyr::first(.data$pvValue),
        .groups = "drop"
      ) %>%
      dplyr::relocate("pv", .before = "pvValue")
  ) %>%
    arrange(.data$method, .data$pv, .data$percentile)
}


add0thPercTR <- function(x) {
  bind_rows(
    x,
    x %>%
      group_by(.data$method) %>%
      summarise(
        score = NA,
        percentile = 0,
        pf = "Sensitivity",
        value = 1
      ),
    x %>%
      group_by(.data$method) %>%
      summarise(
        score = NA,
        percentile = 0,
        pf = "Specificity",
        value = 0
      )
  ) %>%
    arrange(.data$method, .data$pf, .data$percentile)
}


#' For snapshot testing of graphs
#'
#' @param code Code to create a graph
#' @param width Width of the plot.
#' @param height Height of the plot.
#'
#' @return Filepath
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' expect_snapshot_file(save_png(ggplot(mtcars) +
#'   geom_point(aes(hp, mpg))), "riskProfile.png")
#'
save_png <- function(code, width = 400, height = 400) { # nocov start
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  print(code)
  return(path)
} # nocov end
