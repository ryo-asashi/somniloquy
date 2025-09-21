#' Create Calibration Plots
#'
#' @description
#' \code{calibration_plot()} generates a calibration plot to assess the performance of a model's probabilistic predictions.
#'
#' @details
#' The function groups predicted probabilities into bins by \code{findInterval(predicted, breaks, rightmost.closed = TRUE, left.open = FALSE, all.inside = FALSE)}, and plots the mean predicted probability (x-axis) against the fraction of positive actual outcomes (y-axis) for each bin.
#' A perfectly calibrated model would have points lying on the diagonal line \eqn{y = x}, indicating that a predicted probability of, for example, 0.8 corresponds to an 80 percent proportion of positive outcomes.
#'
#' @param actual a vector of true outcomes. Must be a numeric vector containing \code{0} and \code{1}.
#' @param predicted a numeric vector of predicted probabilities, typically ranging from \code{0} to \code{1}.
#' @param breaks a numeric vector of cut points used to bin the predicted values. Defaults to \code{seq(0, 1, by = .1)}.
#' @param show_plot logical. If \code{TRUE} (the default), a plot is displayed. If \code{FALSE}, the summary data is returned without plotting.
#' @param ... additional arguments passed to the \code{plot()} function. This can be used to customize the plot's title (\code{main}), color (\code{col}), point and line types (\code{type}), etc.
#'
#' @examples
#' # Generate sample data
#' n_obs <- 500
#' actual <- sample(0:1, n_obs, replace = TRUE, prob = c(0.7, 0.3))
#'
#' # Generate slightly miscalibrated predictions based on actuals
#' predicted <- ifelse(actual == 1,
#'                     rbeta(n_obs, shape1 = 4, shape2 = 1.5),
#'                     rbeta(n_obs, shape1 = 1, shape2 = 4))
#' predicted <- pmin(pmax(predicted, 0), 1)
#'
#' # Basic plot
#' calibration_plot(actual, predicted)
#'
#' # Customize the plot
#' calibration_plot(actual, predicted,
#'                  main = "Calibration Plot",
#'                  xlab = "Mean Predicted Probability",
#'                  ylab = "Observed Fraction of Positives",
#'                  col = "maroon",
#'                  pch = 19,
#'                  cex = 1.2)
#'  abline(0, 1, col = "gray50", lty = 2L)
#'
#' # Get the summary data without plotting
#' cal_data <- calibration_plot(actual, predicted, show_plot = FALSE)
#' print(cal_data)
#' @returns
#' If \code{show_plot = TRUE}, the function draws a plot as a side effect and invisibly returns a data frame with the summary statistics.
#' If \code{show_plot = FALSE}, it visibly returns the data frame.
#' The returned data frame includes the following columns:
#' \item{bin}{The bin number to which the predictions were assigned.}
#' \item{n}{The number of observations in each bin.}
#' \item{actual}{The mean of the true outcomes in each bin (i.e., the fraction of positives).}
#' \item{predicted}{The mean of the predicted probabilities in each bin.}
#'
#' @export
#'
calibration_plot <- function(
  actual, predicted, breaks = seq(0, 1, by = .1), show_plot = TRUE, ...
) {
  if (is.factor(actual)) {
    actual <- as.numeric(actual != levels(actual)[[1L]])
    cli::cli_warn("{.var actual} was a factor and has been converted to a numeric vector.")
  }
  res <- data.frame(
    actual = actual, predicted = predicted,
    bin = findInterval(x = predicted, vec = breaks, rightmost.closed = TRUE,
                       left.open = FALSE, all.inside = FALSE)
  ) |>
    dplyr::group_by(.data$bin) |>
    dplyr::summarize(n = dplyr::n(),
                     actual = mean(.data$actual),
                     predicted = mean(.data$predicted)) |>
    dplyr::filter(.data$n > 0)
  if (show_plot) {
    args <- list(...)
    args[c("formula", "data")] <- NULL
    if (is.null(args$type)) args$type <- "b"
    do.call(base::plot, c(list(actual ~ predicted, data = res), args))
    invisible(res)
  } else {
    res
  }
}
