#' Create Excess Plots
#'
#' @description
#' `excess_plot()` creates a mean or median excess plot as a side-effect and returns the data frame invisibly.
#'
#' @details
#' `excess_plot()` generates a mean or median excess plot, which is a key tool for visually assessing the tail behavior of a distribution.
#'
#' Mean Excess Plot (MEP) represents the function \eqn{e(u) = \mathrm{E}[\;\mathrm{X} - u\mid\mathrm{X} > u\;]}, i.e., the average of all exceedances over a given threshold \code{u}.
#' This plot helps in identifying the type of tail behavior.
#' An upward trend suggests a heavy-tailed distribution, a flat trend indicates an exponential distribution, and a downward trend points to a light-tailed distribution.
#'
#' Median Excess Plot is similar to the MEP, but uses the median of the exceedances instead of the mean. This makes the plot more robust to outliers.
#'
#' @param x a numeric vector.
#' @param method "mean" or "median".
#' @param mode "exact" or "interpolate".
#' @param show_plot logical.
#' @param ... optional arguments to be passed to the \code{plot()} function, such as \code{main}, \code{xlab}, \code{ylab}, \code{col}, etc.
#'
#' @returns
#' `excess_plot()` returns a data frame containing the data used for plotting.
#' If \code{show_plot} is \code{TRUE}, the data frame is returned invisibly.
#'
#' @examples
#' # Create a mean excess plot
#' excess_plot(x = rexp(100))
#'
#' # Create a median excess plot without showing the plot
#' excess_data <- excess_plot(x = rexp(100), method = "median", show_plot = FALSE)
#'
#' # Customize the plot appearance
#' excess_plot(x = rexp(100), col = "maroon", main = "Mean Excess Plot")
#'
#' @export
#'
excess_plot <- function(
    x, method = c("mean", "median"), mode = c("exact", "interpolate"),
    show_plot = TRUE, ...
) {
  method <- match.arg(method)
  mode <- match.arg(mode)
  sx <- sort(x, decreasing = TRUE)
  n <- length(x)
  if (method == "mean") {
    cs <- cumsum(sx)
    mx <- cs / seq_len(n)
  } else { # i.e., method == "median"
    mx <- numeric(n)
    mx[seq(1L, n, 2L)] <- sx[seq_len(n %/% 2 + n %% 2L)]
    mx[seq(2L, n, 2L)] <- (sx[seq_len(n %/% 2)] + sx[seq_len(n %/% 2) + 1L]) / 2
  }
  if (mode == "exact") {
    ys <- us <- numeric(2L * n - 1L)
    ys[seq.int(1L, 2L * n - 1L, 2L)] <- mx - sx
    ys[seq.int(2L, 2L * n - 2L, 2L)] <- mx[-n] - sx[-1L]
    us[seq.int(1L, 2L * n - 1L, 2L)] <- sx
    us[seq.int(2L, 2L * n - 2L, 2L)] <- sx[-1L]
  } else {
    ys <- us <- numeric(n)
    ys[2L:n] <- mx[-n] - sx[-1L]
    us <- sx
  }
  res <- data.frame(u = us, y = ys)
  if (show_plot) {
    args <- list(...)
    args[c("formula", "data")] <- NULL
    if (is.null(args$type)) args$type <- "l"
    do.call(base::plot, c(list(y ~ u, data = res), args))
    invisible(res)
  } else {
    res
  }
}
