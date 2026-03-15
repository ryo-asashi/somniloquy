# Create Excess Plots

\`excess_plot()\` creates a mean or median excess plot as a side-effect
and returns the data frame invisibly.

## Usage

``` r
excess_plot(
  x,
  method = c("mean", "median"),
  mode = c("exact", "interpolate"),
  show_plot = TRUE,
  ...
)
```

## Arguments

- x:

  a numeric vector.

- method:

  "mean" or "median".

- mode:

  "exact" or "interpolate".

- show_plot:

  logical.

- ...:

  optional arguments to be passed to the
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) function,
  such as `main`, `xlab`, `ylab`, `col`, etc.

## Value

\`excess_plot()\` returns a data frame containing the data used for
plotting. If `show_plot` is `TRUE`, the data frame is returned
invisibly.

## Details

\`excess_plot()\` generates a mean or median excess plot, which is a key
tool for visually assessing the tail behavior of a distribution.

Mean Excess Plot (MEP) represents the function \\e(u) =
\mathrm{E}\[\\\mathrm{X} - u\mid\mathrm{X} \> u\\\]\\, i.e., the average
of all exceedances over a given threshold `u`. This plot helps in
identifying the type of tail behavior. An upward trend suggests a
heavy-tailed distribution, a flat trend indicates an exponential
distribution, and a downward trend points to a light-tailed
distribution.

Median Excess Plot is similar to the MEP, but uses the median of the
exceedances instead of the mean. This makes the plot more robust to
outliers.

## Examples

``` r
# Create a mean excess plot
excess_plot(x = rexp(100))


# Create a median excess plot without showing the plot
excess_data <- excess_plot(x = rexp(100), method = "median", show_plot = FALSE)

# Customize the plot appearance
excess_plot(x = rexp(100), col = "maroon", main = "Mean Excess Plot")
```
