#' Create a customized histogram
#'
#' Creates a histogram with customizable title and color.
#'
#' @param x A numeric vector containing the data to plot.
#' @param title The title of the histogram.
#' @param color The color used for the histogram bars.
#'
#' @return A ggplot object containing the histogram.
#' @export
#'
#' @examples
#' my_histogram(mtcars$mpg)
#' my_histogram(mtcars$hp, "Horsepower", "orange")
my_histogram <- function(x, title = "Distribution of Values", color = "skyblue") {
  library(ggplot2)

  ggplot(data.frame(x = x), aes(x = x)) +
    geom_histogram(bins = 20, fill = color, color = "white") +
    labs(
      title = title,
      x = "Value",
      y = "Frequency"
    ) +
    theme_minimal()
}
