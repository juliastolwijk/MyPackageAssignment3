#' Find outliers in a numeric vector
#'
#' Identifies values that fall below or above the standard
#' 1.5 times the interquartile range rule.
#'
#' @param x A numeric vector.
#'
#' @return A list containing the outliers, lower limit, and upper limit.
#' @export
#'
#' @examples
#' find_outliers(mtcars$mpg)
find_outliers <- function(x) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1

  lower_limit <- q1 - 1.5 * iqr
  upper_limit <- q3 + 1.5 * iqr

  outliers <- x[x < lower_limit | x > upper_limit]

  list(
    outliers = outliers,
    lower_limit = lower_limit,
    upper_limit = upper_limit
  )
}
