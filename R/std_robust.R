#' std_robust
#'
#' @description
#' Robust standardization 1.349 * (x - Q[2]) / (Q[3] - Q[1])
#' Comparable to z_score under normality
#'
#' @param x numerical vector
#'
#' @returns Standardized values
#' @export
#'
#' @examples
#' std_robust(rnorm(10, 10, 5))
#'
#'
std_robust <- function(x) {
  # x is a vector
  Q <- quantile(x, probs = c(0.25, 0.5, 0.75))

  # 1.349 is that under normality it is comparable to a Z-score.
  1.349 * (x - Q[2]) / (Q[3] - Q[1])

}

