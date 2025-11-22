#' std_robust
#'
#' @description
#' Robust standardization 1.349 * (x - Q[2]) / (Q[3] - Q[1])
#' Comparable to z_score under normality
#' If IQR = 0, uses MAD scaled to be comparable to z_score
#'
#' @param x numerical vector
#' @param revertbible If spread and location should be saved
#'
#' @returns Standardized values
#' @export
#'
#' @examples
#' std_robust(rnorm(10, 10, 5))
#'
#'
std_robust <- function(x, revertible = FALSE) {
  # x is a vector
  Q <- quantile(x, probs = c(0.25, 0.5, 0.75))


  mu <- Q[2]
  # 1.349 is that under normality it is comparable to a Z-score.
  spread <- (Q[3] - Q[1]) / 1.349

  if (spread == 0) {

    # MAD relation to sd under normality

    spread <- 1.4826 * mean(abs(x - mu))

  }

  if (spread == 0) {
    # all are constant, this works for the revertible transformation, spread at 0 makes the reverse not work.
    spread <- 1
  }

  if (revertible) {
    return(list(x = (x - mu) / spread, mu = mu, spread = spread))
  }


  (x - mu) / spread


}

