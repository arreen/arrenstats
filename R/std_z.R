#' std_z
#'
#' @description
#' Classic standardization. Dividing by n-1 in sd.
#'
#' @param x numerical vector
#' @param revertbible If spread and location should be saved
#'
#' @returns Standardized values
#' @export
#'
#' @examples
#' std_z(rnorm(10, 10, 5))
#'
#'
std_z <- function(x, revertible = FALSE) {

  mu <- mean(x)
  spread <- sd(x)
  # x is a vector
  if (revertible) {
    return(list(x = (x - mu) / spread, mu = mu, spread = spread))
  }

  (x - mean(x)) / sd(x)
}
