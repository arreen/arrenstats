#' std_z
#'
#' @description
#' Classic standardization. Dividing by n-1 in sd.
#'
#' @param x numerical vector
#'
#' @returns Standardized values
#' @export
#'
#' @examples
#' std_z(rnorm(10, 10, 5))
#'
#'
std_z <- function(x) {
  # x is a vector
  (x - mean(x)) / sd(x)
}
