#' ordinal_encoder
#'
#' @description
#' Ordinal encoding
#'
#' @param factor categorical variable
#'
#' @returns ordinal encoding, python + keras friendly index
#' @export
#'
#' @examples
#' ordinal_encoder(c(letters[1:3], letters[2:4]))
#'
#'
ordinal_encoder <- function(factor) {
  stopifnot(is.character(factor) | is.factor(factor) | is.numeric(factor))


  if (!is.factor(factor)) {
    factor <- as.factor(factor)
  }

  return(as.integer(factor) - 1)
}
