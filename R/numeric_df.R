#' numeric_df
#'
#' @description
#' Keeps only the numeric or integer columns of the data.frame
#'
#' @param df dataframe.
#' @param char keeps categorical instead (factor and character), defaults to FALSE.
#'
#' @returns filtered data.frame
#' @export
#'
#' @examples
#' numeric_df(iris)
#'
#'
numeric_df <- function(df, char = FALSE) {
  types <- sapply(1:ncol(df), function(x) class(df[, x]) %>% str_c(collapse = "")) %>% unlist()
  stopifnot(c("numeric", "integer", "character", "factor") %in% types %>% any())

  if (char) {
    index <- str_detect(types, "(factor)|(character)")
  } else {
    index <- str_detect(types, "(integer)|(numeric)")
  }

  return(df[, index])

}


