#' corr_Y
#'
#' @description
#' Computes correlation between Y and other variable and orders by magnitude
#'
#' @param df dataframe
#' @param var_name name of variable to get correlation against all other
#'
#' @returns correlation vector
#' @export
#'
#' @examples
#' corr_Y(iris, "Sepal.Width")
#'
#'
corr_Y <- function(df, var_name) {

  num_df <- df[sapply(df, is.numeric)]

  if (!var_name %in% colnames(num_df)) stop("Variable not found or not numeric")

  cors <- sapply(num_df, function(col) cor(num_df[[var_name]], col, use = "pairwise.complete.obs", method = "pearson"))

  cors <- cors[names(cors) != var_name]

  cors[order(-abs(cors))]
}

