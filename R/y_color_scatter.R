#' y_color_scatter
#'
#' @description
#' If Y is numeric it will color the graph based on low mid high that are based on percentiles 33 and 66.
#'
#' @param x1 numerical variable
#' @param x2 numerical variable
#' @param y numerical variable
#' @param varnames variable names
#'
#' @returns Scatter plot colored by levels of Y.
#' @import ggplot2
#' @export
#'
#'
y_color_scatter <- function(x1, x2, y, varnames = c("Var 1", "Var 2", "Y")) {
  stopifnot(length(x1) == length(x2))
  stopifnot(length(x1) == length(y))

  if (is.numeric(y)) {
    Q <- quantile(y, c(0.33, 0.66))

    temp_y <- rep("", length(y))

    temp_y[y <= Q[1]] <- "low"
    temp_y[y > Q[1] & y <= Q[2]] <- "mid"
    temp_y[y > Q[2]] <- "high"

    y <- as.factor(temp_y)
  }

  stopifnot(is.character(y) | is.factor(y))

  i <- length(varnames)
  while (length(varnames) < 3) {
    i <- i + 1
    varnames <- c(varnames, c("Var 1", "Var 2", "Y")[i])
  }


  ggplot(cbind(x1, x2, y), aes(x = x1, y = x2, color = as.factor(y))) +
    geom_point() +
    labs(
      x = varnames[1],
      y = varnames[2],
      color = varnames[3]
    ) +
    theme_bw()
}
