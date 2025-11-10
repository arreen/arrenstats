#' confusion_matrix
#'
#' @description
#' Colors the confusion matrix by the off-diagonal deviation from 0 (scaled by rowsum). This makes the most important mistakes pop out.
#'
#' @param y actual classes
#' @param y_hat predicted classes
#' @param decimals number of decimals to display
#'
#' @returns confusion matrix
#' @import dplyr reshape2 ggplot2
#' @export
#'
#'
#'
confusion_matrix <- function(y, y_hat, decimals = 2) {

  cm <- table(Number = y, Predict = y_hat)

  C <- nrow(cm)
  # I wanna do relative coloring, based on deviation
  rowsums <- sapply(1:C, function(x) sum(cm[x, ]))
  perfect_pred <- diag(rowsums)

  diff <- cm - perfect_pred

  # If one row has more examples than another we need to scale to see devation
  for (i in 1:nrow(diff)) {
    diff[i, ] <- diff[i, ] / rowsums[i]

  }

  # I dont care about the diagonal, the most interesting is that the errors pop out
  diff[diff < 0] <- 0


  cm_long <- melt(cm)
  cm_long$Number <- cm_long$Number[nrow(cm_long):1] %>% as.factor()
  cm_long$Predict <- cm_long$Predict %>% as.factor()

  cm_long$color <- melt(diff)$value



  ggplot(cm_long, aes(x = Predict, y = Number, fill = color)) +
    geom_tile(color = "black") +
    geom_text(aes(label = value), color = "black", size = 5) +
    scale_fill_gradient(low = "white", high = "#3182bd", guide = "none") +
    theme_minimal(base_size = 25) +
    scale_y_discrete(labels = levels(cm_long$Number) %>% rev() ) +
    labs(x = "Predicted", y = "Actual", title = paste0("Overall accuracy = ",
                                                       mean(train$y == y_hat_t) %>%
                                                         round(decimals)) ) +
    theme(
      axis.text.x = element_text(hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid = element_blank(),
    )
}


