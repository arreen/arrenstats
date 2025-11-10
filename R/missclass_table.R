#' missclass_table
#'
#' @description
#' Returns prescision and recall
#'
#' @param y actual classes
#' @param y_hat predicted classes
#'
#' @returns missclass_table
#' @import dplyr
#' @export
#'
#'
#'
missclass_table <- function(y, y_hat) {

  cm <- table(Number = y, Predict = y_hat)
  FN <- sapply(1:nrow(cm), function(x) (sum(cm[x, ]) - cm[x, x]))
  TP <- diag(cm)
  FP <- sapply(1:ncol(cm), function(x) (sum(cm[, x]) - cm[x, x]))

  # when predicting class X how many times was right?
  Prescision <- TP / (TP + FP)

  # when the class was X how often was X predicted?
  Recall <- TP / (TP + FN)

  miss_table <- rbind(Prescision, Recall) %>% round(2)

  print("Prescision: when predicting class X how many times was right?")
  print("Recall: when the class was X how often was X predicted?")
  return(miss_table)

}
