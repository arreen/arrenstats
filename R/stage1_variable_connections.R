#' stage1_variable_connections
#'
#' @description Trains a bayesian network with hill climber. All numerical variables are binned into above and below median.
#'
#' @param train_data your training data
#' @param y_name returns the markov blanket for this variable
#'
#' @returns returns markov blanket, which features impact y, other features impact
#' the distribution of features in Ys markov blanket, but
#' once you observe the markov blanket you don't gain information from the other variables.
#' @import dplyr bnlearn bnviewer
#' @export
#'
#'
#'
stage1_variable_connections <- function(train_data, y_name) {
  stopifnot(y_name %in% colnames(train_data))

  bn_df <- train_data

  for (i in 1:ncol(bn_df)) {
    type <- bn_df[, i] %>% class()

    if (type == "character") {
      bn_df[, i] <- bn_df[, i] %>% as.factor()
    }
    if (type == "numeric" | type == "integer") {
      # 1/0 encoded
      if ((bn_df[, i] %>% unique() %>% length()) == 2) {
        bn_df[, i] <- bn_df[, i] %>% as.factor()
      } else {
        mu <- bn_df[, i] %>% median()
        bn_df[, i] <- ifelse(bn_df[, i] > mu, "high", "low") %>% as.factor()
      }
    }

  }

  res <- hc(bn_df)

  viewer(res,
         bayesianNetwork.width = "100%",
         bayesianNetwork.height = "800px",
         bayesianNetwork.layout = "layout_with_fr") %>% print()

  cat("Given there are no feedback loops in the data (can be solved with DBN) and the markov assumption is reasonable,", "\n", "the following variables are the ones that are important to Y:")
  return(mb(res, y_name))
}


