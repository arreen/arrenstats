#' stage1_variable_connections
#'
#' @description Trains a bayesian network with hill climber. All numerical variables are binned into above and below median.
#'
#' @param train_data your training data
#' @param y_name returns the markov blanket for this variable
#' @param restart number of initializations for the hill climber optimization
#' @param feature_cap stops the function if you exceed this many features
#'
#' @returns returns markov blanket, which features impact y, other features impact
#' the distribution of features in Ys markov blanket, but
#' once you observe the markov blanket you don't gain information from the other variables.
#' @import dplyr bnlearn bnviewer
#' @export
#'
#'
#'
stage1_variable_connections <- function(train_data, y_name, restart = 20, feature_cap = 100) {
  stopifnot(y_name %in% colnames(train_data))

  p <- ncol(train_data)

  if (p > feature_cap) {
   return("Too many features")
  }


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




    score_cache <- scoreparameters("bdecat", data = bn_df)
    res <- orderMCMC(score_cache)

    map_matrix <- getDAG(res)

    y_idx <- which(rownames(map_matrix) == y_name)

    # Markov blanket: parents, children, co-parents (other parents of children)
    parents <- which(map_matrix[, y_idx] == 1)
    children <- which(map_matrix[y_idx, ] == 1)
    co_parents <- unique(unlist(lapply(children, function(c) which(map_matrix[, c] == 1))))
    co_parents <- setdiff(co_parents, y_idx)  # remove y itself

    mb_nodes <- sort(unique(c(parents, children, co_parents)))

    mb_vars <- rownames(map_matrix)[mb_nodes]

    nodes <- data.frame(id = rownames(map_matrix) %>% seq_along(), label = rownames(map_matrix))

    map_matrix <- apply(map_matrix, 2, as.numeric)


    edges <- data.frame(
      from = which(map_matrix == 1, arr.ind = TRUE)[,1],
      to   = which(map_matrix == 1, arr.ind = TRUE)[,2]
    )

    visNetwork(nodes, edges) %>% visEdges(arrows = 'from', scaling = list(min = 2, max = 2)) %>% print()


    cat("Given there are no feedback loops in the data (can be solved with DBN) and the markov assumption is reasonable,", "\n", "the following variables are the ones that are important to Y:")
    return(mb_vars)



}


