#' pca_workflow
#'
#' @description
#' Finds variables that are interesting to look into, might have impact on Y. WORKS FOR CORRELATED DATA.
#'
#' Remove features from dominating components if they have too many outliers, skews distance
#' Remove features you can't change if the goal is to impact Y, ex. height for Y = weight
#' Remove features that are boring or trivial results if goal is new knowledge
#'
#' @param data df, can contain categorical, they will be filtered away
#' @param y_name name of Y
#' @param components the amount of components to display
#' @param decimals rounding
#' @param pca_loading input index of components to analyze more, I recommend 1:n as it is most interesting
#'
#' @returns Cummulative variance for the PCA components, summary for the PCA regression, most important components in the PCA regression
#' @import dplyr
#' @export
#'
#' @examples
#' pca_workflow(iris, "Sepal.Width")
#' pca_workflow(iris, "Sepal.Width", pca_loading = 1:2)
#'
#'
pca_workflow <- function(data, y_name, components = 3, decimals = 1, pca_loading = -1) {
  stopifnot(any( y_name %in% colnames(data) ))

  pca_df <- data[, -which(y_name == colnames(data))] %>% numeric_df() %>% std_df()
  data.pca <- princomp(pca_df)
  # Inspect summary data.pca here
  print(summary(data.pca))

  if (!any(pca_loading < 0)) {
    loadings <- matrix(0, length(pca_loading)*2, ncol(pca_df)) %>% as.data.frame()

    for (i in 1:length(pca_loading) ) {
      vec <- data.pca$loadings[, pca_loading[i]][order(data.pca$loadings[, pca_loading[i]] %>%
                                                         abs(), decreasing = T)]
      loadings[2*i - 1, ] <- vec
      loadings[2*i, ] <- vec %>% names()
    }


    return(loadings)
  }

  reg_df <-  data.pca$scores %>% as.data.frame()
  reg_df <- reg_df %>% std_df()
  reg_df <- cbind(reg_df, data.frame(y_name = data[y_name]))

  model <- lm(paste0(y_name, " ~ .") %>% as.formula(), reg_df)
  print(summary(model))

  # Based on highest betas
  component_indexes <- names(model$coefficients[-1][order(model$coefficients[-1] %>% abs(), decreasing = T)])[1:components] %>% str_remove("Comp.") %>%
    as.numeric()


  component_i <- data.pca$loadings[, component_indexes[1]][order(data.pca$loadings[, component_indexes[1]] %>%
                                                                   abs(), decreasing = T)]

  frame <- c("magnitude" %>% paste("component", component_indexes[1]),
             "variable" %>% paste("component", component_indexes[1])
  )

  print_df <- cbind(component_i %>% round(decimals), names(component_i)) %>% as.data.frame()
  print_df[, 1] <- print_df[, 1] %>% as.numeric()
  colnames(print_df)[(ncol(print_df)-1):ncol(print_df)] <- frame

  for (i in 2:length(component_indexes)) {
    component_i <- data.pca$loadings[, component_indexes[i]][order(data.pca$loadings[, component_indexes[i]] %>%
                                                                     abs(), decreasing = T)]

    frame <- c("magnitude" %>% paste("component", component_indexes[i]),
               "variable" %>% paste("component", component_indexes[i])
    )

    print_df <- cbind(print_df, component_i  %>% round(decimals), names(component_i))
    colnames(print_df)[(ncol(print_df)-1):ncol(print_df)] <- frame
  }

  rownames(print_df) <- NULL

  return(print_df)

}
