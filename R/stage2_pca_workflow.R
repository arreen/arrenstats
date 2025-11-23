#' stage2_pca_workflow
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
#' @param rows_to_return amount of rows to return
#' @param y_type type of the target variable, if "none" it is good at detecting binary and numerical, if it is a multiclass variable coded as numbers it will not be automatically detected, you can make it into a character or specify like this: "numeric", "binary", "categorical"
#'
#' @returns Cummulative variance for the PCA components, summary for the PCA regression, most important components in the PCA regression
#' @import dplyr VGAM
#' @export
#'
#' @examples
#' stage2_pca_workflow(iris, "Sepal.Width")
#' stage2_pca_workflow(iris, "Sepal.Width", pca_loading = 1:2)
#'
#'
stage2_pca_workflow <- function(data, y_name, components = 3, decimals = 1, pca_loading = -1,
                                rows_to_return = 4, y_type = "none") {

  stopifnot(any( y_name %in% colnames(data) ))
  stopifnot(y_type %in% c("none", "numeric", "binary", "categorical"))


  if (y_type == "none") {
    y_class <- data[y_name] %>% class()
    lvs <- data[y_name] %>% as.character() %>% unique() %>% length()
    if(lvs == 2){
      y_type <- "binary"
    }


    if (y_class == "factor" | y_class == "character") {
      y_type <- "categorical"
    } else {
      y_type <- "numeric"
    }

  }


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

  form <- paste0(y_name, " ~ .") %>% as.formula()

  if (y_type == "numeric") {
    model <- lm(form, reg_df)
  } else if (y_type == "binary") {
    reg_df[[y_name]] <- factor(reg_df[[y_name]])
    model <- glm(form, data = reg_df, family = binomial(link = "logit"))
  } else {
    reg_df[[y_name]] <- factor(reg_df[[y_name]])

    options(vgam.verbose = FALSE)
    suppressWarnings(    model <- vglm(form,
                                       family = multinomial,
                                       data = reg_df))


  }


  print(summary(model))



  if (y_type == "categorical") {
    intercept_index <- names(model@coefficients) %>% str_detect("Intercept") %>% which()
    component_indexes <- names(model@coefficients[-intercept_index][order(model@coefficients[-intercept_index] %>% abs(), decreasing = T)])[1:components] %>% str_remove("Comp.") %>%
      str_remove("\\:[0-9]") %>%
      as.numeric() %>% unique()
  } else {
    # Based on highest betas
    component_indexes <- names(model$coefficients[-1][order(model$coefficients[-1] %>% abs(), decreasing = T)])[1:components] %>% str_remove("Comp.") %>%
      as.numeric()
  }


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

  print("Variables in the PCs that linearly impact Y the most")
  return(print_df %>% head(n = rows_to_return))

}
