SVM into trees
SVM high/low

implement after:
multiclass: skip the svm for the first one, we have vector based trees, they predict k gradients per observation.

library(e1071)

# Problematic for rare classes

function arguments: y_name cost = 1
cost <- 0.01
y_name <- "stroke"
y_type <- "binary"
data <- t_std

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








if (y_type == "numeric") {
  mu <- median(data[[y_name]])

  binned_y <- ifelse(data[[y_name]] > mu, "high", "low") %>% as.factor()

  form <- as.formula(paste0("binned_y", " ~ ."))
  model <- svm(form, data = cbind(data[, colnames(data) != y_name], binned_y), kernel = "linear",
               cost = cost, scale = FALSE)



} else if (y_type == "binary"){
  temp_df <- data
  temp_df[[y_name]] <- as.factor(temp_df[[y_name]])
  form <- as.formula(paste0(y_name, " ~ ."))
  model <- svm(form, data = temp_df, kernel = "linear",
               cost = cost, scale = FALSE)
  rm(temp_df)
}

#first split is SVM
# WHICH LEAF DOES IT BELONG TO
leaf_index <- model$fitted %>% ordinal_encoder() - 1
leaf_index[leaf_index == 0] <- 1




split_fun <- function(x, y, y_type = "numeric") {
  stopifnot(y_type %in% c("none", "numeric", "binary", "categorical"))
  stopifnot(length(x) == length(y))



  possible_splits <- function(x) {
    xs <- sort(unique(x))
    if (length(xs) <= 1) return(numeric(0))

    splits <- (xs[-1] + xs[-length(xs)]) / 2
    return(splits)
  }

  split_candidates <- possible_splits(x)

  #Implement running sum
  # store sum yi^2 and sum yi

  if (y_type == "numeric") {

    SSE <- function(y) sum((y - mean(y))^2)

    #rolling_y1 <- 0
    #rollingy_2

    SSE_vec <- c()

    for (i in 1:length(split_candidates)) {
      left_index <- x < split_candidates[i]

      SSE_vec <- c(SSE_vec, SSE(y[left_index]) + SSE(y[!left_index]))

    }

    best_split <- split_candidates[which.min(SSE_vec)]
    impurity_index <- which.min(SSE_vec)



  } else if (y_type == "binary") {

  } else {

  }

  return(list(best_split = best_split, impurity_index = impurity_index))


}



features <- which(colnames(data) != y_name)

gain <- c()

# This doesnt handle SVM well yet
nodes <- list()
nodes[[1]] <- list(
  is_leaf    = TRUE,
  feature    = NA,
  threshold  = NA,
  left_id    = NA,
  right_id   = NA,
  prediction = NA   # will be filled once you compute Y-mean for the root
)


for (leaf in unique(leaf_index)) {

  temp_df <- data[leaf == leaf_index, ]

  if (nrow(temp_df) > 1) {

    for (var in features) {


      # Sort and then compute candidate split for each vlaue, rolling O(nlogn)
      split_lis <- split_fun(x, y)


      gain <- c(gain, split_lis$impurity_index)

      nodes[[leaf_id]] <- list(
        is_leaf   = FALSE,
        feature   = var,
        threshold = split_lis$best_split,
        left_id   = new_left_id,
        right_id  = new_right_id,
        prediction = NA
      )

      nodes[[new_left_id]]  <- list(is_leaf=TRUE, feature=NA, threshold=NA,
                                    left_id=NA, right_id=NA,
                                    prediction = mean(Y[left_indices]))
      nodes[[new_right_id]] <- list(is_leaf=TRUE, feature=NA, threshold=NA,
                                    left_id=NA, right_id=NA,
                                    prediction = mean(Y[right_indices]))




  } else {
    #EDGE CASE HANDLING
  }
  }


}

list()


# negative indexes and positive correspond

return(svm model, prediction rule)


# Need a check for splitting leaves with only 1 obs, set best_split to Inf or something like that
# WE NEED TO DEAL WItH rare SVM categories, binary stroke classifies everything as no stroke

model$fitted
predict(model, t_std)

Build a tree to 7 branches.




