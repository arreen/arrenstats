#' encode_my_df
#'
#' @description
#' Takes a data_frame, uses the embeddings from embeddings() in the embeddings object.
#' Outputs vector representations for each category in the categorical variables (character or factor varaibles).
#' This is usually 2 columns with coordinates, 0 is for unseen categories.
#' BE CAREFUL STANDARDIZING THESE CATEGORIES, YOU LOSE GEOMETRICAL MEANING, the tag TQZ in the variable names
#' is so the function std_df will handle them correctly, local mean per column but global scaling per category.
#'
#'
#' @param data data which embeddings from embeddings() are available
#' @param embeddings embeddings object from embeddings()
#' @param y_name name for the target variable
#' @param plot plots embeddings
#' @param y_type target variable type "numeric", "binary", "categorical" #MULTICLASS NOT IMPLEMENTED
#'
#' @returns categorical variables embedded
#' @import reticulate dplyr stringr keras3 tensorflow
#' @export
#'
#' @examples
#' #output <- embeddings(train_data, val_data, y_name = "y")
#'
#'
library(reticulate)
#reticulate::install_miniconda(force = TRUE)
#conda_create("tf-env")
# RESTART R
use_condaenv("tf-env", require<- = TRUE)
# numpy and keras in py-config
# conda_install("tf-env", "tensorflow", pip = TRUE)
# restart R
py_config()
library(tensorflow)
library(keras3)
library(stringr)
library(dplyr)


embeddings <- function(train_data, val_data, y_name, d = 2, dropout = 0, plot = FALSE, y_type = "numeric") {
  stopifnot(all(colnames(train_data) == colnames(val_data) ))
  stopifnot(y_name %in% colnames(train_data))
  stopifnot(y_type %in% c("numeric", "binary", "categorical"))

  # The tag to identify variables that are encoded, so causes issues later if variables contain this
  stopifnot( all(!str_detect(colnames(train_data), "TQZ")) )

  if (y_type == "numeric") {
    y_train <- train_data[y_name] %>% unlist()
    y_val <- val_data[y_name] %>% unlist()
  } else {
    y_train <- train_data[y_name] %>% unlist() %>% ordinal_encoder()
    y_val <- val_data[y_name] %>% unlist() %>% ordinal_encoder()

  }


  train_data <- numeric_df(train_data, char = TRUE)
  val_data <- numeric_df(val_data, char = TRUE)

  # have to fix validation categories first, otherwise levels change
  category_levels <- list()
  for (i in which(colnames(train_data) != y_name)) {

    category_levels[[length(category_levels) + 1]] <- train_data[, i] %>% as.character() %>% unique()

  }
  names(category_levels) <- colnames(train_data)[which(colnames(train_data) != y_name)]


  for (i in which(colnames(val_data) != y_name)) {

    # It will cause issues if new categories appear in validation,
    # so we handle it by masking

    train_lv <- unique(as.character(train_data[, i]))
    val_lv   <- unique(as.character(data[, i]))
    extra_lv  <- val_lv[!val_lv %in% train_lv]

    # New order: common first, extras last
    new_lv <- c(extra_lv, train_lv)

    val_col <- as.character(val_data[, i])
    val_col[val_col %in% extra_lv] <- "OTHER"

    if (sum(val_col == "OTHER") == 0 ) {
      val_data[, i] <- val_data[, i] %>% ordinal_encoder() %>% as.integer() + 1

    } else {

      new_lv <- c("OTHER", train_lv)
      val_data[, i] <- factor(val_col, levels = new_lv)

      val_data[, i] <- val_data[, i] %>% ordinal_encoder() %>% as.integer()
    }





  }

  for (i in which(colnames(train_data) != y_name)) {

    train_data[, i] <- train_data[, i] %>% ordinal_encoder() %>% as.integer() + 1

  }


  x_train <- train_data[, which(colnames(train_data) != y_name)] %>% as.matrix()

  x_val <- val_data[, which(colnames(val_data) != y_name)] %>% as.matrix()


  # vary d for different categories? probably not too much, fix at 2, 3, 4, 5...


  # ------------------------------------
  categorical_vars <- list()

  for (nm in colnames(x_train)) {
    index <- which(nm == colnames(x_train))

    categorical_vars[[nm]] <- length(unique(x_train[, index]))
  }

  inputs_list <- list()
  embeddings_list <- list()

  for (var in names(categorical_vars)) {

    n_unique <- categorical_vars[[var]]
    input_var <- layer_input(shape = 1, name = paste0("input_", var))

    # Kaiming initialization but keeps unseen categories at 0
    # ------------------------------------------------------------
    init_matrix <- matrix(rnorm((n_unique + 1) * d, 0, sqrt(2 / n_unique)),
                          nrow = n_unique + 1, ncol = d)

    init_matrix[1, ] <- 0
    # ------------------------------------------------------------

    embedding_var <- input_var %>%
      layer_embedding(input_dim = n_unique + 1, output_dim = d, # +1 for the padding
                      embeddings_initializer = initializer_constant(init_matrix)
                      # For the unseen categories in the new data
      ) %>%
      layer_dense(d, activation = "relu") %>%
      layer_flatten()

    inputs_list <- c(inputs_list, input_var)
    embeddings_list <- c(embeddings_list, embedding_var)
  }

  p <- length(inputs_list)

  # Concatenate embeddings
  input <- layer_concatenate(embeddings_list) %>%
    layer_dense(max(p * d, 64), activation = "relu") %>%
    layer_dropout(rate = dropout) %>% layer_dense(max(round(p * d / 2), 64), activation = "relu") %>%
    layer_dropout(rate = dropout) %>%
    layer_dense(max(p * d, p * 4), activation = "relu")

  # Output

  if (y_type == "numeric") {
    output <- input %>%
      layer_dense(1)

    # Build model
    model <- keras_model(inputs = inputs_list, outputs = output)
    model %>% compile(
      optimizer = optimizer_adam_w(
        learning_rate = 0.001,
        weight_decay = 0.004  # default values
      ),
      loss = "mse"  # or "binary_crossentropy" for classification
    )
  }

  if (y_type == "binary") {
    output <- input %>%
      layer_dense(1, activation = "sigmoid")

    # Build model
    model <- keras_model(inputs = inputs_list, outputs = output)
    model %>% compile(
      optimizer = optimizer_adam_w(
        learning_rate = 0.001,
        weight_decay = 0.004  # default values
      ),
      loss = "binary_crossentropy",
      metrics = "accuracy"
    )

  }






  x_lis <- list()
  x_lis_v <- list()

  for (i in 1:ncol(x_train)) {

    x_lis[[i]] <- x_train[, i]
    x_lis_v[[i]] <- x_val[, i]
  }

  # Weights 1/(#c1*#c2...)
  # --------------------
  n_rows <- nrow(x_train)
  n_cols <- ncol(x_train)
  counts_matrix <- matrix(0, nrow = n_rows, ncol = n_cols)

  for (j in 1:n_cols) {

    current_col_values <- x_train[, j]
    freq_table <- table(current_col_values)

    counts_matrix[, j] <- as.numeric(freq_table[as.character(current_col_values)])
  }

  neg_log_matrix <- -log(counts_matrix)
  final_sums <- numeric(n_rows)

  for (i in 1:n_rows) {
    final_sums[i] <- sum(neg_log_matrix[i, ])
  }

  weights <- exp(final_sums)

  # pareto reasoning for 20, could be 0.75 or 0.9 aswell.
  # have to clip weights for gradient explosion
  Q <- quantile(weights, 0.8)
  weights[weights > Q] <- Q

  weights <- weights / mean(weights)
  # ----------------------------


  # batch size
  b_s <- min(max(min(32, nrow(x_train)), nrow(x_train)/50 %>% round()), 256)


  early_stop <- callback_early_stopping(
    monitor = "val_loss",         # Watch the validation loss
    patience = 5,                 # Wait 5 epochs after improvement stops before quitting
    restore_best_weights = TRUE,  # IMPORTANT: Go back to the best version, not the last one
    mode = "min"                  # We want loss to be minimal (use "max" for accuracy)
  )

  history <- model |> fit(x_lis, y_train, epochs = 50, batch_size = b_s,
                          validation_data = list(x_lis_v, y_val), sample_weight = weights,
                          callbacks = list(early_stop))

  # DON'T smooth, doesn't change much if n is okay large, standardize in std_df()

  embeddings_list <- list()

  for (var in 1:length(categorical_vars)) {

    embeddings_list[[var]] <- model$get_weights()[[var]]

  }

  names(embeddings_list) <- categorical_vars %>% names()


  return_val <- list(embeddings_list = embeddings_list, category_levels = category_levels, d = d)
  class(return_val) <- "embeddings"


  if (plot) {
    p(return_val)
  }

  return(return_val)

}






