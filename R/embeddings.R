#' embeddings
#'
#' @description
#' Outputs vector representations for each category in the categorical variables (character or factor varaibles).
#' This is usually 2 columns with coordinates, 0 is for unseen categories.
#' BE CAREFUL STANDARDIZING THESE CATEGORIES, YOU LOSE GEOMETRICAL MEANING, the tag TQZ in the variable names
#' is so the function std_df will handle them correctly, local mean per column but global scaling per category.
#' The neural net weighs the loss function according to rare classes 1/(c1*c2..) but winzorized and standardized.
#'
#'
#' @param data data which embeddings from embeddings() are available
#' @param embeddings embeddings object from embeddings()
#' @param y_name name for the target variable
#' @param plot plots embeddings
#' @param y_type target variable type "numeric", "binary", "categorical"
#' @param epochs max epochs
#'
#' @returns categorical variables embedded
#' @import reticulate dplyr stringr keras3 tensorflow
#' @export
#'
#' @examples
#' #output <- embeddings(train_data, val_data, y_name = "y")
#'
#'
embeddings <- function(train_data, val_data, y_name, d = 2, dropout = 0, plot = FALSE, y_type = "numeric", epochs = 50) {
  stopifnot(all(colnames(train_data) == colnames(val_data) ))
  stopifnot(y_name %in% colnames(train_data))
  stopifnot(y_type %in% c("numeric", "binary", "categorical"))

  types <- sapply(1:ncol(train_data), function(x) class(train_data[, x]) %>% str_c(collapse = "")) %>% unlist()
  character_sum <- sum(types[colnames(train_data) != y_name] == "character") + sum(types[colnames(train_data) != y_name] == "factor")

  stopifnot(character_sum != 0)

  # The tag to identify variables that are encoded, so causes issues later if variables contain this
  stopifnot( all(!str_detect(colnames(train_data), "TQZ")) )


  if (!reticulate::py_available(initialize = FALSE)) {
    reticulate::use_condaenv("tf-env", require = TRUE)
  }


  if (y_type == "numeric") {
    y_train <- train_data[y_name] %>% unlist()
    y_val <- val_data[y_name] %>% unlist()
  } else if (y_type == "binary") {
    y_train <- train_data[y_name] %>% unlist() %>% ordinal_encoder()
    y_val <- val_data[y_name] %>% unlist() %>% ordinal_encoder()
  } else {
    y_train <- train_data[y_name] %>% unlist()
    y_val <- val_data[y_name] %>% unlist()

    train_classes <- y_train %>% as.character() %>% unique()
    n_classes_y <- train_classes %>% length()
    val_classes <- y_val %>% as.character() %>% unique()

    if (!all(val_classes %in% train_classes)) {
      # we have unseen classes, remove these
      print("Removing classes in validation that are unseen in training")
      unseen <- val_classes[!(val_classes %in% train_classes)]
      row_index_unseen <- which(unlist(val_data[y_name]) %in% unseen)
      val_data <- val_data[-row_index_unseen, ]
      y_val <- y_val[-row_index_unseen]
    }

    # A-E 0-4
    class2id <- setNames(seq_along(train_classes) - 1L, train_classes)


    y_train <- class2id[y_train] %>% unname()
    y_val <- class2id[y_val]   %>% unname()


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
    val_lv   <- unique(as.character(val_data[, i]))
    extra_lv  <- val_lv[!val_lv %in% train_lv]

    # New order: common first, extras last
    new_lv <- c(extra_lv, train_lv)

    val_col <- as.character(val_data[, i])
    val_col[val_col %in% extra_lv] <- "Unseen category"

    if (sum(val_col == "Unseen category") == 0 ) {
      val_data[, i] <- val_data[, i] %>% ordinal_encoder() %>% as.integer() + 1

    } else {

      new_lv <- c("Unseen category", train_lv)
      val_data[, i] <- factor(val_col, levels = new_lv)

      val_data[, i] <- val_data[, i] %>% ordinal_encoder() %>% as.integer()
    }





  }

  for (i in which(colnames(train_data) != y_name)) {

    # +1 cuz pad unseen as 0s
    train_data[, i] <- train_data[, i] %>% ordinal_encoder() %>% as.integer() + 1

  }

  x_train <- train_data[, which(colnames(train_data) != y_name), drop = FALSE] %>% as.matrix()

  x_val <- val_data[, which(colnames(val_data) != y_name), drop = FALSE] %>% as.matrix()


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


  if (length(embeddings_list) > 1) {
    input <- layer_concatenate(embeddings_list)
  } else {
    input <- embeddings_list[[1]]
  }

  input <- input %>%
    layer_dense(max(p * d, 64), activation = "relu") %>%
    layer_dropout(rate = dropout) %>%
    layer_dense(max(round(p * d / 2), 64), activation = "relu") %>%
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

  if (y_type == "categorical") {
    output <- input %>%
      layer_dense(n_classes_y, activation = "softmax")

    model <- keras_model(inputs = inputs_list, outputs = output)

    model %>% compile(
      optimizer = optimizer_adam_w(
        learning_rate = 0.001,
        weight_decay = 0.004
      ),
      loss = "sparse_categorical_crossentropy",
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


  if (y_type == "categorical") {
    y_multiclass_mod <- 1/table(y_train)[as.character(y_train)]
    weights <- y_multiclass_mod * weights
  }




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

  history <- model |> fit(x_lis, y_train, epochs = epochs, batch_size = b_s,
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






