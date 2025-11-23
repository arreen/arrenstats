#' stage2_preprocess
#'
#' @description
#' Encoding the data via neuralnet autoencoder and standardizing with robust, embeddingwise and zscore.
#'
#' @param data dataframe column or multiple.
#'
#' @usage p(iris)
#'
#' @returns processes train and val data. preprocess_new_data contains embeddings and scaling so new_data can be converted to the same structure.
#' @export
#'
#' @examples
#'
stage2_preprocess <- function(train_df, val_df, y_name, plot = TRUE, y_type = "none") {
  stopifnot(all(colnames(train_df) %in% colnames(val_df)))

  binary_var_index <- sapply(1:ncol(train_df), function(x) train_df[, x] %>% unique() %>% length())
  if (any(binary_var_index < 2)) {
    return(paste0("check variables", colnames(train_df)[binary_var_index < 2]) %>% print())
  }

  binary_var_index <- which(binary_var_index == 2)

  binary_df <- data.frame(var_name = character(), mu = numeric(), spread = numeric())

  for (binary in binary_var_index) {

    train_df[, binary] <- train_df[, binary] %>% ordinal_encoder()

  }

  for (binary in binary_var_index) {
    val_df[, binary] <- val_df[, binary] %>% ordinal_encoder()

  }

  if (y_type == "none") {
    y_class <- train_df[y_name] %>% class()
    lvs <- train_df[y_name] %>% as.character() %>% unique() %>% length()
    if(lvs == 2){
      y_type <- "binary"
    }


    if (y_class == "factor" | y_class == "character") {
      y_type <- "categorical"
    } else {
      y_type <- "numeric"
    }

  }

  embeddings <- embeddings(train_df, val_df, y_name, plot = plot, y_type = y_type)

  train_df <- encode_my_df(train_df, embeddings, y_name)
  val_df <-   encode_my_df(val_df, embeddings, y_name)


  std_lis <- std_df(train_df, revertible = TRUE)

  train_df <- std_lis$std_df

  val_df <- std_df(val_df, standardize_new_data_like_train = std_lis$reverse_df)

  rev_df <- rbind(std_lis$reverse_df, binary_df)
  rownames(rev_df) <- NULL

  return(list(train = train_df, val = val_df,
              preprocess_new_data = list(reverse_df = rev_df,
                                         embeddings = embeddings)
              )
  )
}

# BUGGED SOMEHOW Y GETS BINARY1s THING
a <- stage2_preprocess(train_data, val_data, "binary1")


q <- a$val %>% std_df(original_scale = a$preprocess_new_data$reverse_df)
q$y[1:5]
val_data$y[1:5]

q$binary2[1:5]
val_data$binary2[1:5]
