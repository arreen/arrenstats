#' stage2_preprocess
#'
#' @description
#' Encoding the data via neuralnet autoencoder see emdeddings(), observations are weighted by frequency of classes and y. The data is standardized with robust, embeddingwise and zscore. Binary variables are scaled with MAD*1.48. BEWARE OF INDEX STORING FOR Y, Y IS MOVED TO THE FIRST COLUMN.
#'
#' @param train_df training data, numerical variables should be integer or numeric, categorical should be factor or character.
#' @param val_df validation set, not used for standardizing and used in the neural net to find embeddings
#' @param y_name target variable colname
#' @param plot if FALSE embedding plots are not produced, I think it is important to not hide the loss function graph
#' @param y_type type of the target variable, if "none" it is good at detecting binary and numerical, if it is a multiclass variable coded as numbers it will not be automatically detected, you can make it into a character or specify like this: "numeric", "binary", "categorical"
#' @param epochs max epochs, increase if you dont see convergence
#'
#' @returns processes train and val data. preprocess_new_data contains embeddings and scaling so new_data can be converted to the same structure.
#' @export
#'
#' @examples
#' # a <- stage2_preprocess(train_data, val_data, y_name = "fruits", y_type = "categorical")
#'
stage2_preprocess <- function(train_df, val_df, y_name, plot = TRUE, y_type = "none", epochs = 50) {
  stopifnot(all(colnames(train_df) %in% colnames(val_df)))
  stopifnot(y_name %in% colnames(train_df))

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


  types <- sapply(1:ncol(train_data), function(x) class(train_data[, x]) %>% str_c(collapse = "")) %>% unlist()
  character_sum <- sum(types[colnames(train_data) != y_name] == "character") + sum(types[colnames(train_data) != y_name] == "factor")

  if (character_sum != 0){
    embeddings <- embeddings(train_df, val_df, y_name, plot = plot, y_type = y_type, epochs = epochs)

    train_df <- encode_my_df(train_df, embeddings, y_name)
    val_df <-   encode_my_df(val_df, embeddings, y_name)

  } else {
    embeddings <-  list()
  }




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

