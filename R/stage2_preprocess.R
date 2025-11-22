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
stage2_preprocess <- function(train_df, val_df, y_name, plot = TRUE) {
  embeddings <- embeddings(train_df, val_df, y_name, plot = plot)

  train_df <- encode_my_df(train_df, embeddings, y_name)
  val_df <-   encode_my_df(val_df, embeddings, y_name)

  std_lis <- std_df(train_df, revertible = TRUE)

  train_df <- std_lis$std_df

  val_df <- std_df(train_df, original_scale = std_lis$reverse_df)

  return(list(train = train_df, val = val_df,
              preprocess_new_data = list(reverse_df = std_lis$reverse_df,
                                         embeddings = embeddings)
              )
  )
}

