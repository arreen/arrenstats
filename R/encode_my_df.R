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
#'
#' @returns categorical variables embedded
#' @import dplyr
#' @export
#'
#' @examples
#'
encode_my_df <- function(data, embeddings, y_name) {


  stopifnot(class(embeddings) == "embeddings")

  category_levels <- embeddings$category_levels
  embeddings_list <- embeddings$embeddings_list
  d <- embeddings$d

  stopifnot(all(embeddings_list %>% names() %in% colnames(data)))
  stopifnot(y_name %in% colnames(data))


  y <- data[y_name] %>% unlist()
  numeric_data <- numeric_df(data)
  numeric_data <- numeric_data[colnames(numeric_data) != y_name]


  data <- numeric_df(data, char = TRUE)
  data <- data[colnames(data) != y_name]

  make_zero_embeds_df <- function(df, k = d, tag = "TQZ") {
    out <- as.data.frame(matrix(0, nrow = nrow(df), ncol = 1))

    for (i in 1:ncol(df)) {

      x <- df[, i]
      colname <- colnames(df)[i]

      new_names <- paste0(colname, "_", seq(k), "_", tag)

      zero_block <- as.data.frame(matrix(0, nrow = nrow(df), ncol = k))
      names(zero_block) <- new_names

      out <- cbind(out, zero_block)

    }

    out[, -1]
  }

  encoded <- make_zero_embeds_df(data)

  for (i in 1:ncol(data)) {

    # It will cause issues if new categories appear in validation,
    # so we handle it by masking

    train_lv <- category_levels[[i]]
    lv   <- unique(as.character(data[, i]))
    extra_lv  <- lv[!lv %in% train_lv]

    # New order: common first, extras last
    new_lv <- c(extra_lv, train_lv)


    col <- as.character(data[, i])
    col[col %in% extra_lv] <- "Unseen category"


    if (sum(col == "Unseen category") == 0 ) {
      data[, i] <- data[, i] %>% ordinal_encoder() %>% as.integer() + 1

    } else {

      new_lv <- c("Unseen category", train_lv)
      data[, i] <- factor(col, levels = new_lv)

      data[, i] <- data[, i] %>% ordinal_encoder() %>% as.integer()
    }





  }

  emb_len <- length(embeddings_list)
  for (i in 1:nrow(data)) {
    encoded_values <- numeric(emb_len * d)
    pos <- 1

    # now it is python indexing, so do +1
    for (j in 1:emb_len) {
      encoded_values[pos:(pos + d - 1)] <- embeddings_list[[j]][(data[i, j] + 1), ]
      pos <- pos + d

    }

    encoded[i, ] <- encoded_values
  }


  if (length(numeric_data) == 0) {
    return_df <- cbind(y, encoded)
    colnames(return_df) <- c(y_name, colnames(encoded))

    return(return_df)
  }

  return_df <- cbind(y, encoded, numeric_data)
  colnames(return_df) <- c(y_name, colnames(encoded), colnames(numeric_data))
  return(return_df)

}
