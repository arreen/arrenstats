#' plot_embedding
#'
#' @description
#' Plots the embeddings for a categorical variable
#'
#' @param name name of the variale to plot. can be found via output$embeddings_list %>% names()
#' @param embeddings embeddings object from embeddings()
#'
#' @returns A plot where clusters of similarity between levels in the category can be visualized
#' @import dplyr ggplot2
#' @export
#'
#' @examples
#' #embeddings <- embeddings(train_data, val_data, y_name = "y")
#' #plot_embedding("C1", output)
#'
plot_embedding <- function(name, embeddings) {

  emb <- embeddings$embeddings_list[[name]]
  labs <- embeddings$category_levels[[name]]

  # Remove padding row (row 1), which represents unseen category
  emb_no_pad <- emb[-1, , drop = FALSE]

  # Check dimension match
  if (nrow(emb_no_pad) != length(labs)) {
    stop(paste("Number of rows in embedding does not match category levels for", name))
  }



  # Build df for ggplot
  df <- tibble(
    x = emb_no_pad[, 1],
    y = emb_no_pad[, 2],
    category = factor(labs, levels = labs)
  )


  if (length(labs) > 6) {
    print(ggplot(df, aes(x, y, color = category)) +
            geom_point(size = 3) +
            labs(
              title = name,
              x = "Embedding dim 1",
              y = "Embedding dim 2",
              color = "Category"
            ) +
            theme_bw())

    return(invisible(NULL))
  }

  print(ggplot(df, aes(x, y, shape = category)) +
    geom_point(size = 3) +
    labs(
      title = name,
      x = "Embedding dim 1",
      y = "Embedding dim 2",
      shape = "Category"
    ) +
    theme_bw())

    return(invisible(NULL))
}
