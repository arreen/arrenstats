#' p
#'
#' @description
#' Fast plotting for dataframe
#'
#' @param data dataframe column or multiple.
#'
#' @usage p(iris)
#'
#' @returns A plot.
#' @import ggplot2 dplyr
#' @export
#'
#' @examples
#' p(iris[, 2])
#' p(iris[, 1:3])
#'
#' #p(embeddings)
#'

p <- function(data) {

  if (class(data) == "embeddings") {

    my_vars <- data$embeddings_list %>% names()

    for (i in my_vars) {
      plot_embedding(i, data)
    }


    return(invisible(NULL))



  }



  if (is.data.frame(data) | is.matrix(data)) {
    types <- sapply(1:ncol(data), function(x) class(data[, x])) %>% unlist()
    numerical_index <- which(types == "numeric" | types == "integer")
    categorical_index <- which(types == "character" | types == "factor")

    invisible(
    lapply(numerical_index, function(x) {
      print(
        ggplot(data, aes(x = data[, x])) +
          geom_histogram(fill = "lightblue", color = "black", bins = 30) +
          theme_bw() +
          labs(x = names(data)[x], y = "Count",
               title = paste("Histogram of", names(data)[x]))
      )
    })
    )

    invisible(
    lapply(categorical_index, function(x) {
      print(
        ggplot(data, aes(x = data[, x])) +
          geom_bar(fill = "lightblue", color = "black") +
          theme_bw() +
          labs(x = names(data)[x], y = "Count",
               title = paste("Bar chart of", names(data)[x]))
      )
    })
    )

  } else {

    if (is.null(names(data))) {
      name <- "variable"
    } else {
      name <- names(data)
    }
    if (class(data) %in% c("numeric", "integer")) {


      ggplot(data %>% as.data.frame(), aes(x = data)) +
        geom_histogram(fill = "lightblue", color = "black", bins = 30) +
        theme_bw() +
        labs(x = name, y = "Count",
             title = paste("Histogram of", name))
    } else {

      ggplot(data %>% as.data.frame(), aes(x = data)) +
        geom_bar(fill = "lightblue", color = "black") +
        theme_bw() +
        labs(x = name, y = "Count",
             title = paste("Bar chart of", name))
    }


  }

}



