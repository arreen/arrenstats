#' importance_permutation
#'
#' @description
#' Determines the contribution different features have for a model's performance given a loss function.
#' This is done by randomizing one feature at a time, the more the model's predictions worsen the more important the variable was.
#' It is interesting to look in the test-set as well, importance scores there reflect generalizable patterns. Importance score in the training set can reflect overreliance on a variables noise rather than causal patterns.
#'
#' @param model needs to be an object with a built in predict function
#' @param train_df data.frame used to train the model
#' @param y_name name of the y_variable
#' @param loss loss function to use, defaults to squared error
#' @param interaction T/F interpret these like this: positive means correlation as the signal is broken and can't be masked. Negative if interaction as (v1 + v2 + v1v2) - (v1 + v1v2 + v2 + v1v2) = -v1v2. 0 if independent.
#' @param seed seed for repeatable importance scores
#'
#' @usage importance_permutation(model, data, "price_range", loss = function(y, y_hat){ return(as.numeric(y != y_hat))}, interaction = T)
#'
#' @returns Importance scores for the variables, heatmap if interaction is set to TRUE, recommended but computationally expensive
#' @import RColorBrewer ggplot2 reshape2
#' @export
#'
#'
#'
#'
importance_permutation <- function(model, train_df, y_name, loss = function(y, y_hat) (y-y_hat)^2, interaction = FALSE, seed = 12345) {





  if (is.character(loss)) {
    # Preset loss functions
  }

  y <- tryCatch(train_df[y_name][, 1], error = function(e) stop("y_name not found in train_df"))

  set.seed(seed)
  seed_list <- runif(ncol(train_df) - 1)


  y_hat <- predict(model, train_df)

  n <- nrow(train_df)

  baseline_cost <- mean(loss(y, y_hat))

  variables <- colnames(train_df)[colnames(train_df) != y_name]

  importance_df <- data.frame("Variable" = variables, "Importance" = 0)




  for (i in 1:length(variables)) {
    var <- variables[i]
    temp_df <- train_df

    set.seed(seed_list[i])
    temp_df[var] <- sample(temp_df[var][, 1], n)

    y_hat <- predict(model, temp_df)
    cost <- mean(loss(y, y_hat))

    # Positive score means higher importance, negative means it hurt the model
    importance_df[i, 2] <- cost - baseline_cost

  }


  if (interaction) {
    interaction_matrix <- matrix(0, length(variables), length(variables))
    colnames(interaction_matrix) <- rownames(interaction_matrix) <- variables

    for (i in 1:length(variables)) {
      var1 <- variables[i]
      for (j in 1:length(variables)) {
        if (i != j) {


          var2 <- variables[j]
          temp_df <- train_df

          # Same samples as individual variables
          set.seed(seed_list[i])
          temp_df[var1] <- sample(temp_df[var1][, 1], n)

          set.seed(seed_list[j])
          temp_df[var2] <- sample(temp_df[var2][, 1], n)

          y_hat <- predict(model, temp_df)
          delta_cost <- mean(loss(y, y_hat)) - baseline_cost
          individual_costs <- importance_df[i, 2] + importance_df[j, 2]

          # Normalizing by cost
          interaction_matrix[i, j] <- interaction_matrix[j, i] <- (delta_cost - individual_costs) / baseline_cost
        }




      }
    }

  plot_heatmatrix <- function(corr_matrix, main = "Importance interactions, positive is masked signals, negative is interaction") {
    V_long <- melt(corr_matrix)

    df <- data.frame(
      Var1 = V_long$Var1,
      Var2 = V_long$Var2,
      V = V_long$value
    )


    # Plot heatmap: fill by V, overlay transparency or color by significance
    p <- ggplot(df, aes(x = Var1, y = Var2, fill = V)) +
      geom_tile(color = "black") +
      scale_fill_gradient2(
        low = "steelblue",      # negative values
        mid = "white",     # zero
        high = "orange",   # positive values
        midpoint = 0,
        na.value = "grey50"
      ) +
      theme_minimal() +
      scale_y_discrete(limits = rev(levels(df$Var2))) +
      labs(title = main) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold")
      )

    if (ncol(corr_matrix) > 5) {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    return(p)

  }

  plot_heatmatrix <- function(corr_matrix, main = "Importance interactions \n + is masked signals \n - is interaction") {
  V_long <- melt(corr_matrix)

  df <- data.frame(
    Var1 = V_long$Var1,
    Var2 = V_long$Var2,
    V = V_long$value
  )


  # Plot heatmap: fill by V, overlay transparency or color by significance
  p <- ggplot(df, aes(x = Var1, y = Var2, fill = V)) +
    geom_tile(color = "lightgrey") +
    scale_fill_gradient2(
      low = "blue",      # negative values
      mid = "white",     # zero
      high = "orange",   # positive values
      midpoint = 0,
      na.value = "grey50"
    ) +
    theme_minimal() +
    scale_y_discrete(limits = rev(levels(df$Var2))) +
    labs(title = main, fill = "") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )

  if (ncol(corr_matrix) > 5) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(p)

  }

  p <- plot_heatmatrix(interaction_matrix)
  print(p)
  return(importance_df[order(importance_df$Importance, decreasing = TRUE), ])
  }

  return(importance_df[order(importance_df$Importance, decreasing = TRUE), ])

}









