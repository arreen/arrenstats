library(dplyr)
library(ggplot2)
#library(dbscan)
library(reshape2)

# -----------

# How does my program handle NA values???


#' stage1_summary
#'
#' @description
#' Initial data exploration.
#' If 7 or less variables, plots barcharts and histograms.
#' Which variables seem to have connections?
#' Do we have outliers?
#'
#'
#' @param data should be a dataframe, accepted types are character and factor for categorical. Integer and numerical for numerical variables.
#' @param decimals determines decimals for mu and spread, only relevant if you have variables on small scales.
#' @param raw_bool TRUE/FALSE, needed when you want non rounded spreads/kurtosis... for further processing
#' @param location_plots If true plot mu, spread... for all variables as a histogram
#' @param heatmaps If true returns spearman correlation (https://pubmed.ncbi.nlm.nih.gov/27213982/) for numerical variables.
#' CramerV biascorrected (https://en.wikipedia.org/wiki/Cram\%C3\%A9r\%27s_V) and using simulated p-values for robustness, is this table actually surprising?
#' @param surpress_plotting controls histograms and barcharts
#' @param plot_limit If you want to plot all histograms and barcharts for 50 variables, adjust this number
#' @param extreme_outliers If you want extreme outliers outside 5 IQR of Q1 and Q3 set this to TRUE. Default is 3 IQR.
#'
#' @usage stage1_summary(data)
#' stage1_summary(data, decimals = 1, raw_bool = FALSE, location_plots = FALSE, heatmaps = TRUE, surpress_plotting = FALSE, plot_limit = 7, extreme_outliers = FALSE)
#'
#' @returns Outlier counts, according to if within 3 IQR of 1st and 3rd quartile. Mean if n>60 otherwise raw count. The heatmap for categorical variables highlights values with low p-values.
#' @import dplyr ggplot2 reshape2
#' @export
#'
#' @examples
#' # stage1_summary(data)
#'
#' # The following is for using mu, spread ... in further analysis
#' # stage1_summary(data, raw_bool = TRUE, surpress_plotting = TRUE, heatmaps = FALSE)$raw
#'
#'
stage1_summary <- function(data, decimals = 1, raw_bool = FALSE, location_plots = FALSE,
                           heatmaps = TRUE, surpress_plotting = FALSE, plot_limit = 7,
                           extreme_outliers = FALSE) {

  types <- sapply(1:ncol(data), function(x) class(data[, x])) %>% unlist()
  numerical_index <- which(types == "numeric" | types == "integer")
  categorical_index <- which(types == "character" | types == "factor")

  var_names <- colnames(data[, numerical_index])

  if (length(numerical_index) + length(categorical_index) < (plot_limit + 1) & !surpress_plotting) {
    #Plot all histograms instead! if k < 8
    lapply(numerical_index, function(x) {
      print(
      ggplot(data, aes(x = data[, x])) +
        geom_histogram(fill = "lightblue", color = "black", bins = 30) +
        theme_bw() +
        labs(x = names(data)[x], y = "Count",
             title = paste("Histogram of", names(data)[x]))
      )
    })


    lapply(categorical_index, function(x) {
      print(
      ggplot(data, aes(x = data[, x])) +
        geom_bar(fill = "lightblue", color = "black") +
        theme_bw() +
        labs(x = names(data)[x], y = "Count",
             title = paste("Bar chart of", names(data)[x]))
      )
    })


  }

  if (heatmaps) {
    #browser()
    categorical_matrix <- diag(length(categorical_index))
    p_values_matrix <- diag(x = 0, length(categorical_index))
    rownames(categorical_matrix) <- colnames(categorical_matrix) <- colnames(data[, categorical_index])
    rownames(p_values_matrix) <- colnames(p_values_matrix) <- colnames(data[, categorical_index])

    remove_missing <- function(x, y) {
      stopifnot(length(x) == length(y))


      index <- (x == "" | y == "") | (is.na(x) | is.na(y))

      return(rbind(x[!index], y[!index]))

    }


    for (i in categorical_index) {
      for (j in categorical_index) {
        if(i > j){

          filtered <- remove_missing(data[, i], data[, j])

          x1 <- filtered[1, ]
          x2 <- filtered[2, ]

          categories_i <- x1 %>% unique() %>% length()
          categories_j <- x2 %>% unique() %>% length()

          n <- length(x1)

          # CRAMER V
          chi <- chisq.test(x = x1, y = x2)$statistic

          expected_phi <- (categories_i - 1) * (categories_j - 1) / (n-1)
          phi_bias_corrected <- max(0, chi/n - expected_phi)

          k <- categories_i - (categories_i - 1)^2 / (n-1)
          r <- categories_j - (categories_j - 1)^2 / (n-1)

          cramer_V <- sqrt(phi_bias_corrected / min(k, r))

          #cramer_V <- lsr::cramersV(x = x1, y = x2)

          a <- which(i == categorical_index)
          b <- which(j == categorical_index)
          categorical_matrix[a, b] <- categorical_matrix[b, a] <- cramer_V


          p_val <- chisq.test(x = x1, y = x2, simulate.p.value = TRUE)$p.value
          p_values_matrix[a, b] <- p_values_matrix[b, a] <- p_val



        }
      }
    }


    plot_heatmatrix <- function(corr_matrix, p_vals, legend_lab, main) {
      V_long <- melt(corr_matrix)
      p_long <- melt(p_vals)

      # Combine into one data frame
      df <- data.frame(
        Var1 = V_long$Var1,
        Var2 = V_long$Var2,
        V = V_long$value,
        p = p_long$value
      )


      # Plot heatmap: fill by V, overlay transparency or color by significance
      p <- ggplot(df, aes(x = Var1, y = Var2, fill = V)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "steelblue") +
        theme_minimal() +
        scale_y_discrete(limits = rev(levels(df$Var2))) +
        labs(fill = legend_lab, alpha = "Significant (p<0.05)",
             title = main) + theme(
               axis.title.x = element_blank(),
               axis.title.y = element_blank()
             )

      if (ncol(corr_matrix) > 5) {
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }

      if (all(p_vals < 0.05)) {
        p + geom_text(aes(label = round(V, 2), alpha = ifelse(p < 0.05, 1, 0.3)),
                      show.legend = FALSE)
      } else {
        p <- p + scale_alpha_continuous(breaks = c(0.3, 1), labels = c("0.3", "1.0")) +
          geom_text(aes(label = round(V, 2), alpha = ifelse(p < 0.05, 1, 0.3)))
        p
      }


    }



    print(plot_heatmatrix(categorical_matrix, p_values_matrix, "Cramer's V", "Categorical"))
    data[, numerical_index]
    cor_matrix <- cor(data[, numerical_index], method = "spearman")
    print(plot_heatmatrix(cor_matrix, cor_matrix - cor_matrix, expression("Spearman " * rho), "Numerical"))

  }

    skew <- function(x) {
      n <- length(x)
      x_bar <- mean(x)
      a <- sum((x-x_bar)^3)/n
      b <- (sum((x-x_bar)^2)/n) ^ (3/2)
      a/b
    }

    kurt <- function(x) {
      n <- length(x)
      x_bar <- mean(x)
      a <- sum((x-x_bar)^4)/n
      b <- (sum((x-x_bar)^2)/n) ^ 2
      return((a/b) - 3)

    }

    outlier_check_percent <- function(x) {
      n <- length(x)
      Q <- quantile(x, c(0.25, 0.75))
      IQR <- Q[2] - Q[1]
      # Index vector showing which observations are outside the expected interval

      if (extreme_outliers) {
        outlier_index <- (x > Q[1] + 5*IQR) & (x > Q[1] - 5*IQR)
      } else {
        outlier_index <- (x > Q[1] + 3*IQR) & (x > Q[1] - 3*IQR)
      }

      # Motivation: 0.16/60 %>% sqrt() sqrt(var) in bernouli, 2%
      if (n < 60) {
        # For small n we return the raw amount
        return(sum(outlier_index))
      }

      return(mean(outlier_index))

    }


    mu <- sapply(numerical_index, function(x) data[, x] %>% mean())
    spread <- sapply(numerical_index, function(x) data[, x] %>% var())

    #Skewness bias only pulls the skewness closer to 0, never across it




    skewness <- sapply(numerical_index, function(x) data[, x] %>% skew())



    #bootstrap bias correction, parallel processing?
    kurtosis <- sapply(numerical_index, function(x) data[, x] %>% kurt())



    raw <- rbind(mu, spread, skewness, kurtosis)

    gaussian <- sapply(1:ncol(raw), function(x){
      S <- raw[3, x]
      K <- raw[4, x]
      # THIS BUGS WITH NA VALUES!
      n <- nrow(data)
      JB <- (n/6)*(S^2 + (K^2)/4)
      return(JB < qchisq(0.95, 2))
    })

    outliers <- sapply(numerical_index, function(x) data[, x] %>% outlier_check_percent())

    # multimodality <- sapply(numerical_index, function(x){
    #   y <- as.matrix(data[, x])
    #   # Motivation: sqrt(n) is a common heuristic, same in kNN
    #   db <- dbscan::hdbscan(y, minPts = length(y) %>% sqrt())
    #
    #   # GMM model
    #   # k means optimize for finding k
    #
    #   Kernel density estimation density()
    #
    #
    #   # cluster amount
    #   return(db$cluster %>% unique() %>% length() - 1)
    # })




    raw <- rbind(raw, gaussian, outliers) #, multimodality)

    colnames(raw) <- var_names





    if (location_plots) {

      # Why do we even want these plots???
      hist(mu)
      hist(spread)

      print('ref_kurtosis <- c(Uni = -1.2, Normal = 0, Exp = 6, Laplace = 3, Cauchy = Inf, (Gamma a=1) = 6)' )
      hist(skewness)
      hist(kurtosis)

    }


    # Clean the output maybe!
    processed <- rbind(raw[1:2, ] %>% round(decimals), raw[3:4, ] %>% round(2), raw[5, ], raw[6, ] %>% round(2))
    rownames(processed) <- rownames(raw)

    if (raw_bool) {
      return(list("raw" = raw, "processed" = processed))
    }


    # Recommend if the variable is to be normalized by x-median/IQR or x-mu/sigma Given the skew + kurtosis,
    #high kurtosis or high skew absolute value
    print("Variables not listed as gaussian might benefit from standardization using IQR")
    print("The gaussian flagging is not reliable for all distributions, search JB test")


    return(processed)


}




# na value check

# categorical vs numerical?

# PCA ?



