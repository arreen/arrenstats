library(dplyr)
library(ggplot2)
library(dbscan)
# -----------

# How does my program handle NA values???

data <- volcano

stage1_summary <- function(data, decimals = 1, raw_bool = FALSE, plots = FALSE) {

  types <- sapply(1:ncol(data), function(x) class(data[, x])) %>% unlist()
  numerical_index <- which(types == "numeric")
  k <- length(numerical_index)
  var_names <- colnames(data[, numerical_index])


  if (k > 8) {

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
      outlier_index <- (x > Q[1] + 3*IQR) & (x > Q[1] - 3*IQR)

      # Motivation: 0.16/60 %>% sqrt() sqrt(var) in bernouli, 2%
      if (n < 60) {
        # For small n we return the raw amount
        return(sum((x > Q[1] + 3*IQR) & (x > Q[1] - 3*IQR)))
      }

      return(mean((x > Q[1] + 3*IQR) & (x > Q[1] - 3*IQR)))

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

    multimodality <- sapply(numerical_index, function(x){
      y <- as.matrix(data[, x])
      # Motivation: sqrt(n) is a common heuristic, same in kNN
      db <- dbscan::hdbscan(y, minPts = length(y) %>% sqrt())

      # GMM model
      # k means optimize for finding k

      Kernel density estimation density()


      # cluster amount
      return(db$cluster %>% unique() %>% length() - 1)
    })




    raw <- rbind(raw, gaussian, outliers, multimodality)

    colnames(raw) <- var_names





    if (plots) {

      # Why do we even want these plots???
      hist(mu)
      hist(spread)

      print('ref_kurtosis <- c(Uni = -1.2, Normal = 0, Exp = 6, Laplace = 3, Cauchy = Inf, (Gamma a=1) = 6)' )
      hist(skewness)
      hist(kurtosis)

    }


    # Clean the output maybe!
    processed <- rbind(raw[1:2, ] %>% round(decimals), raw[3:4, ] %>% round(2), raw[5, ], raw[6, ] %>% round(2), raw[7, ])
    rownames(processed) <- rownames(raw)

    if (raw_bool) {
      return(list("raw" = raw, "processed" = processed))
    }


    # Recommend if the variable is to be normalized by x-median/IQR or x-mu/sigma Given the skew + kurtosis,
    #high kurtosis or high skew absolute value
    print("Variables not listed as gaussian might benefit from standardization using IQR")
    print("The flagging is not reliable for XYZ search JB test")


    return(processed)

  } else {
    #Plot all histograms instead!
    sapply(numerical_index, function(x) data[, x] %>% hist())
  }
}



Simulation study, test this on small sample size for different distributions!
n <- 100
p <- 15

X <- matrix(rnorm(n * p, mean = 0, sd = 1), nrow = n, ncol = p)
colnames(X) <- paste0("V", 1:p)

stage1_summary(X, plots = TRUE)

Multimodal is not working, try for different n

´´

# Create 2 standardization functions!


# cross variables checks

# Unsupervised clustering checks

x <- c(rnorm(20, 3), rnorm(20, 8), rnorm(20, 15))
hist(x, breaks = 50)
plot(density(x))




# Cluster Y
# Idea is someone cant be a complete beginner but after a certain point to find patterns you need variation.
# then train the NN on each cluster, then weigh them together.
# HOW DO YOU WEIGH NEURONS TOGETHER ASSUMING SAME STRUCTURE OF NN

# After this each epoch is one of the clusters.
# Add more noise to the X after each epoch, noise schedule.

# Graph the training error in each cluster.


