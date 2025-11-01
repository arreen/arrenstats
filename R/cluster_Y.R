# Unsupervised Y clustering

set.seed(123)
n <- 100

# 1. Normal
x_normal <- rnorm(n, 0, 1)

# 2. Skewed (Exponential)
x_skewed <- rexp(n, 1/2)

# 3. Heavy-tailed (Student’s t)
x_heavy <- rt(n, df = 2)

# 4. Bimodal (well-separated)
x_bimodal <- c(rnorm(n/2, -2, 0.4), rnorm(n/2, 2, 0.4))

# 5. Trimodal (moderately overlapping)
x_trimodal <- c(rnorm(n/3, -3, 0.5),
                rnorm(n/3, 0, 0.8),
                rnorm(n/3 + 1, 2.5, 0.6))

# 6. Clustered around random centers (nonparametric clusters)
k <- 4
centers <- runif(k, -5, 5)
x_clusters <- unlist(lapply(centers, function(mu) rnorm(n/k, mu, sd = runif(1, 0.1, 1))))

# 7. Uniform background with dense cluster (hard case)
x_hard <- c(runif(n * 0.7, -5, 5), rnorm(n * 0.3, 0, 0.3))

# Combine into one dataframe
data <- data.frame(
  value = c(x_normal, x_skewed, x_heavy, x_bimodal, x_trimodal, x_clusters, x_hard),
  type = rep(c("Normal", "Skewed", "Heavy-tailed", "Bimodal", "Trimodal",
               "Random clusters", "Uniform + dense cluster"),
             each = n)
)

# Visualize
library(ggplot2)
ggplot(data, aes(x = value, fill = type)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~type, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(title = "Synthetic 1D Distributions for Clustering Tests",
       x = "Value", y = "Density")



k <- 4
centers <- runif(k, -5, 5)
x_clusters <- unlist(lapply(centers, function(mu) rnorm(n/k, mu, sd = runif(1, 0.1, 1))))


dens <- density(x_clusters)
modes <- dens$x[which(diff(sign(diff(dens$y))) == -2)]  # local maxima
plot(dens); abline(v = modes, col = 2)


set.seed(1)

k <- 4
true_centers <- runif(k, -5, 5)

# total sample to reuse
n_max <- 10000
x_clusters <- unlist(lapply(true_centers, function(mu)
  rnorm(n_max / length(true_centers), mu, sd = runif(1, 0.1, 1))
))

# helper: estimate modes from first n samples
estimate_modes <- function(x, n) {
  dens <- density(x[1:n])
  dens$x[which(diff(sign(diff(dens$y))) == -2)]
}

# sample sizes to test
ns <- c(200, 500, 1000, 5000, 10000)

# store mode estimates for nested subsets
mode_estimates <- lapply(ns, function(n) sort(estimate_modes(x_clusters, n)))

# plot convergence
plot(
  range(ns), range(unlist(mode_estimates), true_centers), type = "n",
  xlab = "n (nested samples)", ylab = "Mode / Center position",
  main = "Convergence of Estimated Modes to True Centers"
)
abline(h = true_centers, col = 2, lty = 2)
for (i in seq_along(mode_estimates)) {
  points(rep(ns[i], length(mode_estimates[[i]])), mode_estimates[[i]], pch = 19)
}


