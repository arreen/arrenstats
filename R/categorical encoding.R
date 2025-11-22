# Dummy encoding, features explode

# small categories, put as 'other' unless very important
# look at error per category charts

# target encoding, Leave one out leaks data

# catboost, varied orderbased. Each tree trained on a random permutation,
# then the encoding is forgotten and converted to buckets


# prior uniform or noninformative for each variable, then we update based on info,
# split tree, then convert into buckets based on majority vote
# soft voting gets more info, GMM, but not applicable in this case

# doesnt work for multiclass
# take mean of the target variable, but leakage?
posterior for y and each datarow, then take the posterior mean, have a prior for each category,
variance of the variable in the category + mean!

Model-based encoding
Learning a mapping
category→E[Y∣category]


LOO encoding, save mean for future obs.



encode_category <- function(category, target, threshhold = 0.01) {
  stopifnot(is.character(category) | is.factor(category))
  stopifnot(length(category) == length(target))

  counts <- table(category)

  encoded <- rep(0, length(target))

  for (i in 1:length(target)) {
    encoded[i] <- mean(target[-i])
  }

  max_deviation <- function(y) {
    Q <- quantile(y, c(0.025, 0.975))
    mu <- mean(y)
    phi <- max(abs((mu-Q[1])/mu),
               abs((mu-Q[2])/mu)
    )
    return(phi)
  }

  #percent diff is (mu - yi) / (mu *(n-1)) %>% abs()

  if (any((max_deviation(target) / (counts - 1)) > threshhold) ) {
    v <- (max_deviation(target) / (counts - 1)) %>% round(2)

    vec_str <- capture.output(print(v))

    warning(paste(paste(vec_str, collapse = "\n"),  paste0("\ncategory deviations are above the threshhold ", threshhold)))

  }



}

Do K-means leave out but with bayesian prior


Works well if n is large, percent diff doesnt matter

GOTTA APPLY THIS SHUFFLING SEPARATELY FOR EACH FEATURE, otherwise crazy dependancy



set.seed(1)

# Define categories
cats <- c("A", "B", "C", "D", "RARE")

# Set probabilities so one is rare
probs <- c(0.35, 0.30, 0.20, 0.14, 0.01)

# Generate factor variable
x <- factor(sample(cats, size = 200, replace = TRUE, prob = probs))

table(x)

y <- rbinom(200, size = 1, prob = 0.4)  # or any distribution you prefer
df <- data.frame(x, y)

