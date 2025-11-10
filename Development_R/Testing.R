data <- read.csv("Development_R/egenskaper.csv")
data$Person.ID <- NULL

data$Quality.of.Sleep <- ifelse(data$Quality.of.Sleep > median(data$Quality.of.Sleep), "above median", "below median")

library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

library(bnlearn)

stage1_summary(data)

y <- "Sleep.Duration"
corr <- corr_Y(data, y)
corr

data$Stress.Level <- data$Stress.Level + runif(374, -0.1, 0.1)
y_color_scatter(data[names(corr)[1]], data[names(corr)[2]], data[y], varnames = c(names(corr)[1:2], y))


num_data <- data %>% numeric_df()
cat_data <- data %>% numeric_df(char = TRUE)

p(data$Sleep.Duration)
p(data$Physical.exercise) should be made into a categorical varaible
p(data$BMI)

pca_workflow(data, y)


# Categorical
# ------------------------------
for (i in 1:ncol(cat_data)) {
  cat_data[, i] <- cat_data[, i] %>% as.factor()
}

cat_num <- num_data
for (i in 1:ncol(num_data)) {
  y <- cat_num[, i]
  Q <- quantile(y, c(0.33, 0.66))

  temp_y <- rep("", length(cat_num[, i]))

  temp_y[y <= Q[1]] <- "low"
  temp_y[y > Q[1] & y <= Q[2]] <- "mid"
  temp_y[y > Q[2]] <- "high"

  cat_num[, i] <- as.factor(temp_y)
}

BN_data <- cbind(cat_data, cat_num)

res <- hc(BN_data)
plot(res)
res

viewer(res,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "800px",
       bayesianNetwork.layout = "layout_with_fr")

bnlearn::parents(res, "Y_binned")
bnlearn::children(res, "Y_binned")
mb(res, "Y_binned")


fitted_bn <- bn.fit(res, data = BN_data, method = "bayes")

fitted_bn$Quality.of.Sleep
fitted_bn$Sleep.Disorder


fitted_bn <- bn.fit(res, data = BN_data, method = "mle")


cpquery(fitted_bn, event = (Y_binned == "above_avg"),
        evidence = (Access_to_Resources == "High")) - cpquery(fitted_bn, event = (Y_binned == "above_avg"),
                                                              evidence = (Access_to_Resources == "Low"))

# ----------------------------------------------------


data$Purchased
model <- glm(Purchased ~ ., data, family = "binomial")


data <- cbind(data %>% numeric_df(), data$)
data <- data %>% std_df()
colnames(data)[15] <- "price_range"
model <- svm(price_range ~ ., data = data, kernel = "polynomial",
             cost = 1, gamma = 1, coef0 = 1, scale = FALSE, degree = 1)

data$predictions <- predict(model, data)
mean(data$predictions == data$price_range)

