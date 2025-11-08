data <- read.csv("Development_R/train.csv")
data$blue <- as.factor(data$blue)
data$dual_sim <- as.factor(data$dual_sim)
data$four_g <- as.factor(data$four_g)
data$three_g <- as.factor(data$three_g)
data$touch_screen <- as.factor(data$touch_screen)
data$wifi <- as.factor(data$wifi)

stage1_summary(data)
data$price_range <- as.factor(data$price_range)

#pc fc (front camera is always worse)
PCA makes me believe the average of these can be used instead if
we need independent variables

#width height
Width limits the height
These matter for price_range, maybe average or make uncorrelated

#three g and four g
ALL 4g phones have 3g!!

price range ram!!!!! HUGE

pca_df <- numeric_df(data)

pca_df <- std_df(pca_df)
res <- FactoMineR::PCA(pca_df)


lm()

debug(y_color_scatter)
y_color_scatter(data$battery_power, data$ram, data$price_range, varnames = c("BP", "ram", "price_range"))
y_color_scatter(data$px_width, data$px_height, data$price_range, varnames = c("W", "H", "price_range"))
y_color_scatter(data$talk_time, data$clock_speed, data$price_range, varnames = c("W", "H", "price_range"))


SVM


install.packages("e1071")
library(e1071)

data <- cbind(data %>% numeric_df(), data$price_range)
data <- data %>% std_df()
colnames(data)[15] <- "price_range"
model <- svm(price_range ~ ., data = data, kernel = "polynomial",
             cost = 1, gamma = 1, coef0 = 1, scale = FALSE, degree = 1)

data$predictions <- predict(model, data)
mean(data$predictions == data$price_range)

sv <- model$SV

w <- t(model$coefs) %*% model$SV
# Looking at coefficients is problematic in higher dimensions

We want this to be linear, batch out the test for svm for numeric, sorted by vector norms
Same for categorical
Output prediction rate

Combine with bayesian network for categorical


interpret_svm <- function(data, y_name, cost = 1) {
  y_index <- colnames(data) == y_name
  if (data[, -y_index]) {

  }
  num_df(data, char = TRUE)
}
