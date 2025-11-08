install.packages("e1071")
library(e1071)

data <- cbind(data %>% numeric_df(), data$price_range)
data <- data %>% std_df()
colnames(data)[15] <- "price_range"
model <- svm(price_range ~ ., data = data, kernel = "polynomial",
             cost = 1, gamma = 1, coef0 = 1, scale = FALSE, degree = 1)

data$predictions <- predict(model, data)
mean(data$predictions == data$price_range)
