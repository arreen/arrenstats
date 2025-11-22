
library(mlbench)





special standarization for embeddings

data <- read.csv("Development_R/train1.data", col.names = c("v1", "v2", "X1"))
data$X1 <- data$X1 %>% as.factor()

data_list <- data %>% std_df(revertible = TRUE)
data <- data_list$std_df

for (i in 1:4) {
  data <- rbind(data, data, data, data, data)
}


test <- read.csv("Development_R/test.data", col.names = c("v1", "v2", "X1"))
test$X1 <- test$X1 %>% as.factor()
test <- test %>% std_df(data_list$reverse_df)


CV for kernel, ONCE, after it is the same for all

library(e1071)
kernels <- c("linear", "polynomial", "radial basis")

Cross validation

k <- 10
folds <- sample(rep(1:k, length.out = nrow(data)))



for (i in 1:10) {
  train <- df[which(folds != i), ]
  test  <- df[which(folds == i), ]

  model <- svm(X1 ~ ., data = data, kernel = "polynomial",
               cost = 1, coef0 = 1, scale = FALSE, degree = 3)

  loss <- loss_fun()

}

model <- svm(X1 ~ ., data = data, kernel = "polynomial",
             cost = 1, coef0 = 1, scale = FALSE, degree = 3)


data$predictions <- predict(model, data)
mean(data$predictions == data$X1)

mean(predict(model, test) == test$X1)

data$predictions <- NULL

model <- svm(X1 ~ ., data = data, kernel = "linear",
             cost = 1, coef0 = 1, scale = FALSE, degree = 3)

data$predictions <- predict(model, data)
mean(data$predictions == data$X1)

mean(predict(model, test) == test$X1)


