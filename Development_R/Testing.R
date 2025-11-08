data <- read.csv("Development_R/Final_data.csv")
data$meal_name <- NULL

library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)

library(bnlearn)


num_data <- data %>% numeric_df()
cat_data <- data %>% numeric_df(char = TRUE)

stage1_summary(data, heatmaps = F)
p(data$Physical.exercise) should be made into a categorical varaible
p(data$BMI)




pca_df <- num_data[-which(colnames(num_data) == "Weight..kg.")] %>% std_df()
#outliers
pca_df$Physical.exercise <- NULL
#too correlated
pca_df$cal_from_macros <- NULL
corr <- corr_Y(cbind(pca_df, data.frame("weight" = data$Weight..kg.)), "weight")
pca_df <- pca_df[, !(colnames(pca_df) %in% names(corr)[corr %>% abs() > 0.7])]

data.pca <- princomp(pca_df)
summary(data.pca)

reg_df <-  data.pca$scores %>% as.data.frame()
reg_df <- reg_df %>% std_df()
reg_df <- cbind(reg_df, data.frame("weight" = data$Weight..kg.))

model <- lm(weight ~ ., reg_df)
summary(model)

colnames(num_data)[colnames(num_data) %>% str_detect("Weight")] <- "weight"

lm(weight ~ ., num_data[colnames(pca_df)])

data.pca$loadings[, 5][order(data.pca$loadings[, 5] %>% abs(), decreasing = T)]

data.pca$loadings[, 6][order(data.pca$loadings[, 6] %>% abs(), decreasing = T)]

data.pca$loadings[, 23][order(data.pca$loadings[, 23] %>% abs(), decreasing = T)]

# Categorical
# ------------------------------
for (i in 1:ncol(cat_data)) {
  cat_data[, i] <- cat_data[, i] %>% as.factor()
}
res <- hc(cat_data)
plot(res)
res

viewer(res,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "800px",
       bayesianNetwork.layout = "layout_with_fr")

bnlearn::parents(res, "Y_binned")
bnlearn::children(res, "Y_binned")
mb(res, "Y_binned")
