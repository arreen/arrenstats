
Stanford guy, how to fit this
https://www.youtube.com/watch?v=Fa-rr6AXMwg


# Do PCA for numerical, BN for categorical

data <- read.csv("Development_R/StudentPerformanceFactors.csv")

data$Y_binned <- ifelse(data$Exam_Score > 67, "above_avg", "below_avg")



bn_df <- numeric_df(data, char = TRUE)

bn_df <- na.omit(bn_df)

# Handle missing data better later! losing alot alot of info!

# shitty code
remove_index <- c()
for (i in 1:nrow(bn_df)) {
  if(any(bn_df[i, ] == "")) {
    remove_index <- c(remove_index, i)
  }

}

bn_df <- bn_df[-remove_index, ]

for (i in 1:ncol(bn_df)) {
  bn_df[, i] <- as.factor(bn_df[, i])
}


bl <- data.frame(
  from = rep("Y_binned", ncol(bn_df) - 1),
  to   = colnames(bn_df)[colnames(bn_df) != "Y_binned"]
)

res <- hc(bn_df, blacklist = bl)

res <- hc(bn_df)
plot(res)
res

viewer(res,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "800px",
       bayesianNetwork.layout = "layout_with_fr")

bnlearn::parents(res, "Y_binned")
bnlearn::children(res, "Y_binned")
mb(res, "Y_binned")

fitted_bn <- bn.fit(res, data = bn_df, method = "bayes")

fitted_bn$Y_binned


fitted_bn <- bn.fit(res, data = bn_df, method = "mle")


cpquery(fitted_bn, event = (Y_binned == "above_avg"),
        evidence = (Access_to_Resources == "High")) - cpquery(fitted_bn, event = (Y_binned == "above_avg"),
                                                              evidence = (Access_to_Resources == "Low"))

cpquery(fitted_bn, event = (Y_binned == "above_avg"),
        evidence = (Parental_Involvement == "High")) - cpquery(fitted_bn, event = (Y_binned == "above_avg"),
                                                               evidence = (Parental_Involvement == "Low"))
