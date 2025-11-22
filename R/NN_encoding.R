library(reticulate)
#reticulate::install_miniconda(force = TRUE)
#conda_create("tf-env")
# RESTART R
use_condaenv("tf-env", require<- = TRUE)
# numpy and keras in py-config
# conda_install("tf-env", "tensorflow", pip = TRUE)
# restart R
py_config()



library(tensorflow)
library(keras3)
library(stringr)
library(dplyr)

#tensorflow::install_tensorflow(envname = "tf-env")







# standardization happens in std_df, where things have the tag TQZ in the names


WRONG PLACE TO STANDARDIZE, SHOULD STANDARDIZE AFTER TRAINING DATA

for (i in seq_along(embeddings_list)) {
  # Remove the padding before taking the mean, the mean is taken separately to center the point swarm on 0
  embeddings_list[[i]][-1, 1] <- embeddings_list[[i]][-1, 1] - mean(embeddings_list[[i]][-1, 1])
  embeddings_list[[i]][-1, 2] <- embeddings_list[[i]][-1, 2] - mean(embeddings_list[[i]][-1, 2])

  v <- c(embeddings_list[[i]][-1, 1], embeddings_list[[i]][-1, 2])

  # Scale by robust scaling
  Q <- quantile(v, probs = c(0.25, 0.5, 0.75))

  # 1.349 is that under normality it is comparable to a Z-score.
  spread <- (Q[3] - Q[1]) / 1.349

  embeddings_list[[i]][-1, ] <- embeddings_list[[i]][-1, ] / spread

}








