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










