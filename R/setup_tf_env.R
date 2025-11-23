#' setup_tf_env
#'
#' @description
#' Helps you download a tensorflow environment. Run once after downloading the package.
#' Package developed with python 3.10, other Python versions may not work reliably.
#'
#' @export
#' @import reticulate
#'
#'
setup_tf_env <- function() {
  if (!"tf-env" %in% conda_list()$name) conda_create("tf-env")
  conda_install("tf-env", c("tensorflow", "keras", "numpy"), pip = TRUE)
  message("Environment 'tf-env' is ready.")
}
