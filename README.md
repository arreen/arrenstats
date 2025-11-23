
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arrenstats

<!-- badges: start -->

<!-- badges: end -->

Data analysis package.

## Installation

You can install the development version of arrenstats from
[GitHub](https://github.com/) with:

``` r
#devtools::build_readme()
# install.packages("devtools")
# devtools::install_github("arreen/arrenstats", build_vignettes = TRUE)
```

After installing the package, to use some functions you need keras3 and
tensorflow installed. The code below sets up a python evironment used by
other functions in the package.

``` r
# setup_tf_env()
```

## Workflow

This package outlines the typical data-to-knowledge pipeline where the
process naturally progresses by applying stage1_XXX, then advancing to
stage2_XXX, and finally generating output via stage3_XXX.

``` r
library(arrenstats)

data <- iris
train_fraction <- 0.65
val_fraction <- 0.25
n <- nrow(iris)

train_index <- sample(1:n, round(n*train_fraction))
val_index <- sample((1:n)[-train_index], round(n*val_fraction))

train_data <- iris[train_index, ]
val_data <- iris[val_index, ]
test_data <- iris[-c(train_index, val_index), ]
```

Data exploration

``` r

# stage1_missing_values() NOT IMPLEMENTED, mean/median impute? add noise, spread from data?

# stage1_summary(train_data)

# stage1_bayesian() BIN numerical values, understand the theories behind bayesian
```

## Workflow
