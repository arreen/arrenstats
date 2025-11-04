#' std_df
#'
#' @description
#' Standardizes all numerical variables according to if theyre skewed or have fat tails.
#'
#' @param data a data.frame
#' @param revertible if true saves mean and standard deviation with variable names
#'
#' @returns Standardized data.frame
#' @export
#'
#' @examples
#' std_df(iris)
#'
#'
std_df <- function(data, revertible = FALSE) {
  # x is a vecto
  types <- sapply(1:ncol(data), function(x) class(data[, x])) %>% unlist()
  numerical_index <- which(types == "numeric" | types == "integer")

  extreme_meassures <- stage1_summary(data, raw_bool = TRUE,
                 surpress_plotting = TRUE, heatmaps = FALSE, extreme_outliers = TRUE)$raw

  meassures <- stage1_summary(data, raw_bool = TRUE,
                              surpress_plotting = TRUE, heatmaps = FALSE, extreme_outliers = FALSE)$raw

  n <- nrow(data)
  if (n < 60) {
    # stage1_summary actually gives hardcount if n is small
    meassures[rownames(meassures) %in% c("outliers")] <- meassures[rownames(meassures) %in% c("outliers")] / n
    extreme_meassures[rownames(meassures) %in% c("outliers")] <- extreme_meassures[rownames(meassures) %in% c("outliers")] / n
  }

  skew_fat_tails <- rbind(meassures[rownames(meassures) %in% c("gaussian", "outliers"), ], extreme_meassures[rownames(meassures) %in% c("outliers")])

  if (revertible) {
    store_df <- data.frame(var_name = character(), mu = numeric(), spread = numeric())
  }

  for (i in 1:length(numerical_index)) {

    # to skewed/kurtose or too many outliers (5%) outside 3 IQR
    # or too many extreme outliers (1%) outside 5 IQR
    if (n > 100) {
      need_robust <- (skew_fat_tails[1, i] != 1) | (skew_fat_tails[2, i] > 0.05) | (skew_fat_tails[3, i] > 0.01)
    } else {
      # variance in low sample, harsher limit, still continous bounding
      need_robust <- (skew_fat_tails[1, i] != 1) | (skew_fat_tails[2, i] * n > 5) | (skew_fat_tails[3, i] * n > 1)
    }

    if (need_robust) {
      if (!revertible) {
        data[, numerical_index[i]] <- data[, numerical_index[i]] %>% std_robust()
      } else {
        store <- data[, numerical_index[i]] %>% std_robust(revertible = revertible)
        data[, numerical_index[i]] <- store$x
        store_df <- rbind(store_df,
                          data.frame(var_name = colnames(data)[numerical_index[i]],
                                     mu = store$mu,
                                     spread = store$spread
                                     ))

      }
    } else {
      if (!revertible) {
        data[, numerical_index[i]] <- data[, numerical_index[i]] %>% std_z()
      } else {
        store <- data[, numerical_index[i]] %>% std_z(revertible = revertible)
        data[, numerical_index[i]] <- store$x
        store_df <- rbind(store_df,
                          data.frame(var_name = colnames(data)[numerical_index[i]],
                                     mu = store$mu,
                                     spread = store$spread
                          ))

      }

    }

  }

  if (revertible) {
    return(list(std_df = data, reverse_df = store_df))
  }

  return(data)
}

