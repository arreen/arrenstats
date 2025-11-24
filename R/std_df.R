#' std_df
#'
#' @description
#' Standardizes all numerical variables according to if they're skewed or have fat tails. If originial scale is given it will revert the given standardization for the data argument.
#' Special standardization for embeddings (tagged TQZ, be careful of including this in var names).
#' They get scaled per category, but centered by level, pointswarm centered at 0, radius scaled down, geometric meaning of embedding is perserved.
#' Standardizes binary variables by first mapping them to 0, 1 (the category appearing first in the vector becomes 0) and then applying - mean / MAD*1.4826
#'
#' @param data a data.frame, can also revert standardization for vectors
#' @param revertible if true saves mean and standard deviation with variable names
#' @param original_scale put the reverse_df you got from revertible = TRUE in here to revert standardization
#' @param standardize_new_data_like_train If you put reverse_df here, it will standardize data using these locations and scales
#'
#' @returns Standardized data.frame
#' @import dplyr
#' @export
#'
#' @examples
#' std_df(iris)
#'
#'
std_df <- function(data, original_scale = NA, revertible = FALSE, standardize_new_data_like_train = NA) {

  if (is.data.frame(standardize_new_data_like_train)) {
    my_df <- standardize_new_data_like_train

    scale_these <- which(colnames(data) %in% my_df$var_name)

    binary_var_index <- sapply(1:ncol(data), function(x) data[, x] %>% unique() %>% length())
    binary_var_index <- which(binary_var_index == 2)

    for (binary in binary_var_index) {
      data[, binary] <- data[, binary] %>% ordinal_encoder()

    }


    for (i in scale_these) {
      print(i)
      var <- colnames(data)[i]
      mu <- my_df[my_df$var_name == var, 2]
      spread <- my_df[my_df$var_name == var, 3]
      data[, i] <- (data[, i] - mu) / spread


    }
    return(data)

  }


  if (is.data.frame(original_scale)) {

    if (is.data.frame(data)) {
      for (i in 1:nrow(original_scale)) {
        var <- original_scale$var_name[i]
        data[var] <- data[var] * original_scale$spread[i] + original_scale$mu[i]
      }
    return(data)


    } else {

      if (is.vector(data)) {
        if (!names(data) %>% is.null()) {
          for (i in 1:nrow(original_scale)) {
            var <- original_scale$var_name[i]
            data[var] <- data[var] * original_scale$spread[i] + original_scale$mu[i]
          }

        } else {
          for (i in 1:nrow(original_scale)) {
            data[i] <- data[i] * original_scale$spread[i] + original_scale$mu[i]
          }
        }

        return(data)
      }
    }

  }
  # x is a vecto
  types <- sapply(1:ncol(data), function(x) class(data[, x])) %>% unlist()
  numerical_index <- which(types == "numeric" | types == "integer")


  binary_var_index <- sapply(1:ncol(data), function(x) data[, x] %>% unique() %>% length())
  if (any(binary_var_index < 2)) {
    return(paste0("check variables", colnames(data)[binary_var_index < 2]) %>% print())
  }

  binary_var_index <- which(binary_var_index == 2)


  tqz_index <- str_detect(colnames(data), "TQZ") %>% which()

  tqz_cols <- colnames(data)[tqz_index]



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



  skew_fat_tails <- rbind(meassures[rownames(meassures) %in% c("gaussian", "outliers"), , drop = FALSE], extreme_meassures[rownames(meassures) %in% c("outliers"), drop = FALSE])

  if (revertible) {
    store_df <- data.frame(var_name = character(), mu = numeric(), spread = numeric())
  }


  # ------------
  groups <- str_remove(tqz_cols, "_[0-9]+_TQZ$")

  # --------------

  group_standardized <- setNames(as.list(rep(FALSE, length(groups))), groups)

  for (i in 1:length(numerical_index)) {


    if (n > 100) {
      need_robust <- (skew_fat_tails[1, i] != 1) | (skew_fat_tails[2, i] > 0.05) | (skew_fat_tails[3, i] > 0.01)
    } else {
      # variance in low sample, harsher limit, still continous bounding
      need_robust <- (skew_fat_tails[1, i] != 1) | (skew_fat_tails[2, i] * n > 5) | (skew_fat_tails[3, i] * n > 1)
    }




    if ((i %in% tqz_index) | (i %in% binary_var_index)) {


      if (i %in% binary_var_index) {

        store <- data[, i] %>% ordinal_encoder() %>% std_robust(revertible = TRUE, binary = TRUE)

        if (revertible) {
          store_df <- rbind(store_df,
                            data.frame(var_name = colnames(data)[numerical_index[i]],
                                       mu = store$mu,
                                       spread = store$spread
                            ))
        }

        data[, i] <- store$x

      } else {
        group <- str_remove(colnames(data)[i], "_[0-9]+_TQZ$")



        if (need_robust) {

          if (!group_standardized[[group]]) {


            group_df <- data[, str_remove(colnames(data), "_[0-9]+_TQZ$") == group] %>% unlist()
            Q <- quantile(group_df, probs = c(0.25, 0.5, 0.75))


            mu <- Q[2]
            # 1.349 is that under normality it is comparable to a Z-score.
            spread <- (Q[3] - Q[1]) / 1.349

            if (spread == 0) {

              # MAD relation to sd under normality

              spread <- 1.4826 * mean(abs(group_df - mu))

            }

            if (spread == 0) {
              # all are constant, this works for the revertible transformation, spread at 0 makes the reverse not work.
              spread <- 1
            }

            group_standardized[[group]] <- spread
      }



        } else {
          spread <- group_standardized[[group]]
        }



        # Remove mu afterwards
        mu <- median(data[, numerical_index[i]])
        data[, numerical_index[i]] <- (data[, numerical_index[i]] - mu) / spread

        if (revertible) {
          store_df <- rbind(store_df,
                            data.frame(var_name = colnames(data)[numerical_index[i]],
                                       mu = mu,
                                       spread = spread
                            ))
        }

         else {


        if (!group_standardized[[group]]) {
          group_df <- data[, str_detect(colnames(data), group)] %>% unlist()

          spread <- sd(group_df)

          group_standardized[[group]] <- spread

        } else {
          spread <- group_standardized[[group]]
        }




        # Remove mu afterwards
        mu <- mean(data[, numerical_index[i]])
        data[, numerical_index[i]] <- (data[, numerical_index[i]] - mu) / spread
        if (revertible) {
                  store_df <- rbind(store_df,
                          data.frame(var_name = colnames(data)[numerical_index[i]],
                                     mu = mu,
                                     spread = spread
                          ))
        }


         }

      }

    } else {
      # to skewed/kurtose or too many outliers (5%) outside 3 IQR
      # or too many extreme outliers (1%) outside 5 IQR

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

  }

  if (revertible) {
    return(list(std_df = data, reverse_df = store_df))
  }

  return(data)
}



