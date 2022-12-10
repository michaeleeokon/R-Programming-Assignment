library(dplyr)
library(tidyr)
library(ggplot2)
library(ruler)
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}

maha_dist <- . %>% select_if(is.numeric) %>%
    mahalanobis(center = colMeans(.), cov = cov(.))

isnt_out_maha <- function(tbl, isnt_out_f, ...) {
  tbl %>% maha_dist() %>% isnt_out_f(...)
}
isnt_out_funs <- funs(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)

row_packs_isnt_out <- row_packs(
  # Non-outliers based on some column
  column = . %>% transmute_if(is.numeric, isnt_out_funs),
  # Non-outliers based on Mahalanobis distance
  maha = . %>% transmute(maha = maha_dist(.)) %>%
    transmute_at(vars(maha = maha), isnt_out_funs)
)

group_packs_isnt_out <- group_packs(
  # Non-outliers based on grouping
  group = compute_group_non_outliers,
  .group_vars = "group"
)
