# check for completeness
is_completed <- function(x) {
  return(ifelse(is.na(x), F, T))
}

# function to determine outliers
is_outlier_iqr <- function(x) {
  # +/- 1.5*IQR
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}

is_outlier_sd <- function(x) {
  # mean +/- 3 SD
  return(x < mean(x, na.rm = T) - 3 * sd(x, na.rm = T) | x > mean(x, na.rm = T) + 3 * sd(x, na.rm = T))
}