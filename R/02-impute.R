#' Basic linear interpolation imputation
#' 
#' @param dates A vector of dates.
#' @param x A vector of values.
#' 
#' @returns A vector of values with NAs imputed.
#' @export
impute_linear <- function(dates, x) {
  na_vals <- dates[is.na(x)]
  if (length(na_vals) == 0) return(x)
  
  while (length(na_vals) > 0) {
    # Find the first NA
    first_na <- which(is.na(x))[1]

    # Find the next non-NA value
    next_non_na <- first_na + 1
    while (is.na(x[next_non_na]) & next_non_na <= length(x)) {
      next_non_na <- next_non_na + 1
    }

    if (next_non_na - first_na > 10) {
      if (first_na == 1) {
        warning(paste0("There are ", next_non_na - first_na, " NA values at the start of the time series. Consider subsetting the data before modelling."))
      } else {
        warning(paste0("There are ", next_non_na - first_na, " NA values in the middle of the time series. Consider more sophisticated imputation strategies first, such as imputeTS::na_kalman() or multivariate imputation."))
      }
    } 

    if (next_non_na >= length(x)) {
      x[first_na:length(x)] <- x[first_na - 1]
    } else if (first_na == 1) {
      x[1:next_non_na] <- x[next_non_na + 1]
    } else if (next_non_na - first_na == 1) {
      x[first_na] <- (x[first_na-1] + x[next_non_na]) / 2
    } else {
      # Fill NAs by linear interpolation, accounting for differing gaps in dates
      dates_to_replace <- as.numeric(dates[c(first_na-1, next_non_na)])
      y_coords <- x[c(first_na-1, next_non_na)]
      x_out <- as.numeric(dates[first_na:(next_non_na-1)])
      new_vals <- approx(dates_to_replace, y_coords, xout = x_out)$y

      x[first_na:(next_non_na - 1)] <- new_vals
    }

    # Find the next NA
    na_vals <- dates[is.na(x)]
  }

  x
}
