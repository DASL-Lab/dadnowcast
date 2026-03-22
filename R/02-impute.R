#' Basic linear interpolation imputation.
#' 
#' Interpolation is performed by linearly interpolating between the two nearest non-NA value. If the NAs are at the start or end of the time series, they are umputed as the value of the nearest non-NA value.
#' 
#' @param dates A vector of dates.
#' @param x A vector of values.
#' 
#' @returns A vector of values with NAs imputed.
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

    if (next_non_na - first_na > 10) { # more than 10 NAs in a row
      if (first_na == 1) {
        warning(paste0("There are ", next_non_na - first_na, " NA values at the start of the time series. Consider subsetting the data before modelling."))
      } else {
        warning(paste0("There are ", next_non_na - first_na, " NA values in the middle of the time series. Consider more sophisticated imputation strategies first, such as imputeTS::na_kalman() or multivariate imputation."))
      }
    } 

    if (next_non_na >= length(x)) { 
      # If there are NAs at the end of the time series, fill them in with the most recent non-NA value
      x[first_na:length(x)] <- x[first_na - 1]
    } else if (first_na == 1) {
      # If there are NAs at the start of the time series, fill them in with the first non-NA value
      x[1:next_non_na] <- x[next_non_na + 1]
    } else if (next_non_na - first_na == 1) {
      # If there's just one NA, fill it in with the average of the two bordering non-NA values
      # Note that "approx" needs at least two non-NA values to interpolate, so this is a special case.
      x[first_na] <- (x[first_na-1] + x[next_non_na]) / 2
    } else {
      # Fill NAs by linear interpolation, accounting for differing gaps in dates
      dates_to_replace <- as.numeric(dates[c(first_na-1, next_non_na)])
      y_coords <- x[c(first_na-1, next_non_na)]
      x_out <- as.numeric(dates[first_na:(next_non_na-1)])
      new_vals <- approx(dates_to_replace, y_coords, xout = x_out)$y

      x[first_na:(next_non_na - 1)] <- new_vals
    }

    # Find the next NA for the next iteration
    na_vals <- dates[is.na(x)]
  }

  x
}
