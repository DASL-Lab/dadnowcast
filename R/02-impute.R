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
    while (is.na(x[next_non_na]) & next_non_na < length(x)) {
      next_non_na <- next_non_na + 1
    }

    if (next_non_na == length(x)) {
      x[first_na:length(x)] <- x[first_na - 1]
    } else if (first_na == 1) {
      x[1:next_non_na] <- x[next_non_na + 1]
    } else {
      # Fill NAs by linear interpolation, accounting for differing gaps in dates
      x[first_na:(next_non_na - 1)] <- approx(dates[c(first_na-1, next_non_na)], x[c(first_na-1, next_non_na)], xout = dates[first_na:(next_non_na-1)])$y
    }

    # Find the next NA
    na_vals <- dates[is.na(x)]
  }

  x
}
