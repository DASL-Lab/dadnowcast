#' Nowcast from a simple linear model (internal)
#'
#' @param NewX The known x values to be nowcast on.
#' @param model The model to use for predictions.
now_simple_lm <- function(newX, model) {
  predict.lm(model, newdata = list(x = newX))
}

#' Nowcast from a simple AR model (internal)
#'
#' @param NewX The known x values to be nowcast on.
#' @param model The model to use for predictions.
now_simple_arx <- function(newX, model) {
  predict(model, newxreg = newX, n.ahead = length(newX))
}

#' Return the number of NA's at the end of a vector (internal)
#'
#' @param y A vector of values with some number of NA's at the end of it.
find_nas <- function(y) {
  which(rev(y) != "NA") |>
    min() - 1
}

#' Perform nowcasting for the specified response variable
#'
#' @param formula y ~ x
#' @param data A data frame with columns used in the formula.
#' @param model Currently implemented: "lm" and "ar"
#' @param order The AR order to be used in `arima()`.
#'
#' @returns Predictions for the missing data in the response variable

now_simple_models <- function(
  formula, data,
  model = c("lm", "ar")[1],
  order = NULL
) {
  y <- data[, all.vars(formula[[2]])]
  x <- data[, all.vars(formula[[3]])]

  naCount <- find_nas(y)
  numNonNa <- (length(y) - naCount)
  shortData <- data[1:numNonNa, ]

  modToUse <- fit_simple_model(formula, shortData, model, order)

  # create predict part here that predicts naCount ahead

  newX <- x[(numNonNa + 1):length(x)]

  switch(model,
    "lm" = now_simple_lm(newX, modToUse),
    "ar" = now_simple_arx(newX, modToUse)
  )
}