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
now_simple_arx <- function(newX, model, nAhead) {
  predict.Arima(model, newxreg = newX, n.ahead = nAhead)
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
  
  naCount <- sum(is.na(y))
  shortData <- data[1:(length(y)-naCount),]
  
  modToUse <- fit_simple_model(formula, shortData, model,order)
  
  # create predict part here that predicts naCount ahead
  
  newX <- x[(length(shortData[,1])+1):length(x)]
  
  switch (model,
    "lm" = now_simple_lm(newX, modToUse),
    "ar" = now_simple_arx(newX, modToUse, length(newX[,1]))
  )
}