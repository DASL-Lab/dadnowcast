#' Fit a linear model on given data and make predictions for a given set of data
#'
#' @param XTrain Training data for the explanatory variables in the model
#' @param YTrain Training data for the response variable
#' @param XNowcast Data to make predictions bases on
#'
#' @returns Linear model object and predictions
#' @export

fit_LM <- function(Y_train, X_train, X_nowcast) {
  full_data <- as.data.frame(cbind(Y_train, X_train))

  fitted_LM <- lm(Y_train ~ ., data = full_data)

  XNowcast <- as.data.frame(X_nowcast)

  colnames(XNowcast) <- colnames(full_data)[-1]

  predicted_LM <- predict(fitted_LM, newdata = XNowcast)

  list(fitted_LM, predicted_LM)
}