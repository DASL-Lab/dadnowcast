#' Fit a Random Forest model on given data and make predictions for a given set of data
#'
#' @param XTrain Training data for the explanatory variables in the model
#' @param YTrain Training data for the response variable
#' @param XNowcast Data to make predictions bases on
#' @param ntree Integer indicating the number of trees in the random forest, default is 500
#' @param mtry Indicates the number of features to try in each node of the Random Forest. Default is the default for randomForest
#'
#' @returns Random Forest object and predictions
#' @export

fit_RF <- function(Y_train, X_train = NULL, X_nowcast = NULL, ntree = 500, mtry = if (!is.null(y) && !is.factor(Y_train)) {
                     max(floor(ncol(X_train) / 3), 1)
                   } else {
                     floor(sqrt(ncol(X_train)))
                   }) {
  Y_train <- data.frame(Y_train)
  X_train <- data.frame(X_train)

  formulaToUse <- paste0(colnames(Y_train), "~")
  for (i in seq(length(ncol(X_train)))) {
    formulaToUse <- paste0(formulaToUse, colnames(X_train)[i], "+")
  }
  formulaToUse <- substr(formulaToUse, 1, (nchar(formulaToUse) - 1))
  formulaToUse <- as.formula(formulaToUse)

  data <- data.frame(X_train, Y_train)

  RFModel <- randomForest::randomForest(formulaToUse, data = data, ntree = ntree, mtry = mtry)

  predictions <- predict(RFModel, newdata = X_nowcast)

  list(model = RFModel, prediction = predictions)
}