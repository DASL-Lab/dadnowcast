#' Fit an eXtreme Gradient Boost model model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#' @param ntree Integer indicating the number of trees in the random forest, default is 500
#' @param mtry Indicates the number of features to try in each node of the Random Forest. Default is the default for randomForest
#' @param weights A vector of length same as y that are positive weights used only in sampling data to grow each tree (not used in any other calculation)
#' @param replace Should sampling of cases be done with or without replacement?
#' @param maxnodes Maximum number of terminal nodes trees in the forest can have.
#'
#' @returns XGBoost object and predictions
#' @export

fit_XGBoost <- function(Y_train, X_train = NULL, 
                   params = list(nrounds = NULL, evals = list(), 
                                 objective = NULL, verbose = 1,
                                 XGBparams = list())) {
  
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    paste("Package \"xgboost\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }
  
  Y_train <- data.frame(Y_train)
  X_train <- data.frame(X_train)
  data <- data.frame(X_train, Y_train)
  dMatrixTrain <- xgb.DMatrix(data, label = Y_train)
  
  if (is.null(params$nrounds)) {
    nrounds <- 10
  } else {
    nrounds <- params$nrounds
  }
  
  if (is.null(params$evals)) {
    evals <- list()
  } else {
    evals <- params$evals
  }
  
  if (is.null(params$objective)) {
    objective <- NULL
  } else {
    objective <- params$objective
  }
  
  if (is.null(params$verbose)) {
    verbose <- 1
  } else {
    verbose <- params$verbose
  }
  
  if (is.null(params$XGBparams)) {
    xgbParams2 = xgb.params()
  } else {
    xgbParams2 <- params$XGBparams
  }
  
  XGBModel <- xgboost::xgb.train(
    data = dMatrixTrain, params = xgbParams2, nrounds = nrounds, evals = evals, 
    objective = objective, verbose = verbose)
  
  predictions <- predict(XGBModel, newdata = cbind(X_nowcast,rep(NA,length(X_nowcast[,1]))))
  
  list(model = XGBModel, prediction = predictions)
}