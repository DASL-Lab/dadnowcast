#' Fit an eXtreme Gradient Boost model model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Current data of X_train for which there is no Y_train data, used to make nowcasts
#' @param params A named list containing options for the parameters used by `xgboost::xgb.train`: `nrounds` which is the number of boosting iteration to do, `XGBparams` a list of more parameters used in the model the most important of which is `max_depth` both `nrounds` and `max_depth` are tuned if their values are not specified, it also takes `verbose` should output be silent (0) or not (1)
#'
#' @returns XGBoost object, predictions, and fitted values

fit_XGBoost <- function(Y_train, X_train = NULL, X_nowcast = NULL,
                        params = list(
                          nrounds = 100, verbose = 1,
                          XGBparams = list()
                        )) {
  # check if the package "xgboost" is installed and gives a warning if it isn't
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package \"xgboost\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }

  # ensure the data is in a form that the functions we are using will accept
  Y_train <- data.frame(Y_train)
  X_train <- data.frame(X_train)
  dMatrixTrain <- xgboost::xgb.DMatrix(X_train, label = as.matrix(Y_train))

  # only does cross validation for nrounds if the number of rounds is not specified
  if (!"nrounds" %in% names(params)) {
    # internal cross validation to tune the nrounds of the model
    XGBCV <- xgboost::xgb.cv(
      params = list(max_depth = 2), data = dMatrixTrain,
      nrounds = 100, nfold = 5, verbose = 0,
      early_stopping_rounds = 10
    )
    nrounds2 <- XGBCV$early_stop$best_iteration
  } else {
    nrounds2 <- params$nrounds
  }

  if (!"verbose" %in% names(params)) {
    verbose <- 1
  } else {
    verbose <- params$verbose
  }

  if (!"XGBparams" %in% names(params)) {
    depths <- seq(2, 10)

    cvDepth <- c()

    for (i in depths) {
      cv <- xgboost::xgb.cv(
        params = list(max_depth = i), data = dMatrixTrain,
        nrounds = nrounds2, nfold = 5, verbose = 0,
        early_stopping_rounds = 10
      )
      cvDepth[i] <- cv$early_stop$best_score
    }
    xgbParams2 <- xgboost::xgb.params(max_depth = which.min(cvDepth))
  } else {
    xgbParams2 <- params$XGBparams
  }

  # training the model
  XGBModel <- xgboost::xgb.train(
    data = dMatrixTrain, params = xgbParams2, nrounds = nrounds2,
    verbose = verbose
  )

  # create the nowcasting data in the proper form
  yNow <- as.matrix(rep(NA, length(data.frame(X_nowcast)[, 1])))

  X_nowcast <- as.matrix(X_nowcast)

  # create the predictions for the XGBoost model
  dMatrixPred <- xgboost::xgb.DMatrix(data = X_nowcast, label = yNow)

  predictions <- data.frame(prediction = predict(XGBModel, newdata = dMatrixPred))

  # create the fitted values by running predict of the training data
  fits <- predict(XGBModel, newdata = dMatrixTrain)

  # ensure the output is in the proper named list format
  list(model = XGBModel, prediction = predictions, fitted_values = fits)
}