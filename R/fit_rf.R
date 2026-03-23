#' Fit a Random Forest model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Current data of X_train for which there is no Y_train data, used to make nowcasts
#' @param params Named list containing additional parameters:
#' @param ntree Integer indicating the number of trees in the random forest, default is 500
#' @param mtry Indicates the number of features to try in each node of the Random Forest. Default is the default for randomForest
#' @param weights A vector of length same as y that are positive weights used only in sampling data to grow each tree (not used in any other calculation)
#' @param replace Should sampling of cases be done with or without replacement?
#' @param maxnodes Maximum number of terminal nodes trees in the forest can have.
#'
#' @returns Random Forest object, predictions, and fitted values

fit_rf <- function(Y_train, X_train = NULL, X_nowcast = NULL,
                   params = list(ntree = 500, mtry = NULL, weights = NULL, replace = TRUE, maxnodes = NULL, nodesize = 5)) {
  # check if the package "randomForest" is installed and gives a warning if it isn't
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Package \"randomForest\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }

  # ensure the data is in a consistent format
  Y_train <- data.frame(Y_train)
  X_train <- data.frame(X_train)

  # create the formula for the model, based on the columns of Y_train and X_train, expected to be something like: y ~ x1 + x2
  formulaToUse <- paste0(colnames(Y_train), "~")
  for (i in seq_len(ncol(X_train))) {
    formulaToUse <- paste0(formulaToUse, colnames(X_train)[i], "+")
  }
  formulaToUse <- substr(formulaToUse, 1, (nchar(formulaToUse) - 1))
  formulaToUse <- as.formula(formulaToUse)

  # create a combined dataframe of response variables and explanatory variables
  data <- data.frame(X_train, Y_train)

  # retrieves the variables from `params` and if they are missing uses default values
  if (!"ntree" %in% names(params)) {
    ntree <- 500
  } else {
    ntree <- params$ntree
  }

  if (!"mtry" %in% names(params)) {
    mtry <- if (!is.null(Y_train) && !is.factor(Y_train)) {
      max(floor(ncol(X_train) / 3), 1)
    } else {
      floor(sqrt(ncol(X_train)))
    }
  } else {
    mtry <- params$mtry
  }
  # sets a default value for mtry here, if there is no specified one
  if (is.null(mtry)) {
    mtry <- if (!is.null(Y_train) && !is.factor(Y_train)) {
      max(floor(ncol(X_train) / 3), 1)
    } else {
      floor(sqrt(ncol(X_train)))
    }
  }

  if (!"weights" %in% names(params)) {
    weights <- NULL
  } else {
    weights <- params$weights
  }

  if (!"replace" %in% names(params)) {
    replace <- TRUE
  } else {
    replace <- params$replace
  }

  if (!"maxnodes" %in% names(params)) {
    maxnodes <- NULL
  } else {
    maxnodes <- params$maxnodes
  }

  if (!"nodesize" %in% names(params)) {
    nodesize <- 5
  } else {
    nodesize <- params$nodesize
  }

  # creates the random forest model
  RFModel <- randomForest::randomForest(formulaToUse,
    data = data, ntree = ntree, mtry = mtry, weights = weights,
    replace = replace, maxnodes = maxnodes, nodesize = nodesize,
    na.action = na.omit
  )

  # creates predicitons
  predictions <- data.frame(prediction = predict(RFModel, newdata = X_nowcast))

  # creates fitted values
  fits <- predict(RFModel, newdata = X_train)

  # creates the output in the correct format
  list(model = RFModel, prediction = predictions, fitted_values = fits)
}