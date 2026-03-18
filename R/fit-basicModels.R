#' Fit a simple linear model (internal)
#' 
#' @param x The single covariate vector.
#' @param y The response vector.
fit_simple_lm <- function(x, y) {
  lm (y ~ x)
}

#' Fit a simple ARX model (internal)
#' 
#' @inheritParams fit_simple_lm
#' @param order The order of the AR(p) process.
fit_simple_arx <- function(x, y, order = 1) {
  arima(y, order = c(order, 0, 0), xreg = x)
}

#' Fit a simple (on covariate) regression model
#' 
#' @param formula y ~ x
#' @param data A data frame with columns used in the formula.
#' @param model Currently implemented: "lm" and "ar"
#' @param order The AR order to be used in `arima()`.
#' 
#' @returns A model object for which a predict method exists. 
fit_simple_model <- function(
    formula, data,
    model = c("lm", "ar")[1],
    order = NULL
) {
  y <- data[, all.vars(formula[[2]])]
  # TODO: Error checking for number of covariates.
    # Alternative: allow an arbitrary number of covariates.
  x <- data[, all.vars(formula[[3]])]
  model_res <- switch(model,
    "lm" = fit_simple_lm(x, y),
    "ar" = fit_simple_arx(x, y, order)
  )

  model_res
}

#' Fit a basic indicated model
#'
#' @param formula A formula indicating which variables to use in the model
#' @param model A string indicating the type of model to be fit to the data, can be linear or AR
#' @param data A data frame containing at least the data to be used
#' @param ar_order An optional argument indicating the order of the AR model to be used
#'
#' @return The specified model based on the given formula
#'
#'
#' @examples
#' y <- rnorm(100)
#' x <- seq(1,100)
#' df <- data.frame(x,y)
#' fit-basicModels(y ~ x, model = "linear", df)

fit_basic_models <- function(formula, model, data, ar_order = 1) {
  if (!(model %in% c("AR","linear"))) {
    stop("Model not recognized")
  } else if (model == "AR") {
    return(arima(formula, order = c(ar_order, 0, 0)))
  }

  # obtain variables for the left and righthand side of the given formula
  lhs <- formula[[2]]
  rhs <- formula[[3]]

  # this works to detect if the left hand side value is in the data frame!
  if (as.character(lhs) %in% colnames(data)) {
    lhs_data <- data[, which(colnames(data) == lhs)]
  } else {
    return(paste("Variable", lhs, "not found"))
  }

  # determine the number of variables in the righthand side of the formula
  rhsVars <- all.vars(rhs)
  numRhsVars <- length(rhsVars)

  # detect if all variables used in the righthand side are in the given data
  rhs_data <- data.frame(matrix(nrow = length(lhs_data), ncol = numRhsVars))
  # print(c(length(lhs_data), numRhsVars))

  for (i in seq(1:numRhsVars)) {
    if (as.character(rhsVars[i]) %in% colnames(data)) {
      rhs_data[, i] <- data[, which(colnames(data) == rhsVars[i])]
    } else {
      return(paste("Variable", rhsVars[i], "not found"))
    }
  }

  usedData <- cbind(lhs_data, rhs_data)

  colnames(usedData) <- c(lhs, rhsVars)

  lm(formula, data = data)
}
