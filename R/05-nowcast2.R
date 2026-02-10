#' Nowcast various models from a prepared data frame
#' 
#' @param formula A formula, e.g. y ~ x, y ~ lag(x1, 1) + lag(x2, 3)
#' @param data A data frame. Must contain the variables specified in the formula and in `date_col`. Trailing NA values in `y` will be nowcasted.
#' @param model The model to use for nowcasting. Currently implemented: "lm", "ar". Can be a vector, in which case the model is trained for each model in the vector.
#' @param test_size The proportion of the data to use for testing. If NULL, the data are not split. Defaults to 10% of the data.
#' @param params The parameters to use for the model. Must be a named list.
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#' @param eval A character vector of evaluation metrics to use. Currently implemented: "rmse", "mae", and "mre" (mean relative error).
#' 
#' @returns An object of class "`dadnow`".
#' 
#' @export
nowcast_one <- function(formula, data, model, test_size = 0.1, params = NULL, date_col = NULL) {
  prepped_data <- prep_data(formula, data, model, test_size, date_col = date_col)

  params <- check_params(params, model)

  nowcast <- switch(model,
    "lm" = fit_LM(X_train = prepped_data$X_train, Y_train = prepped_data$y_train, X_nowcast = prepped_data$X_nowcast),
    "ar" = fit_AR(X_train = prepped_data$X_train, Y_train = prepped_data$y_train, X_nowcast = prepped_data$X_nowcast, p = params$order),
    "arx" = fit_ARX(X_train = prepped_data$X_train, Y_train = prepped_data$y_train, X_nowcast = prepped_data$X_nowcast, p = params$order),
  )
  nowcast$params <- params
  # TODO: Implement test set evaluation
  nowcast$eval <- switch(eval,
    "rmse" = 1
  )
  prepped_data[paste0("nowcast_", model)] <- list(nowcast)

  return(prepped_data)
}


check_params <- function(params, model, verbose = TRUE) {
  msg <- function(...) if (verbose) cat(...)

  expected_params <- switch(model,
    "lm" = list(),
    "ar" = list(order = 1),
    "arx" = list(order = 1),
    "gam" = list(smooths = NULL)
  )
  if (model == "lm") {
    return(NULL)
  }
  if (is.null(params)) {
    msg("No parameters specified, but parameters are required for this model.\n")
    params <- set_default_params(expected_params, model, verbose = verbose)
  }
  if (!is.list(params)) {
    msg("'params' must be a list.\n")
    params <- set_default_params(expected_params, model, verbose = verbose)
  }
  if (!all(names(params) %in% names(expected_params))) {
    missing_params <- setdiff(names(expected_params), names(params))
    msg("Invalid parameters specified: ", paste0(missing_params, collapse = ", "), "\n")
    params <- set_default_params(expected_params, model, verbose = verbose)
  }

  return(params)
}

set_default_params <- function(params, model, verbose = TRUE) {
  if (verbose) {
    cat(
      paste0(
        "Setting default parameters ",
        paste0(names(params), " = ", paste0(params)),
        "\n"
      )
    )
  }
  params
}