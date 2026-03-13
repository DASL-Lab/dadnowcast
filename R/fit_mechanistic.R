nllik_normal <- function(mu, sigma, y) {
  sum(log(sigma) + ((y - mu) / sigma)^2)
}

optim_normal <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    eta <- theta[1]
    sigma <- theta[2:4]
    nllik_normal(eta * Rt, sigma[1], Dt) +
      nllik_normal(eta * sc * Rt, sigma[2], Ct) +
      nllik_normal(eta * sp * Rt, sigma[3], Pt)
  }
  
  optim(
    par = c("eta" = 1, "sigmaD" = 1, "sigmaC" = 1.5, "sigmaP" = 0.5),
    fn = nllik,
    method = "L-BFGS-B",
    lower = rep(1e-8, 4),
    control = list(maxit = 10000)
  )
}

nllik_poisson <- function(theta, y) {
  -sum(y * log(theta) - theta)
}


optim_poisson <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    eta_Rt <- max(1e-8, theta[1] * Rt)
    nllik_poisson(eta_Rt, Dt) +
    nllik_poisson(eta_Rt * sc, Ct) +
    nllik_poisson(eta_Rt * sp, Pt)
  }
    
  optim(
    par = c("eta" = 1),
    fn = nllik,
    control = list(maxit = 10000),
    method = "Brent",
    lower = 1e-8,
    upper = 10000
  )
}
nllik_negbinom <- function(mu, size, y) {
  -sum(lgamma(y + size) - lgamma(size) - lgamma(y + 1) +
       size * log(size / (size + mu)) + y * log(mu / (size + mu)))
}

optim_negbinom <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    eta_Rt <- max(1e-8, theta[1] * Rt)
    sizeD <- theta[2]          # dispersion for Dt
    sizeC <- theta[3]          # dispersion for Ct
    sizeP <- theta[4]          # dispersion for Pt

    nllik_negbinom(eta_Rt, sizeD, Dt) +
      nllik_negbinom(eta_Rt * sc, sizeC, Ct) +
      nllik_negbinom(eta_Rt * sp, sizeP, Pt)
  }

  ## enforce positivity for all four parameters
  lower_bounds <- rep(1e-8, 4)

  optim(
    par = c("eta" = 1, "thetaD" = 2, "thetaC" = 2.5, "thetaP" = 3),
    fn = nllik,
    method = "L-BFGS-B",
    lower = lower_bounds,
    control = list(maxit = 10000))
}


#' Fit a mechanistic model to the data
#' 
#' @param Dt,Ct,Pt,Rt The data correspoding to DAD, CNISP, PTSOS, and RVDSS, respectively.
#' @param Rt_nowcast The RVDSS data for the nowcast period. This is used to create the nowcast predictions for the mechanistic model.
#' @param sc,sp The scaling factors for the CNISP and PTSOS data.
#' @param method Either "normal", "poisson", or "negbinom".
#'
#' @returns A list with the parameter estimates and the negative log-likelihood.
#' 
#' @examples
#' sim_poisson_data <- function(eta, Rt, sc = 0.2, sp = 0.3) {
#'   data.frame(
#'     Dt = rpois(length(Rt), eta * Rt),
#'     Ct = rpois(length(Rt), eta * sc * Rt),
#'     Pt = rpois(length(Rt), eta * sp * Rt),
#'     Rt = Rt
#'   )
#' }
#' d2 <- sim_poisson_data(eta = 5, Rt = rpois(100, 100))
#' fit_mechanistic(d2$Dt, d2$Ct, d2$Pt, d2$Rt, Rt_nowcast = rpois(10, 100), method = "poisson")
#' @export
fit_mechanistic <- function(Dt, Ct, Pt, Rt, Rt_nowcast, sc = 0.2, sp = 0.3, method = "normal") {
  optim_res <- switch(method,
    "normal" = optim_normal(Dt, Rt, Ct, Pt, sc, sp),
    "poisson" = optim_poisson(Dt, Rt, Ct, Pt, sc, sp),
    "negbinom" = optim_negbinom(Dt, Rt, Ct, Pt, sc, sp)
  )
  if (optim_res$convergence != 0) warning("Model did not converge.")

  model <- c(optim_res$par, sc = sc, sp = sp, method = method, convergence = optim_res$convergence)

  list(model = model, predictions = optim_res$par[1] * Rt_nowcast)
}

#' Fit a mechanistic model to the data, returning a dadnow object
#' 
#' @param formula A formula object, *must* be of the form Dt ~ Ct + Pt + Rt.
#' @param data A data frame. Must contain the variables specified in the formula and in `date_col`. Trailing NA values in `y` will be nowcasted.
#' @param model The model to use for nowcasting. Currently implemented: "lm", "ar". Can be a vector, in which case the model is trained for each model in the vector.
#' @param test_size The proportion of the data to use for testing. If NULL, the data are not split. Defaults to 10% of the data.
#' @param params The parameters to use for the model. Must be a named list containing sc and sp and method (normal, poisson, or negbinom).
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#'
#' @returns A dadnow object with the mechanistic model added.
#' @export
nowcast_mechanistic <- function(
  formula, data, model, test_size = 0.1,
  params = list(sc = 0.2, sp = 0.3, method = "poisson"),
  date_col = NULL
) {

  prepped_data <- prep_data(
    formula, data, model, test_size, date_col = date_col
  )

  test_preds <- fit_mechanistic(
    Dt = prepped_data$y_train,
    Ct = prepped_data$X_train[, 1],
    Pt = prepped_data$X_train[, 2],
    Rt = prepped_data$X_train[, 3],
    Rt_nowcast = prepped_data$X_test[, 3],
    sc = params$sc,
    sp = params$sp,
    method = params$method
  )$predictions

  eval <- data.frame(
    "formula" = deparse(formula),
    "model" = "mechanistic",
    "params" = paste0(names(params), params, collapse = "_"),
    "rmse" = sqrt(mean((prepped_data$y_test - test_preds)^2)),
    "mae" = mean(abs(prepped_data$y_test - test_preds)),
    "mre" = mean(((prepped_data$y_test - test_preds) / (prepped_data$y_test + 0.1))^2)
  )

  Dt <- c(prepped_data$y_train, prepped_data$y_test)
  Ct <- c(prepped_data$X_train[, 1], prepped_data$X_test[, 1])
  Pt <- c(prepped_data$X_train[, 2], prepped_data$X_test[, 2])
  Rt <- c(prepped_data$X_train[, 3], prepped_data$X_test[, 3])

  Rt_nowcast <- prepped_data$X_nowcast[, 3]

  dadnow_mech <- fit_mechanistic(Dt, Ct, Pt, Rt, Rt_nowcast, params$sc, params$sp, params$method)
  dadnow <- list(
    model_id = "mechanistic",
    formula = "mechanistic",
    date_col = date_col,
    prepped_data = prepped_data,
    model = dadnow_mech$model,
    predictions = dadnow_mech$predictions,
    evals = eval,
    params = params
  )
  
  class(dadnow) <- "dadnow"
  dadnow
}

#' Add a mechanistic model to a dadnow or multidadnow object
#'
#' @param dadnow A dadnow or multidadnow object.
#' @param Dt,Ct,Pt,Rt The data correspoding to DAD, CNISP, PTSOS, and RVDSS, respectively.
#' @param Rt_nowcast The RVDSS data for the nowcast period. This is used to create the nowcast predictions for the mechanistic model.
#' @param sc,sp The scaling factors for the CNISP and PTSOS data.
#' @param method Either "normal", "poisson", or "negbinom".
#'
#' @returns A dadnow or multidadnow object with the mechanistic model added.
#' @export
add_mechanistic <- function(dadnow, Dt, Ct, Pt, Rt, Rt_nowcast, sc = 0.2, sp = 0.3, theta = c(1, 1.5, 0.5), method = "normal") {
  dadnow_mech <- nowcast_mechanistic(Dt, Ct, Pt, Rt, Rt_nowcast, sc, sp, method)
  
  dadnow <- combine_dadnow(dadnow, dadnow_mech)
  dadnow
}
