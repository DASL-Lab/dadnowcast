#' Add a model to an existing dadnow or multidadnow object
#' 
#' @param x A dadnow or multidadnow object.
#' @param formula A formula object.
#' @param model The model to use for nowcasting. Currently implemented: "lm", "ar". Can be a vector, in which case the model is trained for each model in the vector.
#' @param params The parameters to use for the model. Must be a named list.
#'
#' @returns A dadnow or multidadnow object with the model added.
#' @export
add_nowcast <- function(multidadnow, model, formula = NULL, params = NULL) {
  
  # If the multidadnow object has already been fit to data, the "data" already has nowcasts in it. 
  # This removes the nowcasts, but keeps the training data (which includes the data to be nowcast, so "training" is a bit of a misnomer).
  if ("model" %in% names(multidadnow$data)) {
    model_data <- multidadnow$data[multidadnow$data$model == "Training", ]
  } else {
    model_data <- multidadnow$data
  }

  if (is.null(formula)) {
    formula <- multidadnow$models[[1]]$formula
    message(paste0("Using formula from first registered model: ", deparse(formula)))
  }
  
  # Fit a new nowcasting model, using anuthing from the multidadnow object that is relevant.
  # Ensures consistency between nowcasts.
  new_dadnow <- nowcast(
    formula = formula,
    data = model_data,
    model = model,
    params = params,
    date_col = multidadnow$date_col,
    batches = multidadnow$batches,
    train_window = multidadnow$train_window,
    level = multidadnow$level
  )
  
  dadnow <- combine_nowcasts(multidadnow, new_dadnow)
  dadnow
}

