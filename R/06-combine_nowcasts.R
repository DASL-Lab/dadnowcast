
#' Add a dadnow to a multidadnow object, or combine two multidadnow objects
#'
#' @param dadnow1,dadnow2 A dadnow or multidadnow object.
#'
#' @returns A single multidadnow object.
#' @export
combine_nowcasts <- function(dadnow1, dadnow2) {

  # Compare the data, response, batches, train_window, and level.
  # If these don't match, there's no reason too store them in the same object.
  d1 <- c(response = dadnow1$response, batches = dadnow1$batches, train_window = dadnow1$train_window, level = dadnow1$level)
  d2 <- c(response = dadnow2$response, batches = dadnow2$batches, train_window = dadnow2$train_window, level = dadnow2$level)

  if (any(d1 != d2)) {
    stop("Response, batches, train_window, and level must be the same for all nowcasts.")
  }

  # Combine the data, ensuring that there are no duplicate rows.
  data <- rbind(dadnow1$data, dadnow2$data)
  data <- data[!duplicated(data), ]

  # Combine the evaluations, renaming the models as necessary.
  evals1 <- dadnow1$evals
  evals2 <- dadnow2$evals
  rownames(evals1) <- NULL
  rownames(evals2) <- NULL
  evals <- rbind(evals1, evals2)
  model_ids <- make_model_id(evals)
  rownames(evals) <- model_ids

  # Combine the lists of models
  models <- c(dadnow1$models, dadnow2$models)

  return_value <- list(
    date_col = dadnow1$date_col,
    data = data,
    evals = evals,
    response = dadnow1$response,
    batches = dadnow1$batches,
    train_window = dadnow1$train_window,
    level = dadnow1$level,
    models = models
  )
  class(return_value) <- "multidadnow"

  # Ensure names match again, updating the model ids inside the models.
  names(return_value$models) <- model_ids
  for (i in seq_along(return_value$models)) {
    return_value$models[[i]]$model_id <- model_ids[i]
  }
  
  return(return_value)
}


