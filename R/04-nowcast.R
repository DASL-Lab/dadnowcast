# Plan:
# Check all trained models
  # Some of them might contain nowcasting as part of the algorithm; in those cases I can just return the nowcasts so that the user has a consistent interface.
  # Most will require nowcasting, in which case dispatch to the relevant functions as necssary.

#' Nowcast from a dadnow object
#'
#' @param dadnow A dadnow object.
#' @param quiet If TRUE, suppresses output.
#'
#' @returns A dadnow object with the nowcasts added.
#' @export
nowcast <- function(dadnow, quiet = FALSE) {
  # Check the model is valid
  if (!all(dadnow$model %in% c(
    "lm", "ar"
  ))) {
    stop("Invalid model.")
  }

  for (model in dadnow$model) {
    res <- switch(model,
      "lm" = nowcast_lm(dadnow$trained_lm, dadnow$X_nowcast),
      "ar" = nowcast_ar(dadnow$trained_ar, dadnow$X_nowcast, dadnow$order)
    )
    dadnow[[paste0("nowcasted_", model)]] <- res
  }
  dadnow
}

