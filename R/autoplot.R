#' Plot the nowcasts
#' 
#' @importFrom ggplot2 autoplot
#' @export
autoplot.dadnow <- function(dadnow) {
  plot_data <- data.frame(x = dadnow$dates_train, y = dadnow$y_train, method = NA)
  

  nowcast_models <- names(dadnow)[grepl("nowcast_", names(dadnow))]
  if (length(nowcast_models) > 0) {
    for (i in seq_len(length(nowcast_models))) {
      plot_data <- rbind(
        plot_data,
        data.frame(
          x = dadnow$dates_nowcast,
          y = dadnow[[nowcast_models[i]]],
          method = gsub("nowcast_", "", nowcast_models[i])
        )
      )
    }
  }

  print(plot_data$y)

  ggplot2::ggplot(data = plot_data) +
    ggplot2::aes(x = x, y = y, colour = method) +
    ggplot2::geom_line()
}