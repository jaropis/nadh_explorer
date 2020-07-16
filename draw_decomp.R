get_dominant_frequency <- function(data_ts) {
  # only looking in the latter part of the line, where oscillations are pronounced
  data_length <- nrow(data_ts)
  n_two_thirds <- round(data_length * 8 / 9)
  findfrequency(data_ts[[2]][n_two_thirds:data_length])
}
decompose_ts <- function(data_ts, type = "mult") {
  freq <- get_dominant_frequency(data_ts)
  new_data_ts <- ts(data_ts[[2]], frequency = freq)
  decomposed <- decompose(new_data_ts, type = type)
  data.frame(trend = decomposed$trend, oscyl = decomposed$seasonal)
}

#' @param data data
#' @param type type of decomposition into trend and seasonal part - multiplicative or additive
#' @param show_decompositions - whether or not the dominant frequency decomposition should be shown
draw_decompositions <- function(data, type = "mult", show_decompositions) {
  main <- data %>% 
    plot_ly(x = ~time, y = ~fluorescence, hoverinfo = "x+y", source = "decompositions") %>% 
    add_lines() %>% 
    add_markers(x = data$time[[1]], y = data$fluorescence[[1]], size = I(1), color = I("blue"), hoverinfo = 'skip') %>% # it is really strange that this is needed here ... otherwise no select button ... strange
    layout(xaxis = list(title = "time (s)"), yaxis = list(title = "fluorescence")) %>% 
    event_register("plotly_brushed")
  if (show_decompositions) {
    decomp <- decompose_ts(data, type)
    trend <- data.frame(time = data[[1]], trend = decomp$trend)
    oscillations <- data.frame(time = data[[1]], oscyl = decomp$oscyl)
    trend_plot <- trend %>% 
      plot_ly(x = ~time, y = ~trend, hoverinfo = "x+y") %>% 
      add_lines() %>% 
      layout(yaxis = list(title = "trend"))
    oscyl_plot <- oscillations %>% 
      plot_ly(x = ~time, y = ~oscyl, hoverinfo = "x+y") %>% 
      add_lines() %>% 
      layout(yaxis = list(title = "oscillatory\npart"))
    subplot(main, trend_plot, oscyl_plot, nrows = 3, shareX= TRUE, titleX = TRUE, titleY = TRUE) %>% 
      hide_legend() 
  } else {
    main %>% 
      hide_legend()
  }
}

draw_fourier <- function(fluor_data, type = "multi", decomp = FALSE) {
  if (decomp) {
    decomp <- decompose_ts(fluor_data, type)
    spect <- spectrum(decomp$oscyl - mean(decomp$oscyl), plot = FALSE)
    main_title <- 'Fourier spectrum of the oscillatory part'
  } else {
    spect <- spectrum(fluor_data$fluorescence - mean(fluor_data$fluorescence), plot = FALSE)
    main_title <- "Raw Fourier spectrum of the fluorescence signal"
  }
  
  freq <- seq_along(spect$freq) *  1/fluor_data$time[length(fluor_data$time)]
  spect_plot <- data.frame(freq = freq, spec = spect$spec, text = paste(as.character(round(freq, 3)), "Hz"))
  spect_plot %>%
    plot_ly(x = ~log(freq), y = ~spec, text = ~text, hoverinfo = "text") %>% 
    add_lines() %>% 
    hide_legend() %>% 
    layout(title = main_title, yaxis = list(title = "spectrum"))
}