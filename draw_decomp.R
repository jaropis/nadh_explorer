get_dominant_frequency <- function(data_ts) {
  # only looking in the latter part of the line, where oscillations are pronounced
  data_length <- nrow(data_ts)
  n_two_thirds <- round(data_length * 4 / 5)
  findfrequency(data_ts[[2]][n_two_thirds:data_length])
}
decompose_ts <- function(data_ts, type = "mult") {
  freq <- get_dominant_frequency(data_ts)
  new_data_ts <- ts(data_ts[[2]], frequency = freq)
  decomposed <- decompose(new_data_ts, type = type)
  data.frame(trend = decomposed$trend, oscyl = decomposed$seasonal)
}
draw_decompositions <- function(data, type = "mult") {
  main <- data %>% 
    plot_ly(x = ~time, y = ~fluorescence) %>% 
    add_lines() %>% 
    layout(xaxis = list(title = "time"), yaxis = list(title = "recording"))
  decomp <- decompose_ts(data, type)
  trend <- data.frame(time = data[[1]], trend = decomp$trend)
  oscillations <- data.frame(time = data[[1]], oscyl = decomp$oscyl)
  trend_plot <- trend %>% 
    plot_ly(x = ~time, y = ~trend) %>% 
    add_lines()
  oscyl_plot <- oscillations %>% 
    plot_ly(x = ~time, y = ~oscyl) %>% 
    add_lines()
  subplot(main, trend_plot, oscyl_plot, nrows = 3, shareX= TRUE, titleX = TRUE, titleY = TRUE) %>% 
    hide_legend()
}

draw_fourier <- function(data, type = "multi") {
  decomp <- decompose_ts(data, type)
  spect <- spectrum(decomp$oscyl - mean(decomp$oscyl))
  spect_plot <- data.frame(freq = spect$freq, spec = spect$spec)
  spect_plot %>%
    plot_ly(x = ~log(freq), y = ~spec) %>% 
    add_lines() %>% 
    hide_legend()
}