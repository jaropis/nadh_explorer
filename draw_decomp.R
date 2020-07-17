#' Function to try and read the dominant frequency off of the recording
#' @param data_ts time series
get_dominant_frequency <- function(data_ts) {
  # only looking in the latter part of the line, where oscillations are pronounced
  data_length <- nrow(data_ts)
  n_two_thirds <- round(data_length * 8 / 9)
  findfrequency(data_ts[[2]][n_two_thirds:data_length])
}

#' Function decomposing a recording into trend and oscillatory part
#' @param data_ts time series data
#' @param type decomposition type (additive or multiplicative)
decompose_ts <- function(data_ts, type = "additive") {
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

#' function to draw fourier spectrum
#' @param flour_data data for fourier analysis
#' @param type type of decomposition, if we are interested in drawing only the oscillatory part 
#' @param decomp whether or not to use decomposition
draw_fourier <- function(fluor_data, type = "additive", decomp = FALSE) {
  if (decomp) {
    decomp <- decompose_ts(fluor_data, type)
    spect <- spectrum(decomp$oscyl - mean(decomp$oscyl), plot = FALSE)
    main_title <- 'Fourier spectrum of the oscillatory part'
  } else {
    spect <- spectrum(fluor_data$fluorescence - mean(fluor_data$fluorescence), plot = FALSE)
    main_title <- "Raw Fourier spectrum of the fluorescence signal"
  }
  
  freq <- seq_along(spect$freq) *  1/(fluor_data$time[length(fluor_data$time)] - fluor_data$time[1])
  spect_plot <- data.frame(freq = freq, spec = spect$spec)
  draw_spect_plot(spect_plot, main_title)
}

#' @param data data
#' @param type type of decomposition into trend and seasonal part - multiplicative or additive
#' @param smoothing_dgr - how much data should be smoothed by MA, from 1 to 50 (the most smoothing)
draw_selections <- function(data, trend, oscillations) {
  main <- data %>% 
    plot_ly(x = ~time, y = ~fluorescence, hoverinfo = "x+y", source = "decompositions") %>% 
    add_lines() %>% 
    add_markers(x = data$time[[1]], y = data$fluorescence[[1]], size = I(1), color = I("blue"), hoverinfo = 'skip') %>% # it is really strange that this is needed here ... otherwise no select button ... strange
    layout(xaxis = list(title = "time (s)"), yaxis = list(title = "fluorescence"))
  trend_plot <- trend %>% 
    plot_ly(x = ~time, y = ~trend, hoverinfo = "x+y") %>% 
    add_lines() %>% 
    layout(yaxis = list(title = "trend"))
  oscyl_plot <- oscillations %>% 
    plot_ly(x = ~time, y = ~oscyl, hoverinfo = "x+y") %>% 
    add_lines() %>% 
    layout(yaxis = list(title = "oscillatory\npart"))
  subplot(main, trend_plot, oscyl_plot, nrows = 1, shareY= FALSE, titleX = TRUE, titleY = TRUE) %>% 
    hide_legend() 
}

#' Function to calculate trend and oscillations by MA
#' @param data data to be divided into trend and oscillations
#' @param smooth_dgr degree of smoothing for MA filter
calc_trend_and_oscyl <- function(data, smoothing_dgr) {
  smooth <- stats::filter(data[[2]], sides=2, filter=rep(1/smoothing_dgr, smoothing_dgr))
  trend <- data.frame(time = data[[1]], trend = smooth)
  oscillations <- data.frame(time = data[[1]], oscyl = data[[2]] - trend[["trend"]])
  list(trend = trend, oscillations = oscillations)
}

get_fourier <- function(data) {
  data = data[complete.cases(data), ]
  spect <- spectrum(data[[2]] - mean(data[[2]]), plot = FALSE)
  freq <- seq_along(spect$freq) *  1/(data[[1]][length(data[[1]])] - data[[1]][1]) # the last value of time track minus the first value of time track
  data.frame(freq = freq, spec = spect$spec)
}

draw_spect_plot <- function(spect_frame, main_title, line_color = "blue") {
  spect_frame = spect_frame[complete.cases(spect_frame), ]
  spect_frame$text <- paste(as.character(round(spect_frame$freq, 3)), "Hz")
  spect_frame %>%
    plot_ly(x = ~log(freq), y = ~spec, text = ~text, hoverinfo = "text") %>% 
    add_lines(color = I(line_color)) %>% 
    hide_legend() %>% 
    layout(title = main_title, yaxis = list(title = "spectrum"))
}