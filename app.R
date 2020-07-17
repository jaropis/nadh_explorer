library(shiny)
library(plotly)
library(dplyr)
library(forecast)
source('draw_decomp.R')
unit <- 10^-3 # ms in this case
delta <- 40 # this is basically the sampling period
ui <- fluidPage(
    #tags$head(HTML()), # put tracking code here
    # App title ----
    titlePanel("NADH explorer"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Selecting smoothing degree
            sliderInput("smoothing_dgr", "Select degree of smoothing", min = 1, max = 50, value = 20),
            
            tags$hr(),
            
            # Selecting decomposition method ----
            radioButtons("method", "Decomposition method",
                         choices = c("additive",
                                     "multiplicative"
                                     ),
                         selected = "additive"),
            
            shiny::checkboxInput("show_decompositions", "Show main decomposition", value = FALSE)
            
        ),
        
        mainPanel(
            #tags$h5("The first three figures are respectively the raw fluorescence signal, the trend remaining after attempting to identify and subtract oscillations and the oscillations themselves"),
            #tags$h5("The remaining two figures are the Fourier transform of the above oscillatory part and the Fourier transform of the fluorescence signal"),
            plotly::plotlyOutput('decompositions'),
            tags$h4(textOutput('description')),
            plotly::plotlyOutput('selections'),
            fluidRow(
                column(6, {
                    plotly::plotlyOutput('fourier_selection_raw')
                }),
                column(6, {
                    plotly::plotlyOutput('fourier_selection_oscyl')
                })
            ),
            tags$br(),
            plotly::plotlyOutput('fourier'),
            plotly::plotlyOutput('fourier_oscyl')
        )
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    # reactive conductor with data
    uploaded_data <- reactive({
        req(input$file1)
        df <- read.csv(input$file1$datapath,
                       header = TRUE,
                       sep = " ")
        names(df) <- c("time", "fluorescence", "pressure")
        df$time <- (df$time - df$time[1]) * unit 
        df
    })
    
    # this reactive conductor will hold selections
    selected_data <- reactive({
        req(uploaded_data())
        selection <- event_data("plotly_brushed", source = "decompositions")
        uploaded_data()[uploaded_data()[[1]] >= selection$x[1] & uploaded_data()[[1]] <= selection$x[[2]], ]
        })
    
    # this reactive conductor will hold trend and oscyllations
    trend_oscyl <- reactive({
        req(nrow(selected_data()) > 1)
        calc_trend_and_oscyl(selected_data(), input$smoothing_dgr)
    })
    
    output$decompositions <- plotly::renderPlotly({
        req(uploaded_data())
        draw_decompositions(uploaded_data(), type = input$method, input$show_decompositions)
    })
    
    output$description <- renderText({
        if (input$show_decompositions) {
            sprintf("The dominant frequency in this recording is %f Hz or %f bpm", 
                    round(1 / (unit * get_dominant_frequency(uploaded_data()) * delta), 4),
                    round((unit * get_dominant_frequency(uploaded_data())) * delta / 60, 4))
        } else {
            NULL
        }
    })
    
    output$selections <- plotly::renderPlotly({
        req(nrow(selected_data()) > 1)
        draw_selections(selected_data(), trend_oscyl()[["trend"]], trend_oscyl()[["oscillations"]])
    })
    
    output$fourier_selection_raw <- plotly::renderPlotly({
        req(nrow(selected_data()) > 1)
        selected_data() %>% 
            get_fourier() %>% 
            draw_spect_plot(main_title = "Raw", line_color = "blue")
    })
    
    output$fourier_selection_oscyl <- plotly::renderPlotly({
        req(trend_oscyl)
        trend_oscyl()[["oscillations"]] %>% 
            get_fourier() %>% 
            draw_spect_plot(main_title = "Oscill", line_color = "red")
    })
    
    output$fourier <- plotly::renderPlotly({
        draw_fourier(uploaded_data(), type = input$method)
    })
    
    output$fourier_oscyl <- plotly::renderPlotly({
        if (input$show_decompositions) {
            draw_fourier(uploaded_data(), type = input$method, decomp = TRUE)
        } else {
            NULL
        }
    })
}

# Create Shiny app ----
shinyApp(ui, server)