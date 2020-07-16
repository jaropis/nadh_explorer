library(shiny)
library(plotly)
library(dplyr)
library(forecast)
source('draw_decomp.R')
unit <- 10^-3 # ms in this case
delta <- 40 # this is basically the sampling period
ui <- fluidPage(
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
            
            # Input: Select number of rows to display ----
            radioButtons("method", "Decomposition method",
                         choices = c("additive",
                                     "multiplicative"
                                     ),
                         selected = "additive"),
            
            shiny::checkboxInput("show_decompositions", "Show main decomposition", value = FALSE)
            
        ),
        
        mainPanel(
            tags$h5("The first three figures are respectively the raw fluorescence signal, the trend remaining after attempting to identify and subtract oscillations and the oscillations themselves"),
            tags$h5("The remaining two figures are the Fourier transform of the above oscillatory part and the Fourier transform of the fluorescence signal"),
            plotly::plotlyOutput('decompositions'),
            tags$h4(textOutput('description')),
            verbatimTextOutput("selecting"),
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
    
    selected_data <- reactive({})
    
    output$decompositions <- plotly::renderPlotly({
        req(uploaded_data())
        draw_decompositions(uploaded_data(), type = input$method, input$show_decompositions)
    })
    
    observe({
        req(uploaded_data())
        d <- event_data("plotly_brushed", source = "decompositions")
        if (is.null(d)) print("Brush points appear here (double-click to clear)") else print(d)
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