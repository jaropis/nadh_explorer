library(shiny)
library(plotly)
library(dplyr)
library(forecast)
source('draw_decomp.R')
unit <- 10^-3 # ms in this case
delta <- 40 # this is basically the sampling period
ui <- fluidPage(
    tags$head(HTML(
        "<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=UA-21860092-5'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-21860092-5');
</script>
"
    )),
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
                         selected = "additive")
            
        ),
        
        mainPanel(
            tags$h5("The first three figures are respectively the raw fluorescence signal, the trend remaining after attempting to identify and subtract oscillations and the oscillations themselves"),
            tags$h5("The remaining two figures are the Fourier transform of the above oscillatory part and the Fourier transform of the fluorescence signal"),
            plotly::plotlyOutput('decompositions'),
            tags$h4(textOutput('description')),
            tags$br(),
            plotly::plotlyOutput('fourier_oscyl'),
            plotly::plotlyOutput('fourier')
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
    
    output$decompositions <- plotly::renderPlotly({
        draw_decompositions(uploaded_data(), type = input$method)
    })
    
    output$description <- renderText({
        sprintf("The dominant frequency in this recording is %f Hz or %f bpm", 
                round(1 / (unit * get_dominant_frequency(uploaded_data()) * delta), 4),
                round((unit * get_dominant_frequency(uploaded_data())) * delta / 60, 4))
    })
    
    output$fourier <- plotly::renderPlotly({
        draw_fourier(uploaded_data(), type = input$method)
    })
    
    output$fourier_oscyl <- plotly::renderPlotly({
        draw_fourier(uploaded_data(), type = input$method, decomp = TRUE)
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)