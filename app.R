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
                         selected = "additive")
            
        ),
        
        mainPanel(
            plotly::plotlyOutput('decompositions'),
            tags$h4(textOutput('description')),
            tags$br(),
            plotly::plotlyOutput('fourier')
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output) {
    # reactive conductor with data
    uploaded_data <- reactive({
        req(input$file1)
        df <- read.csv2(input$file1$datapath,
                       header = TRUE,
                       sep = ";")
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
    
}

# Create Shiny app ----
shinyApp(ui, server)