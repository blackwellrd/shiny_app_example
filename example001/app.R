library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Sidebar layout 
    sidebarLayout(
        sidebarPanel(
          numericInput(inputId = 'numMean', label = 'Mean', 
                       min = 0, max = 100,
                       value = 0, step = 1),
          numericInput(inputId = 'numSD', label = 'SD', 
                       min = 0, max = 50,
                       value = 1, step = 0.1),
          numericInput(inputId = 'numSize', label = 'Size', 
                       min = 0, max = 10000,
                       value = 10, step = 10),
          width = 2
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput(outputId = 'pltSample', width = '100%', height = '900px')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$pltSample <- renderPlotly({
    x <- rnorm(n = input$numSize, mean = input$numMean, sd = input$numSD)
    plot_ly(x = ~x, type = "histogram")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
