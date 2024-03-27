library(shiny)
library(ggplot2)
library(readr)
data <- read_csv("rawdata/songs_with_features.csv")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Logistic Function Explorer"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Slider input for slope parameter
      sliderInput("slope", 
                  "Curvature Parameter:", 
                  min = 15, 
                  max = 100, 
                  value = 26)
    ),
    
    # Show plot
    mainPanel(
      plotOutput("logistic_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Function to calculate logistic function
  logistic <- function(x, slope, intercept) {
    exp(intercept + slope*x) / (1 + exp(intercept + slope*x))
  }
  
  # Generate x values
  x <- seq(0, 1, by = 0.01)
  
  # Reactive expression for y values based on input$slope and adjusted intercept
  y <- reactive({
    # Adjust intercept to ensure crossing y at roughly 0.5
    intercept <- -input$slope * 0.5
    logistic(x, input$slope, intercept)
  })
  
  
  
  # Render plot
  output$logistic_plot <- renderPlot({
    ggplot(data.frame(x, y = y()), aes(x = x, y = y)) + 
      geom_line(linewidth = 1) +
      labs(x = "X", y = "p(X)") + 
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

