library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(rsample)
library(plotly)


data <- read_csv("rawdata/songs_with_features.csv") %>% 
  filter(category %in% c("Klassik", "Dance/Electronic")) %>% 
  select(track.name, track.artist, category, energy, danceability) %>% 
  mutate(category = case_when(
    category == "Klassik" ~ "Klassik",
    category == "Dance/Electronic" ~ "EDM"
  )) %>% 
  mutate(edm = case_when(
    category == "EDM" ~ 1,
    category == "Klassik" ~ 0
  )) %>% 
  mutate(edm_factor = as.factor(edm)) %>% 
  filter(category %in% c("Klassik", "EDM"))  %>% 
  filter(energy > 0.05 & energy < 0.95) 




# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Logistic Explorer"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Slider input for slope parameter
      sliderInput("slope", 
                  "Curvature Parameter:", 
                  min = 1, 
                  max = 100, 
                  value = 26),
      sliderInput("n", "Number of samples", min = 10, max = 1000, value = 100, step = 10),
      sliderInput("C", "Threshold value", min = 0.001, max = 0.999, value = 0.5),
      textOutput("accuracy")
    ),

    # Show plot
    mainPanel(
      plotlyOutput("logistic_plot"),
      plotOutput("confusion")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  
  sample <- reactive({
    set.seed(1)
    data %>% 
      sample_n(input$n)
    
  })
  
  
  # Function to calculate logistic function
  logistic <- function(x, slope, intercept) {
    exp(intercept + slope*x) / (1 + exp(intercept + slope*x))
  }
  
  # Generate x values
  
  # Reactive expression for y values based on input$slope and adjusted intercept
  y <- reactive({
    data_tmp <- sample()
    x <- seq(0, 1, length.out = input$n)
    # Adjust intercept to ensure crossing y at roughly 0.5
    intercept <- -input$slope * 0.5
    logistic(x, input$slope, intercept)
  })
  
  
  
  # Render plot
  output$logistic_plot <- renderPlotly({
 #   x <- seq(0, 1, length.out = input$n)
    data_tmp <- sample()
    x <- seq(0, 1, length.out = input$n)
    data_for_plot <- cbind(data_tmp, tibble(x = x, y = y())) %>% 
      mutate(pred = logistic(energy, input$slope, -input$slope * 0.5)) %>% 
      mutate(pred_class = ifelse(pred > input$C, 1, 0)) %>% 
      mutate(pred_class = factor(pred_class)) %>% 
      mutate(diff = (input$C - y())^2)
    
    min_diff <- (min(data_for_plot$diff))
    
    x_intersection <- data_for_plot %>% 
      filter(diff == min_diff) %>%
      pull(x)
    
    x_intersection <- x_intersection[1]

    
    plot <- ggplot(data_for_plot) + 
      geom_line(aes(x = x, y = y) , linewidth = 1) +
      geom_point(aes(x = energy, y = edm, color = pred_class), size = 3) +
      labs(x = "Energy", y = "EDM", color = "Vorhersage") + 
      geom_hline(yintercept = input$C, linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = x_intersection, linetype = "dashed", alpha = 0.5) +
      theme_minimal()
    ggplotly(plot)
  })
  
  output$predictions <- renderDataTable({
 #   x <- seq(0, 1, length.out = input$n)
    data_tmp <- sample()
    x <- seq(0, 1, length.out = input$n)
    data_for_plot <- cbind(data_tmp, tibble(x = x, y = y()))
    data_for_plot <- data_for_plot %>% 
      mutate(pred = logistic(energy, input$slope, -input$slope * 0.5)) %>% 
      mutate(pred_class = ifelse(pred > input$C, 1, 0)) %>% 
      rename(p_edm = pred) %>% 
      select(track.name, track.artist,  energy, p_edm, edm, pred_class)  %>% 
      mutate(p_edm = round(p_edm, 5)) %>% 
      arrange(energy)
    data_for_plot
  })
  
  output$confusion <- renderPlot({
    x <- seq(0, 1, length.out = input$n)
    data_tmp <- sample()
    data_for_plot <- cbind(data_tmp, tibble(x = x, y = y()))
    predictions <- data_for_plot %>% 
      mutate(pred = logistic(energy, input$slope, -input$slope * 0.5)) %>% 
      mutate(pred_class = ifelse(pred > input$C, 1, 0)) %>% 
      rename(p_edm = pred) %>% 
      mutate(pred_class = factor(pred_class)) %>% 
      mutate(p_edm = round(p_edm, 5)) %>% 
      arrange(energy)
    
    yardstick::conf_mat(predictions, truth = edm_factor, estimate = pred_class) %>% 
      autoplot(type = "heatmap") +
      labs(x = "Wahrheit", y = "Vorhersage") + 
      ggtitle("Konfusionsmatrix")
    
    
  })
 
  output$accuracy <- renderText({
    x <- seq(0, 1, length.out = input$n)
    data_tmp <- sample()
    data_for_plot <- cbind(data_tmp, tibble(x = x, y = y()))
    predictions <- data_for_plot %>% 
      mutate(pred = logistic(energy, input$slope, -input$slope * 0.5)) %>% 
      mutate(pred_class = ifelse(pred > input$C, 1, 0)) %>% 
      rename(p_edm = pred) %>% 
      mutate(pred_class = factor(pred_class)) %>% 
      mutate(p_edm = round(p_edm, 5)) %>% 
      arrange(energy)
    
    accuracy <- yardstick::accuracy(predictions, truth = edm_factor, estimate = pred_class) %>% 
      pull(.estimate)
    
    paste("Accuracy: ", 100*round(accuracy, 2), "%")
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

