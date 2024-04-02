library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(yardstick)

set.seed(1)
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
  filter(energy > 0.05 & energy < 0.95)  %>% 
  sample_n(100)


slope <- 26

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Logistic Explorer"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Slider input for slope parameter
      # sliderInput("slope", 
      #             "Curvature Parameter:", 
      #             min = 1, 
      #             max = 100, 
      #             value = 26),
     # sliderInput("n", "Number of samples", min = 10, max = 1000, value = 100, step = 10),
      sliderInput("C", "Threshold value C", min = 0.00001, max = 0.9999, value = 0.5),
      tableOutput("accuracy")
    ),

    # Show plot
    mainPanel(
      plotOutput("logistic_plot"),
      plotOutput("confusion")
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
  
  # Reactive expression for y values based on slope and adjusted intercept
  y <- reactive({
    data_tmp <- data
    x <- seq(0, 1, length.out = 100)
    # Adjust intercept to ensure crossing y at roughly 0.5
    intercept <- -slope * 0.5
    logistic(x, slope, intercept)
  })
  
  
  
  # Render plot
  output$logistic_plot <- renderPlot({
 #   x <- seq(0, 1, length.out = 100)
    data_tmp <- data
    x <- seq(0, 1, length.out = 100)
    data_for_plot <- cbind(data_tmp, tibble(x = x, y = y())) %>% 
      mutate(pred = logistic(energy, slope, -slope * 0.5)) %>% 
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
      geom_point(aes(x = energy, y = edm, color = pred_class), size = 5) +
      labs(x = "Energy", y = "EDM", color = "Vorhersage") + 
      geom_hline(yintercept = input$C, linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = x_intersection, linetype = "dashed", alpha = 0.5) +
      theme_minimal()
    plot
  })
  
 #  output$predictions <- renderDataTable({
 # #   x <- seq(0, 1, length.out = 100)
 #    data_tmp <- data
 #    x <- seq(0, 1, length.out = 100)
 #    data_for_plot <- cbind(data_tmp, tibble(x = x, y = y()))
 #    data_for_plot <- data_for_plot %>% 
 #      mutate(pred = logistic(energy, slope, -slope * 0.5)) %>% 
 #      mutate(pred_class = ifelse(pred > input$C, 1, 0)) %>% 
 #      rename(p_edm = pred) %>% 
 #      select(track.name, track.artist,  energy, p_edm, edm, pred_class)  %>% 
 #      mutate(p_edm = round(p_edm, 5)) %>% 
 #      arrange(energy)
 #    data_for_plot
 #  })
  
  output$confusion <- renderPlot({
    x <- seq(0, 1, length.out = 100)
    data_tmp <- data
    data_for_plot <- cbind(data_tmp, tibble(x = x, y = y()))
    predictions <- data_for_plot %>% 
      mutate(pred = logistic(energy, slope, -slope * 0.5)) %>% 
      mutate(pred_class = ifelse(pred > input$C, 1, 0)) %>% 
      rename(p_edm = pred) %>% 
      mutate(pred_class = factor(pred_class)) %>% 
      mutate(p_edm = round(p_edm, 5)) %>% 
      arrange(energy)
    
    conf_mat(predictions, truth = edm_factor, estimate = pred_class) %>% 
      autoplot(type = "heatmap") +
      labs(x = "Wahrheit", y = "Vorhersage") + 
      ggtitle("Konfusionsmatrix")
    
    
  })
 
  output$accuracy <- renderTable({
    x <- seq(0, 1, length.out = 100)
    data_tmp <- data
    data_for_plot <- cbind(data_tmp, tibble(x = x, y = y()))
    predictions <- data_for_plot %>% 
      mutate(pred = logistic(energy, slope, -slope * 0.5)) %>% 
      mutate(pred_class = ifelse(pred > input$C, 1, 0)) %>% 
      rename(p_edm = pred) %>% 
      mutate(pred_class = factor(pred_class)) %>% 
      mutate(p_edm = round(p_edm, 5)) %>% 
      arrange(energy)
    
    
    multi_metric <- metric_set(accuracy, recall, precision)
    
    multi_metric(predictions, truth = edm_factor, estimate = pred_class, event_level = "second") %>% 
      select(-.estimator) %>%
      rename(metric = .metric,
             value = .estimate) %>%
      mutate(value = paste0(100*round(value, 4),'%')) 
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

