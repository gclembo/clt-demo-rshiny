library(shiny)
library(tidyverse)

data <- as.data.frame(EuStockMarkets) |>
  mutate(date = (time(EuStockMarkets)))

tidy_data <- as.data.frame(EuStockMarkets) |>
  mutate(date = (time(EuStockMarkets)))|>
  pivot_longer(
    cols = c("DAX", "SMI", "CAC", "FTSE"),
    names_to = "index",
    values_to = "price"
  )

time_data_plot <- tidy_data |>
  ggplot(aes(x = date, y = price)) +
  geom_line(aes(color = index))  +
  labs(title = "Closing Prices Over Time",
       x = "Year", y = "Price",
       color = "Stock Index") 


dist_data_plot <- tidy_data |>
  ggplot(aes(x = price)) +
  geom_histogram(bins = 40, fill = "#9999FF") +
  facet_wrap(~index) +
  labs(title = "Closing Price Distribution for Indeces",
       x = "Price", y = "Count")


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cyborg"),
  titlePanel("Central Limit Threorem Demonstrated by EU Stock Prices"),
  p(
    "This app is intended to demonstrate the central limit theorem using data
    from the built in R dataset EuStockMarkets containing information about
    daily closing prices of major European stock indices, 1991–1998. The drop
    down menu changes which stock index is being sampled from. The 
    slider alows the sample size to change and a distribution of 1000 samples
    is displayed on the right. Along with this is a Shapiro-Wilk normality 
    test which gives information on how normal the distribution is. The mean of
    the samples is displayed on the left along with the means for each index."
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("index", "Stock Index", choices = c("DAX", "SMI", "CAC", "FTSE")),
      sliderInput("size", "Sample Size", min = 1, max = 100, value = 5),
      textOutput("dist_mean"),
      br(),
      
    ),
    mainPanel(
      br(),
      plotOutput("samples"),
      "Shapiro-Wilk Normality Test",
      verbatimTextOutput("normal_test")
    )
  ),
  p(
    "The price distributions for each stock index is shown below."
  ),
  sidebarLayout(
    position = 'right',
    sidebarPanel(width = 3,
      tableOutput("means")
    ),
    mainPanel(
      plotOutput("dist_data")
    )
  ),
  plotOutput("time_data")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny()
  selection <- reactive({
    data |>
      select(input$index) |>
      unlist(use.names = FALSE)
  })

  samples <- reactive({
    replicate(
      1e3, sample(selection(), input$size) |> mean()
    )
  })

  output$samples <- renderPlot({
    df <- data.frame("price" = samples())
    df |> ggplot(aes(x = price)) +
      geom_histogram(bins = 30, fill = "#DD55DD") +
      labs(title = "Distribution of 1000 Samples",
           x = "Price", y = "Count")
  })

  output$dist_mean <- renderText({
    paste0("Mean of Samples: ", mean(samples()))
  })
  
  output$normal_test <- renderText({
    test <- shapiro.test(samples())
    paste0("W = ", test$statistic, ", p-value = ", test$p.value)
  })
  


  output$means <- renderTable(spacing = 'm', width = '100%', { 
    tidy_data |>
      group_by(index) |>
      summarise(mean = mean(price))
  })

  output$dist_data <- renderPlot({
    dist_data_plot
  })

  output$time_data <- renderPlot({
    time_data_plot
  })
}

# Run the application
shinyApp(ui = ui, server = server)
