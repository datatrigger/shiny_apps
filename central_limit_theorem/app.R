library(shiny)
library(tidyverse)

ev <- list()
variance <- list()
ev['Uniform'] <- 0.5
variance['Uniform'] <- 1/12
ev['Cauchy'] <- 0
variance['Cauchy'] <- 1
ev['Exponential'] <- 1
variance['Exponential'] <- 1
ev['Log-normal'] <- exp(1/2)
variance['Log-normal'] <- (exp(1)-1)*exp(1)
ev['Binomial'] <- 0.5
variance['Binomial'] <- 0.25
ev['Poisson'] <- 1
variance['Poisson'] <- 1

distributions <- names(ev)

means <- function(distribution, size_sample, number_samples){
  if(distribution == 'Uniform'){
    random_generator <- function(x) runif(n = x)
  }
  if(distribution == 'Cauchy'){
    random_generator <- function(x) rcauchy(n = x)
  }
  if(distribution == 'Exponential'){
    random_generator <- function(x) rexp(n = x)
  }
  if(distribution == 'Log-normal'){
    random_generator <- function(x) rlnorm(n = x)
  }
  if(distribution == 'Binomial'){
    random_generator <- function(x) rbinom(n = x, size = 1, prob = 0.5)
  }
  if(distribution == 'Poisson'){
    random_generator <- function(x) rpois(n = x, lambda = 1)
  }
  
  rep(size_sample, number_samples) %>%
    map(random_generator) %>%
    map(mean) %>%
    unlist() %>%
    tibble() %>%
    rename(
      means = 1
    ) %>%
    mutate(
      dist = distribution,
      ev = ev[[distribution]],
      variance = variance[[distribution]],
      means = (means - ev)/sqrt(variance/size_sample)
    ) %>%
    select(
      dist,
      means
    ) %>%
    return()
}

ui <- fluidPage(
  titlePanel(
    title = h1("Visualizing the Central Limit Theorem", align = "center"),
    windowTitle = 'Shining CLT'
  ),
  hr(),
  HTML(
    "<h4 style=\"text-align:center\">
      <span style=\"color:#6495ED\">
        Histogram of the standardized means
      </span>
      <em>
        VS
      </em>
      <span style=\"color:red\">
        Standard normal distribution
      </span>
    </h4>"
    ),
  br(),
  fluidRow(
    plotOutput("plot")
  ),
  br(),
  br(),
  fluidRow(
    column(
      4,
      align = 'center',
      wellPanel(
        sliderInput(
          inputId = "size_sample",
          label = "Size of each sample",
          value = 1000,
          min = 1,
          max = 10000,
          step = 10
        )
      )
    ),
    column(
      4,
      align = 'center',
      wellPanel(
        sliderInput(
          inputId = "number_samples",
          label = "Number of samples, i.e computed means",
          value = 1000,
          min = 500,
          max = 7500,
          step = 500
        )
      )
    ),
    column(
      4,
      align = 'center',
      wellPanel(
        sliderInput(
          inputId = "bins",
          label = "Number of bins",
          value = 100,
          min = 10,
          max = 300,
          step = 2
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      12,
      align = 'center',
      actionButton(
        inputId = "resample",
        label = "Resample" , 
        icon = icon("refresh"),
        align = 'center'
      )
    )
  )
)

server <- function(input, output){
  
  df <- reactive({
    input$resample
    distributions %>%
    map(function(x) means(distribution = x, size_sample = input$size_sample, number_samples = input$number_samples)) %>%
    bind_rows() %>%
    mutate(
      dist = dist %>% as.factor()
    )
    })
  
  output$plot <- renderPlot({
    ggplot(data = df()) +
    geom_histogram(aes(x = means, y = ..density..), color = "gray10", fill = 'cornflowerblue', bins = input$bins) +
    stat_function( fun = dnorm, color = 'red', size = 1.2 ) +
    scale_x_continuous(name = 'Mean', breaks = seq(-4, 4, 1), limits = c(-4,4)) +
    scale_y_continuous(name = 'Density') +
    facet_wrap(~dist, scales = 'free') +
    theme_minimal() +
      theme(
        strip.text.x = element_text(size = 14),
        axis.title = element_text(size = 14)
      )
  })
}

shinyApp(ui = ui, server = server)