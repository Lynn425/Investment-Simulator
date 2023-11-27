# Title: Basic Investment Simulator
# Description: This is a shiny app for the "Investment Simulations" tutorial
# available in bCourses (link below)
# https://bcourses.berkeley.edu/courses/1516876/files/folder/readings?preview=84322485
#
# Author: Gaston Sanchez


# ------------------------
# Required packages
# ------------------------
library(shiny)
library(tidyverse)


# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Big Title"),
  fluidRow(
    # Inputs for initial deposit
    column(width = 4,
           h4("Money Input"),
           numericInput(inputId = "initial_amount", 
                        label = "initial amount", 
                        value = 5000)
    ),
    
    # Inputs for mean and standard deviation of annual return rates
    column(width = 4,
           h4("Time input"),
           sliderInput(inputId = "num_years", 
                       label = "number of years", 
                       value = 10, 
                       min = 1, 
                       max = 30)
    ),
    
    # Inputs for number of simulations, and random seed
    column(width = 4,
           h4("Simulation Parameters"),
           sliderInput(inputId = "num_simulations", 
                       label = "number of simulations", 
                       value = 50, 
                       min = 10, 
                       max = 100),
           numericInput(inputId = "seed", 
                        label = "random seed", 
                        value = 123)
    )
  ),
  
  hr(),
  h4('Graph of Simulations'),
  plotOutput('plot_sim'),
  
  hr(),
  fluidRow(
    column(width = 6,
           h4('Summary Table1'),
           p('Some summary stats'),
           tableOutput('table1')
    ),
    column(width = 6,
           h4('Summary Table2'),
           p('Simulations > initial amount'),
           tableOutput('table2')
    )
  )
)


# Define server
server <- function(input, output) {
  
  # ------------------------
  # matrix of simulated data
  # ------------------------
  balance_mat = reactive({
    balance = matrix(0, nrow = input$num_years + 1, ncol = input$num_simulations)
    balance[1, ] = input$initial_amount
    set.seed(input$seed)

    for (sim in 1:input$num_simulations) {
      return_rates = rnorm(input$num_years, mean = 0.11, sd = 0.20)
      for (year in 1:input$num_years) {
        balance[year+1,sim] = (balance[year,sim] * (1 + return_rates[year]))
      }
    }
    
    colnames(balance) = paste0("sim", 1:input$num_simulations)
    balance
  })
  
  
  # -------------------------------------------------------
  # reshape table into long format (for ggplot convenience)
  # -------------------------------------------------------
  dat_sim = reactive({
    tbl = as.data.frame(balance_mat())
    tbl$year = 0:input$num_years
    
    dat = pivot_longer(
      data = tbl, 
      cols = starts_with("sim"), 
      names_to = "simulation",
      values_to = "amount")
    
    dat
  })
  
  
  # --------------------------------
  # Output: timelines of simulations
  # --------------------------------
  output$plot_sim <- renderPlot({
    ggplot(data = dat_sim(), aes(x = year, y = amount)) +
      geom_line(aes(group = simulation))
  })
  

  # ---------------------------
  # Output: table1 of summaries
  # ---------------------------
  # some numeric statistics for every year
  output$table1 <- renderTable({
    dat_sim() %>% 
      #filter(year %in% seq(0, input$num_years, by = 2)) %>%
      group_by(year) %>%
      summarise(
        min = min(amount),
        max = max(amount)
      )
  })

    
  # ---------------------------
  # Output: table2 of summaries
  # ---------------------------
  # number and proportion of simulations greater than initial_amount
  output$table2 <- renderTable({
    dat_sim() %>% 
      group_by(year) %>%
      summarise(
        count = sum(amount > input$initial_amount),
        proportion = count / input$num_simulations
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
