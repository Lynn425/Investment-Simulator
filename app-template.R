# ===============================================
# Fill in the following fields
# ===============================================
# Title: 
# Description: 
# Author: 
# Date: 


# ===============================================
# Required packages (you can use other packages if you want)
# ===============================================
library(shiny)
library(tidyverse)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Customize Me!"),
  fluidRow(
    # Inputs for initial amount, and periodic contributions 
    column(width = 3,
           h4("column1")  # customize text, and include input widgets
    ),
    
    # Inputs for target amount, and number of years 
    column(width = 3,
           h4("column2")  # customize text, and include input widgets
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(width = 3,
           h4("column3")  # customize text, and include input widgets
    ),
    
    # Inputs for number of simulations, and random seed
    column(width = 3,
           h4("column4")  # customize text, and include input widgets
    )
  ),
  
  hr(),
  h4('Plot 1'),  # customize text
  plotOutput('plot1'),

  hr(),
  h4('Plot 2'),  # customize text
  plotOutput('plot2'),
  
  hr(),
  h4('More Info'),
  tableOutput('table1')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # code for plot1
  output$plot1 <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = iris) +
      geom_density(aes(x = Sepal.Length, fill = Species))
  })
  
  
  # code for plot2
  output$plot2 <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = iris) +
      geom_histogram(aes(x = Petal.Length, fill = Species))
  })
  
    
  # code for additional information
  output$table1 <- renderTable({
    # replace the code below with your code!!!
    iris %>%
      group_by(Species) %>%
      summarise(
        avg_sepal_length = mean(Sepal.Length),
        avg_petal_length = mean(Petal.Length)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
