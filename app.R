library(shiny)
library(tidyverse)

#import some basic data of normal distribution parameters
mu=c(0.0509,0.0540,0.0570,0.06,0.0629,0.0657,0.0684,0.0711,0.0737,0.0762,0.0787,0.0811,0.0834,0.0856,0.0878,0.0899,0.0919,0.0938,0.0957,0.0974,0.0991)
sd=c(0.0403,0.0399,0.0411,0.0437,0.0474,0.0521,0.0574,0.0634,0.0694,0.0759,0.0825,0.0894,0.0964,0.1035,0.1107,0.1180,0.1254,0.1328,0.1403,0.1479,0.1555)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("HW5 :Advanced Investment Simulator"),
  fluidRow(
    h3("Widgets"),
    # Inputs for initial deposit,periodic contributions and target amount // widget column 1
    column(width = 4,
           h4("Money Input and desired Money Output"),
           numericInput(inputId = "initial_amount", 
                        label = "initial amount", 
                        value = 1000,
                        min=1000,
                        max=10000),
           numericInput(inputId = "periodic_contributions", 
                        label = "periodic contributions", 
                        value = 360,
                        min=100,
                        max=500),
           numericInput(inputId = "target_amount", 
                        label = "target amount", 
                        value = 5000,
                        min=1000,
                        max=100000)
    ),
    # Inputs for number of years and portfolio composition // widget column 2
    column(width = 4,
           h4("Time and Portfolio"),
           numericInput(inputId = "num_years", 
                       label = "number of years", 
                       value = 10, 
                       min = 1, 
                       max = 30),
           sliderInput(inputId = "stocks_prop", 
                       label = "proportion of stocks", 
                       value = 60, 
                       min = 0, 
                       max = 100,
                       step=5)
    ),
    # Inputs for number of simulations and random seed //widget column 3
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
  h3("Plots"),
  fluidRow(
    column(width=6,
           h4('Simulated Timelines'),
           p('The horizontal line matches the value of target amount, while the vertical line shows average time needed to reach this amount'),
           plotOutput('plot_sim1'),
           h4('Probability of reaching target'),
           plotOutput('plot_sim2')
    )
  ),
  
  hr(),
  h3("Stats"),
  fluidRow(
    column(width = 6,
           h4('minimum and maximum value at the end of each year among all the simulations'),
           p('the differences are due to random sampling of normal distribution parameters'),
           tableOutput('table1')
    ),
    column(width = 6,
           h4('Probability Table'),
           p('Probability of reaching target amount'),
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
      return_rates = rnorm(input$num_years, mean = mu[input$stocks_prop/5+1], sd = sd[input$stocks_prop/5+1])
      for (year in 1:input$num_years) {
        balance[year+1,sim] = (balance[year,sim] * (1 + return_rates[year])+input$periodic_contributions)
      }
    }
    colnames(balance) = paste0("sim", 1:input$num_simulations)
    balance
  })
  
  # ------------------------
  #compute average year
  # ------------------------
  avgyear=reactive({
    targetyear<-c()
    balance = matrix(0, nrow = input$num_years + 1, ncol = input$num_simulations)
    balance[1, ] = input$initial_amount
    set.seed(input$seed)
    for (sim in 1:input$num_simulations) {
      return_rates = rnorm(input$num_years, mean = mu[input$stocks_prop/5+1], sd = sd[input$stocks_prop/5+1])
      for (year in 1:input$num_years) {
        balance[year+1,sim] = (balance[year,sim] * (1 + return_rates[year])+input$periodic_contributions)
        if (balance[year,sim]<input$target_amount & balance[year+1,sim]>=input$target_amount) {
          targetyear<-c(targetyear,year+1)
        }
      }
    }
    mean(targetyear)
  })
  
  # ------------------------
  #compute probability, then transform prob matrix into data frame(for ggplot convenience)
  # ------------------------
  prob_sim=reactive({
    balance = matrix(0, nrow = input$num_years + 1, ncol = input$num_simulations)
    balance[1, ] = input$initial_amount
    set.seed(input$seed)
    for (sim in 1:input$num_simulations) {
      return_rates = rnorm(input$num_years, mean = mu[input$stocks_prop/5+1], sd = sd[input$stocks_prop/5+1])
      for (year in 1:input$num_years) {
        balance[year+1,sim] = (balance[year,sim] * (1 + return_rates[year])+input$periodic_contributions)
      }
    }
    prob=matrix(0,nrow=input$num_years,ncol=2)
    for (year in 1:input$num_years){
      prob[year,1]=year
    }
    for (year in 2:input$num_years+1){
      j=0
      for (sim in 1:input$num_simulations) {
        if(balance[year,sim]>=input$target_amount){
          j=j+1
        }
      }
      prob[year-1,2]=j
    }
    year<-prob[,1]
    probability<-prob[,2]/input$num_simulations
    data<-data.frame(year,probability)
    data
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
  output$plot_sim1 <- renderPlot({
    ggplot(data = dat_sim(), aes(x = year, y = amount)) +
      geom_line(aes(group = simulation))+
      geom_hline(aes(yintercept=input$target_amount),lty=2,col="blue")+
      geom_vline(aes(xintercept=avgyear()),lty=2,col="blue")+
      annotate(geom="text",x=0,y=input$target_amount,label="target",size=6)+
      annotate(geom="text",x=avgyear(),y=0,label=paste(round(avgyear(),2)),size=6)+
      theme_bw()
  })
  
  
  # --------------------------------
  # Output: probability bar chart
  # --------------------------------
  output$plot_sim2 <- renderPlot({
    ggplot(data=prob_sim(),aes(x=year,y=probability))+
      geom_bar(stat='identity')
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
    prob_sim() %>% 
      group_by(year) %>%
      summarise(
        probability=probability
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
