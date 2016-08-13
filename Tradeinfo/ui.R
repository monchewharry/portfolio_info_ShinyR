library(shiny)  
library(quantmod)


fluidPage(
  titlePanel("portfolio info"),#标题
  sidebarPanel(
  	selectInput("select", label = h3("Select stock"), 
        choices = list("600777" = 1, "600606" = 2,
                       "000001.SS" = 3), selected = 3),
  	sliderInput("slider1", label = h3("last days"),
  	            min = 0, max = 100, value = 50),
    checkboxInput(inputId = 'k',label = 'k')
  ),
  mainPanel(
    plotOutput(outputId ='plot')
  )
)


#http://shiny.rstudio.com/tutorial/lesson3/