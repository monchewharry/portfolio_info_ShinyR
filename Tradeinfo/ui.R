library(shiny)  
library(quantmod)

shinyUI(fluidPage(
  titlePanel("Ding's Portfolio Info"),#标题
  sidebarPanel(
    img(src="head.jpg", height = 80, width = 80),
    span("Contact: caodingxian@163.com", style = "color:blue"),
    
    tags$p(h5("Portfolio Return Rate(exlude 150201):")),
    verbatimTextOutput('grossRet'),
    tags$p(h5("SSEC Return Rate:")),
    verbatimTextOutput('ssec_ret'),
    
  	selectInput("select", label = h3("Select stock"), 
        choices = list("000001上证综指" = 1, "600606绿地控股" = 2,
                       "600777新潮能源" = 3,'000876新希望'=4,'000301东方市场'=5,
                       '000768中航飞机'=6)
        , selected = 1),
  	checkboxInput(inputId = 'if_k',label = h3('k线图')),
  	
  	sliderInput("slider1", label = "last days",
  	            min = 0, max = 200, value = 100),
  	checkboxInput(inputId = 'if_ma',label = h3('add MA')),
  	sliderInput('MA',label = 'MA_n',
  	            min=5,max=60,value=10)
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("stockplot")), 
      tabPanel("Position Brief", dataTableOutput("record"))
  )
  )
))


#http://shiny.rstudio.com/tutorial/lesson3