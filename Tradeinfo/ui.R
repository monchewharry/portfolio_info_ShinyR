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
      tabPanel("Position Brief", dataTableOutput("record")),
      tabPanel('Chat Room', bootstrapPage(
        # We'll add some custom CSS styling -- totally optional
        includeCSS("shinychat.css"),
        
        # And custom JavaScript -- just to send a message when a user hits "enter"
        # and automatically scroll the chat window for us. Totally optional.
        includeScript("sendOnEnter.js"),
        
        div(
          # Setup custom Bootstrap elements here to define a new layout
          class = "container-fluid", 
          div(class = "row-fluid",
              # Set the page title
              tags$head(tags$title("ShinyChat")),
              
              # Create the header
              div(class="span6", style="padding: 10px 0px;",
                  h1("ShinyChat"), 
                  h4("Hipper than IRC...")
              ), div(class="span6", id="play-nice",
                     "IP Addresses are logged... be a decent human being."
              )
              
          ),
          # The main panel
          div(
            class = "row-fluid", 
            mainPanel(
              # Create a spot for a dynamic UI containing the chat contents.
              uiOutput("chat"),
              
              # Create the bottom bar to allow users to chat.
              fluidRow(
                div(class="span10",
                    textInput("entry", "")
                ),
                div(class="span2 center",
                    actionButton("send", "Send")
                )
              )
            ),
            # The right sidebar
            sidebarPanel(
              # Let the user define his/her own ID
              textInput("user", "Your User ID:", value=""),
              tags$hr(),
              h5("Connected Users"),
              # Create a spot for a dynamic UI containing the list of users.
              uiOutput("userList"),
              tags$hr(),
              helpText(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>."))
            )
          )
        )
      ))
  )
  )
))


#http://shiny.rstudio.com/tutorial/lesson3