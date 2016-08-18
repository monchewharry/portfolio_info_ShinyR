library(shiny)
library(quantmod)
library(stringr)

# Globally define a place where all users can share some reactive data.
vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to Shiny Chat!"
}

#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

shinyServer(function(input, output,session) {
  portfolio = c("^SSEC",'600777.ss','600606.ss','000876.sz','000301.sz','000768.sz','150201.sz') 
  cost = c(3000,14.526,9.196,8.338,4.53,22.49,0.535)
  amt = c(0,500,500,1500,500,300,27600)
  price=c()
  for (s in portfolio) {
    ero <- FALSE
    setSymbolLookup(stockn=list(name=s))
    tryCatch(getSymbols('stockn'), 
             error = function(e) {price <<- c(price,NA) 
                                   ero <<- TRUE} , 
             finally = print(price)
             )
    if(ero) next
    price = c(price,Cl(last(STOCKN,1)))
  }
  
  grossCost <- sum((cost*amt)[!is.na(price)])#exclude na
  grossValue <- sum(price * amt,na.rm = TRUE)
  
  
  dataset <- reactive({
    setSymbolLookup(stock1=list(name=portfolio[as.numeric(input$select)]))
    getSymbols('stock1',from=as.Date(-input$slider1, origin= format(Sys.Date(), "%Y-%m-%d"))   )
    STOCK1
  })#有交互的处理
  
  output$stockplot <- renderPlot({#显示图片
    #print(cost[as.numeric(input$select)])
    id = input$select
    
    if(input$if_k){
      chartSeries(dataset(),type = 'candle',name = portfolio[as.numeric(id)],
                  theme = 'white')
      abline(h = cost[as.numeric(id)])
      
    }else{
      chartSeries(dataset(),type='line',name =portfolio[as.numeric(id)],
                  theme = 'white')
      abline(h = cost[as.numeric(id)])
    }
    if(input$if_ma){
      addEMA(n= input$MA)
    }
      
  }, height=700)
  
  output$record <- renderDataTable(
    data.frame(Stock_num = portfolio,
               Cost = cost,
               Amount = amt,
               Times =round(price/cost , digits = 3) ),
    options = list(
      pageLength = 10,
      initComplete = I("function(settings, json) {alert('Done.');}")
      )
  )
  output$grossRet <-renderPrint(
   paste(as.character(round(grossValue/grossCost -1,5)*100),'%',"  ",format(Sys.Date(), "%Y-%m-%d"),sep = "") 
  )
  output$ssec_ret <-renderPrint(
    paste(as.character(round(price[1]/cost[1] -1,5)*100),'%',"  ",format(Sys.Date(), "%Y-%m-%d"),sep = "") 
  )
  
  ####CHAT
  # Create a spot for reactive variables specific to this particular session
  sessionVars <- reactiveValues(username = "")
  
  # Track whether or not this session has been initialized. We'll use this to
  # assign a username to unininitialized sessions.
  init <- FALSE
  
  # When a session is ended, remove the user and note that they left the room. 
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
      vars$chat <- c(vars$chat, paste0(linePrefix(),
                                       tags$span(class="user-exit",
                                                 sessionVars$username,
                                                 "left the room.")))
    })
  })
  
  # Observer to handle changes to the username
  observe({
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    
    if (!init){
      # Seed initial username
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
      isolate({
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-enter",
                                                    sessionVars$username,
                                                    "entered the room.")))
      })
      init <<- TRUE
    } else{
      # A previous username was already given
      isolate({
        if (input$user == sessionVars$username || input$user == ""){
          # No change. Just return.
          return()
        }
        
        # Updating username      
        # First, remove the old one
        vars$users <- vars$users[vars$users != sessionVars$username]
        
        # Note the change in the chat log
        vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                          tags$span(class="user-change",
                                                    paste0("\"", sessionVars$username, "\""),
                                                    " -> ",
                                                    paste0("\"", input$user, "\""))))
        
        # Now update with the new one
        sessionVars$username <- input$user
      })
    }
    # Add this user to the global list of users
    isolate(vars$users <- c(vars$users, sessionVars$username))
  })
  
  # Keep the username updated with whatever sanitized/assigned username we have
  observe({
    updateTextInput(session, "user", 
                    value=sessionVars$username)    
  })
  
  # Keep the list of connected users updated
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){
      return(tags$li(user))
    })))
  })
  
  # Listen for input$send changes (i.e. when the button is clicked)
  observe({
    if(input$send < 1){
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    isolate({
      # Add the current entry to the chat log.
      vars$chat <<- c(vars$chat, 
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
  })
  
  # Dynamically create the UI for the chat window.
  output$chat <- renderUI({
    if (length(vars$chat) > 500){
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")
    
    # Pass the chat log through as HTML
    HTML(vars$chat)
  })
  
  
})