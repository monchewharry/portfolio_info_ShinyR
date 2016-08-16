library(shiny)
library(quantmod)



shinyServer(function(input, output) {
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
  
})