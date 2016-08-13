library(shiny)
library(quantmod)

portfolio = c('600777.SS','600606.SS',"000001.SS") 

function(input, output) {
  dataset <- reactive({
    setSymbolLookup(stock1=list(name=portfolio[as.numeric(input$select)]))
    getSymbols('stock1',from=as.Date(-input$slider1, origin= format(Sys.Date(), "%Y-%m-%d"))   )
    STOCK1
  })#有交互的处理
  
  output$plot <- renderPlot({#显示图片
    if(input$k)
      chartSeries(dataset(),type = 'candle',name =portfolio[as.numeric(input$select)])
    else
      chartSeries(dataset(),type='line',name =portfolio[as.numeric(input$select)])
    
  }, height=700)
  
}