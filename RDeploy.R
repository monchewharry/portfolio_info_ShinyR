library(rsconnect)
setwd("path to/Tradeinfo")
rsconnect::setAccountInfo(name='xxx', token='xxxx'
                          , secret='xxxxx')
rsconnect::deployApp(account = 'ding',appName = 'Tradeinfo')

library(shiny)
runApp()

