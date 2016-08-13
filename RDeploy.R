library(rsconnect)
setwd("/Users/CDX/R_workspace/Tradeinfo")
rsconnect::setAccountInfo(name='ding', token='48CD5247CA4348EE301101517285A152'
                          , secret='laBhGpWiW2RPKAiCGOLb3zMNfDY/K3LGMpbB267H')
rsconnect::deployApp(account = 'ding',appName = 'Tradeinfo')

library(shiny)
runApp()

