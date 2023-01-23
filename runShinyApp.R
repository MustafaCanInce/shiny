library("rstudioapi")
getwd()
setwd(dirname(getActiveDocumentContext()$path))
getwd()  

runShinyApp <- function(){
  source("ui.R")
  source("server.R")
  shinyApp(ui = ui, server = server)
  runApp(getwd())
}

runShinyApp()

