library("rstudioapi")
getwd()
setwd(dirname(getActiveDocumentContext()$path))

runShinyApp <- function(){
  source("ui.R")
  source("server.R")
  shinyApp(ui = ui, server = server)
  runApp(getwd())
}

runShinyApp()
