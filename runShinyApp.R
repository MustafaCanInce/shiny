runShinyApp <- function(){
  required_packages <- c("rstudioapi", "shiny", "shinyWidgets", "shinyalert", "shinybusy", "shinyjs", "shinyFiles",
                       "tibble", "jpeg", "tiff", "png", "raster", "scales", "plotly", "shapes", "dplyr", "mice")

  # Load packages that are already installed
  for (package in required_packages) {
    if (require(package, character.only = TRUE)) {
      library(package, character.only = TRUE)
    }
  }

  # Install and load packages that are not installed
  for (package in required_packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }


  getwd()
  setwd(dirname(getActiveDocumentContext()$path))


  source("ui.R")
  source("server.R")
  shinyApp(ui = ui, server = server)
  runApp(getwd())
}
runShinyApp()
