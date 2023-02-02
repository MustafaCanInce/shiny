library(shiny);
library(shinyWidgets)
library(shinyalert)
library(shinybusy)
library(shinyjs)
library(tibble)
library(jpeg)
library(tiff)
library(png)
library(raster)
library(scales)
library(plotly)
library(shapes)
library(dplyr)

ui <- fluidPage(
  add_busy_bar(
    timeout = 1000,
    color = "#112446",
    centered = FALSE,
    height = "8px"
  ),
  # Style applies to elements with the class "all_action_button" and it sets the border-radius to 20px, padding to 20px, and margin to 10px
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .all_action_button {
        border-radius: 20px;
        padding: 10px;
        margin: 10px;
        height: 6vh;
      }
                "))
  ),
  # Creates a sidebarLayout() which is a layout that positions the sidebarPanel() on the left side of the page.
  sidebarLayout(
    sidebarPanel(width="%20",
                 
                 
                 verbatimTextOutput("info"),
    ),
    # Creates a mainPanel() which is a container for displaying the main content of the application.
    mainPanel(
      uiOutput(outputId = "plot.ui") 
    )
  ),
  # Creates an absolutePanel() which is a container for UI elements. Inside this panel, 
  # It creates a wellPanel() which is a well-styled container for UI elements, this well panel contains several actionButton() and one fileInput() UI elements.
  absolutePanel(
    wellPanel(
      fileInput(
        inputId = "image_file",        label = NULL, buttonLabel = "Upload Image", multiple = TRUE, accept = ".jpg",),
      actionButton(
        icon = NULL, inputId = "imputation_Button", width = "15vh", class = "all_action_button", label = HTML("Missing Value<br/>Imputation")),
      actionButton(
        icon = NULL, inputId = "next_Button"      , width = "15vh", class = "all_action_button", label = "Next Image"),
      actionButton(
        icon = NULL, inputId = "prev_Button"      , width = "15vh", class = "all_action_button", label = "Previous Image"),
      actionButton(
        icon = NULL, inputId = "missing_Button"   , width = "15vh", class = "all_action_button", label = "Add Missing Point"),
      actionButton(
        icon = NULL, inputId = "undo_Button"      , width = "15vh", class = "all_action_button", label = "Undo Last Point"),
      actionButton(
        icon = NULL, inputId = "clear_button"     , width = "15vh", class = "all_action_button", label = "Clear Points"),
      actionButton(
        icon = NULL, inputId = "scale_Button"     , width = "15vh", class = "all_action_button", label = "Scale"),
      actionButton(
        icon = NULL, inputId = "done_Button"      , width = "15vh", class = "all_action_button", label = "Done"),
      actionButton(
        icon = NULL, inputId = "settings_id"      , width = "15vh", class = "all_action_button", label = "Settings")
    ),
    
    top="10%", left = "3vh", width="20vh"),
)