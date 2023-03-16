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
    tags$style(
    HTML("
      .all_action_button {
        border-radius: 20px;
        padding: 10px;
        margin: 10px;
        height: 6vh;
      }
                "))
  ),
  
  fluidRow(
    column(width = 12,
           # main panel
           uiOutput(outputId = "plot.ui", style = "height: auto; width: auto; position: fixed; top: 5vh; left: 21vh;")
           
           
    ),
    column(width = 6,
           # left panel
           absolutePanel(
             wellPanel(
               fileInput(inputId = "image_file", label = NULL, multiple = TRUE, accept = c(".JPG", ".JPEG", ".PNG", ".TIF")),
               verbatimTextOutput("file_names"),
               actionButton(
                 icon = NULL, inputId = "imputation_Button", width = "15vh", class = "all_action_button", label = HTML("Missing Value<br/>Imputation")),
               actionButton(
                 icon = NULL, inputId = "next_Button"      , width = "15vh", class = "all_action_button", label = "Next Image"),
               actionButton(
                 icon = NULL, inputId = "prev_Button"      , width = "15vh", class = "all_action_button", label = "Previous Image"),
               actionButton(
                 icon = NULL, inputId = "missing_Button"   , width = "15vh", class = "all_action_button", label = "Add Missing Landmark"),
               actionButton(
                 icon = NULL, inputId = "undo_Button"      , width = "15vh", class = "all_action_button", label = "Undo Last Landmark"),
               actionButton(
                 icon = NULL, inputId = "clear_button"     , width = "15vh", class = "all_action_button", label = "Clear Landmarks"),
               actionButton(
                 icon = NULL, inputId = "scale_Button"     , width = "15vh", class = "all_action_button", label = "Scale"),
               actionButton(
                 icon = NULL, inputId = "done_Button"      , width = "15vh", class = "all_action_button", label = "Done"),
               actionButton(
                 icon = NULL, inputId = "settings_id"      , width = "15vh", class = "all_action_button", label = "Settings"),
               verbatimTextOutput(outputId="info", placeholder=TRUE),
               verbatimTextOutput("coords")
             ),
             top="0vh", left = "0vh", width="20vh"
           )
    )
  )
)