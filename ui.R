ui <- fluidPage(
  add_busy_bar(
    timeout = 1000,
    color = "#112446",
    centered = FALSE,
    height = "0.5vh"
  ),
  useShinyjs(),
  tags$head(
    tags$script("
  $(document).ready(function() {
  var debounceTimeout;
  $(window).on('resize', function() {
    clearTimeout(debounceTimeout);
    debounceTimeout = setTimeout(function() {
      var width = window.innerWidth;
      var height = window.innerHeight;
      Shiny.setInputValue('screenSize', [width, height]);
    }, 500);
  });
});
"),
    tags$style(
      HTML("
      .all_action_button {
        border-radius: 2vh;
        padding: 1%;
        margin: 0.5vh;
        height: 4.5vh;
        }
          "),
      HTML(".inner_button {
        padding: 1%;
        margin: 0.5vh;
        height: 2.5vh;
    }
          "),
      HTML("
      #folder {
        background-color: white;
         border: 0.5px solid black;
      }
    ")
    )
  ),

  fluidRow(
    column(width = 12,
           # main panel
           div(
             id = "ui_div",
             style = "display:inline",
             uiOutput(outputId = "plot.ui", style = "height: auto; width: auto; position: fixed; top: 0vh; left: 26vh;")
           ),

    ),
    column(width = 6,
           # left panel
           absolutePanel(
             align = "center",
             wellPanel(
               fileInput(inputId = "image_file", label = NULL, multiple = TRUE, accept = c(".JPG", ".JPEG", ".PNG", ".TIF"),
                         buttonLabel = "Upload"),
               verbatimTextOutput("file_names"),
               actionButton(
                 icon = NULL, inputId = "help_button"      , width = "15vh", class = "all_action_button", label = "Help",
                 title = "Additional information"),

               actionButton(
                 icon = NULL, inputId = "settings_id"      , width = "15vh", class = "all_action_button", label = "Settings",
                 title = "Change output directory and rater name."),

               actionButton(
                 icon = NULL, inputId = "markings_Button"  , width = "15vh", class = "all_action_button", label = "Marking",
                 title = "Navigate between images, view and remove markings, add missing values."),

               actionButton(
                 icon = NULL, inputId = "show_scale_Button", width = "15vh", class = "all_action_button", label = "Scaling",
                 title = "Calibrate and measure distance between landmarks."),

               actionButton(
                 icon = NULL, inputId = "show_inrel_Button", width = "15vh", class = "all_action_button", label = "Reliability",
                 title = "Bu bir butondur."),

               actionButton(
                 icon = NULL, inputId = "imputation_Button", width = "15vh", class = "all_action_button", label = "Imputation",
                 title = "Impute missing landmarks."),

               actionButton(
                 icon = NULL, inputId = "done_Button"      , width = "15vh", class = "all_action_button", label = "Save File",
                 title = "Get marked image and scaled/normal output."),

               actionButton(
                 icon = NULL, inputId = "plots_button"     , width = "15vh", class = "all_action_button", label = "Plots",
                 title = "Draw scatterplot and/or heatmap.")

             ),
             top = "0vh", left = "0vh", width = "26vh"
           ),

           absolutePanel(
             align = "center",
             wellPanel(
               HTML("<b>Imputation Settings</b>"),
               br(),
               "L1 and L2 two reference landmarks. Please select prefered anatomical landmarks.",
               numericInput(inputId = "l1_input", label = "L-1:", value = "1"),
               numericInput(inputId = "l2_input", label = "L-2:", value = "2"),
               textInput(inputId = "imp_csv_input", label = "Csv file input file path:", value = "", placeholder = "Input csv folder path"),
               radioButtons(inputId = "imp_radio_button" , label = "Imputation Method:", choices = c("minF Method", "Multiple Regression Method", "Expected Maximization Method")),
               div(style = "display:flex; flex-direction: row; justify-content: center;",
                   actionButton(inputId = "calculate_imp", label = "Submit"),
                   actionButton(inputId = "close_imp_panel", label = "Close")
               )
             ),
             id = "imputation_panel",
             style = "display: none;",
             top = "60vh", left = "0vh", width = "26vh"
           ),

           #known_distance <- 1,

           absolutePanel(
             align = "center",
             wellPanel(

               HTML("<b>Scale Settings</b>"),
               #hr(),
               HTML("<p> </p>"),

               actionButton(inputId = "scale_cal_Button", label = "Calibrate", class = "inner_button", style = "line-height: 0.5vh;"),
               actionButton(inputId = "ratio_button", label = "Reset Calibration", class = "inner_button", style = "line-height: 0.5vh;"),
               actionButton(inputId = "scale_meas_Button", label = "Measure Distance", class = "inner_button", style = "line-height: 0.5vh;"),
               tags$hr(style="border-color: black;"),
               numericInput(inputId = "knowndistance_input", label = "Referance Length:", value = ""),
               selectInput(inputId = "knowndistance_units_input", label = "Units of Length:", choices = c("mm", "cm", "m"), selected = "mm"),
               div(style = "display:flex; flex-direction: row; justify-content: center;",
                   actionButton(inputId = "save_scale_unit_dist_button", label = "Submit"),
                   actionButton(inputId = "close_scale_panel", label = "Close")
               )

             ),
             id = "scale_panel",
             style = "display: none;",
             top = "60vh", left = "0vh", width = "26vh"
           ),

           absolutePanel(
             align = "center",
             wellPanel(
               HTML("<b>Settings</b>"),
               br(),
               shinyDirButton(id = "folder", label = "Select a Folder", title = "Please select a folder to save the output files.", FALSE),
               verbatimTextOutput(outputId = "filepath_text_print", placeholder = TRUE),
               tags$hr(style="border-color: black;"),
               radioButtons(inputId = "rater_type", label = "Rater Type:",
                            choices = c("Single Rater", "Multiple Raters"), selected = "Single Rater"),
               conditionalPanel(condition = "input.rater_type == 'Single Rater'",
                                textInput(inputId = "name_input", label = "Rater Name:", value = "", placeholder = "Enter your name"),
                                textInput(inputId = "trial_input", label = "Trial:", value = "", placeholder = "Enter the trial value"),

               ),
               conditionalPanel(condition = "input.rater_type == 'Multiple Raters'",
                                textInput(inputId = "name_input_mult", label = "Rater Name:", value = "", placeholder = "Enter your name")
               ),
               div(style = "display:flex; flex-direction: row; justify-content: center;",
                   actionButton(inputId = "save_name_button", label = "Submit"),
                   actionButton(inputId = "close_settings_panel", label = "Close")
               )
             ),
             id = "settings_panel",
             style = "display: none;",
             top = "60vh", left = "0vh", width = "26vh"
           ),

           absolutePanel(
             align = "center",
             wellPanel(
               HTML("<b>Plots</b>"),
               br(),
               fileInput("csv_input_plots_button", "Select csv files", multiple = TRUE, accept = ".csv"),
               radioButtons(inputId = "type_plo_radio_button" , label = "Plot Type", choices = c("Heatmap", "Scatter Plot")),
               actionButton(inputId = "draw_plot_button", label = "Submit"),
               actionButton(inputId = "close_plots_Button", label = "Close")
             ),
             id = "plots_panel",
             style = "display: none;",
             top = "60vh", left = "0vh", width = "26vh"
           ),

           absolutePanel(
             align = "center",
             wellPanel(
               HTML("<b>Marking</b>"),
               br(),
               actionButton(
                 icon = NULL, inputId = "prev_Button"      , label = "Previous Image", class = "inner_button", style = "line-height: 0.5vh;"),
               actionButton(
                 icon = NULL, inputId = "next_Button"      , label = "Next Image", class = "inner_button", style = "line-height: 0.5vh;"),
               actionButton(
                 icon = NULL, inputId = "missing_Button"   , label = "Add One Missing Landmark", class = "inner_button", style = "line-height: 0.5vh;"),
               br(),
               actionButton(
                 icon = NULL, inputId = "undo_Button"      , label = "Undo Last Landmark", class = "inner_button", style = "line-height: 0.5vh;"),
               br(),
               actionButton(
                 icon = NULL, inputId = "clear_button"     , label = "Clear All Landmarks", class = "inner_button", style = "line-height: 0.5vh;"),
               verbatimTextOutput(outputId = "info", placeholder = TRUE),
               verbatimTextOutput(outputId = "coords"),
               actionButton("close_markings_Button", "Close")
             ),
             id = "marking_panel",
             style = "display: none;",
             top = "60vh", left = "0vh", width = "26vh"
           ),

           absolutePanel(
             align = "center",
             wellPanel(
               HTML("<b>Help</b>"),
               br(),
               tags$a(href = "https://youtu.be/tN3gev199Lw", "Video"),
               br(),

               br(),
               actionButton("close_help_button", "Close")
             ),
             id = "help_panel",
             style = "display: none;",
             top = "60vh", left = "0vh", width = "26vh"
           ),

           absolutePanel(
             align = "center",
             wellPanel(
               HTML("<b>Save File</b>"),
               br(),

               actionButton(
                 icon = NULL, inputId = "save_img_Button", label = "Save Marked Image", class = "inner_button", style = "line-height: 0.5vh;"),
               radioButtons(inputId = "done_radio_button" , label = "Output types", choices = c("Save scaled coordinates as csv", "Save unscaled coordinates as csv")),

               div(style = "display:flex; flex-direction: row; justify-content: center;",
                   actionButton(inputId = "done_submit_button", label = "Submit"),
                   actionButton(inputId = "close_donepanel_Button", label = "Close")
               )
             ),
             id = "done_panel",
             style = "display: none;",
             top = "60vh", left = "0vh", width = "26vh"
           ),
           absolutePanel(
             align = "center",
             wellPanel(
               HTML("<b>Inter Rater Landmark Reliability</b>"),
               HTML("<p> </p>"),
               numericInput(inputId = "rel_dimension_input", label = "Number of Dimensions:", value = "2"),
               numericInput(inputId = "rel_subject_input", label = "Number of Subjects:", value = "2"),
               numericInput(inputId = "rel_landmark_input", label = "Number of Landmarks:", value = "3"),
               textInput(inputId = "rel_path1_input", label = "First Markings Csv Directory:", value = "", placeholder = "Please enter a file folder"),
               textInput(inputId = "rel_path2_input", label = "Second Markings Csv Directory:", value = "", placeholder = "Please enter a file folder"),
               actionButton(inputId = "rel_submit_button", label = "Submit"),
               actionButton("close_interrel_button", "Close")
             ),
             id = "inter_reliability_panel",
             style = "display: none;",
             top = "60vh", left = "0vh", width = "26vh"
           )
    )
  ),
  fluidRow(
    column(6, offset = 2,
           div(
             id = "plot_div",
             style = "display:none",
             plotOutput("plot", height = "70vh", width = "80vh")
           )
    )
  ),

  ui <- fluidPage(
    fluidRow(
      column(6, offset = 2,
             div(
               id = "img_div",
               style = "display:none; margin-left: 21vh;",
               img(src = "https://i.ibb.co/VYTtHx8/flow-09.png", width = "890px", height = "1600px")
             )
      )
    )
  )
)
