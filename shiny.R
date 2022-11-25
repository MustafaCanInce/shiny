library(shiny);

ui <- fluidPage(
  # Upload Image Button
  absolutePanel(wellPanel(actionButton(label="Upload Photo", icon=NULL, width=100, inputId="upload_Button")), top=200, height=200, left=900, width=150),
  
  # Next Image Button
  absolutePanel(wellPanel(actionButton(icon=NULL, inputId="next_Button", label="Next Image", width=100)), top=300, height=200, left=900, width=150),
  
  # Previous Image Button
  absolutePanel(wellPanel(actionButton(icon=NULL, inputId="prev_Button", width=100, label="Previous Image")), top=400, height=200, left=900, width=150),
  
  # Missing Point Button
  absolutePanel(wellPanel(actionButton(icon=NULL, inputId="missing_Button", width=100, label="Missing point")), top=500, height=200, left=900, width=150),
  
  #Image Table Output
  #absolutePanel(wellPanel(dataTableOutput(outputId="image_table_Output")), top=150, height=300, left=25, width=975),
  #titlePanel("Reliability"),
        
         

sidebarLayout(
  sidebarPanel(width=1920,
               actionButton("plotpoints", label = "Show Dots"),
               #actionButton("stopaddpoint", label = "Stop"),
               verbatimTextOutput("info")
  ),
  
  mainPanel(
    uiOutput("plot.ui")
  ) 
)
)
server <- function(input, output) {
  options(shiny.maxRequestSize=100*1024^2) # set maximum image size
       
         xy_new <- reactiveValues(x= numeric(0), y = numeric(0), line=numeric(0)) # add new points
         
           output$plot.ui <- renderUI({
                 plotOutput("distplot",
                                                 click = "plot_click",
                                                 dblclick = "plot_dblclick",#dblclick is not shown as a point in plot
                                                 hover = "plot_hover",
                                                 brush = "plot_brush")
             })
           
             # Listen for clicks and store values
             observe({
                   if (is.null(input$plot_click)){
                         return()
                     }
                   
                     isolate({
                           xy_new$x <- c(xy_new$x, input$plot_click$x)
                           xy_new$y <- c(xy_new$y, input$plot_click$y)
                       })
               })
           
             # Get the click values on button click
             pointsforplot <- eventReactive(input$plotpoints, ignoreNULL = F, {
                   
                     tibble(x = xy_new$x, y = xy_new$y)
                   
                 })
             
               output$distplot <- renderPlot({
                     
                       # Will update on button click, refreshing the plot
                       coord <- pointsforplot()
                       
                         plot(coord$x, coord$y, xlim=c(-400, 400), ylim=c(-400, 400), xlab="X", ylab="Y")
                       
                     })
               
                 output$info <- renderText({
                       xy_str <- function(e) {
                             if(is.null(e)) return("NULL\n")
                             paste0("x=", round(e$x, 2), " y=", round(e$y, 2), "\n")
                         }
                       xy_range_str <- function(e) {
                             if(is.null(e)) return("NULL\n")
                             paste0("xmin=", round(e$xmin, 2), " xmax=", round(e$xmax, 2),
                                                         " ymin=", round(e$ymin, 2), " ymax=", round(e$ymax, 2),
                                                         " xrange=", round(e$xmax-e$xmin, 2), " yrange=", round(e$ymax-e$ymin,2),
                                                         " diag=",round(sqrt((e$xmax-e$xmin)^2+(e$ymax-e$ymin)^2)))
                         }
                       
                         paste0(
                               "click: ", xy_str(input$plot_click),
                               "dblclick: ", xy_str(input$plot_dblclick),
                               #"hover: ", xy_str(input$plot_hover),
                                 "brush: ", xy_range_str(input$plot_brush)
                           )
                   })
             }


shinyApp(ui=ui, server=server)