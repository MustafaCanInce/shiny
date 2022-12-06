library(shiny);
library(tidyverse)
library(fontawesome)
library(jpeg)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=1080,
                 #actionButton("plotpoints", label = "Show Dots"),
                 #actionButton("stopaddpoint", label = "Stop"),
                 verbatimTextOutput("info"),
                 actionButton("clear", label = "Clear Points")
    ),
    
    mainPanel(
      #tabsetPanel(
      #  tabPanel("Plot", plotOutput("plot")),
      #  tabPanel("Summary", verbatimTextOutput("summary")),
      #  tabPanel("Table", tableOutput("table"))
      #),
      uiOutput(outputId = "plot.ui") # , width = "500",height = "1000"
    )
  ),
  # Upload Image Button
  absolutePanel(wellPanel(actionButton(icon=NULL, inputId="upload_Button ", width=100, label="Upload Photo")),   top=300, height=200, left='170vh', width=150),
  
  # Next Image Button
  absolutePanel(wellPanel(actionButton(icon=NULL, inputId="next_Button"   , width=100, label="Next Image")),     top=450, height=200, left='170vh', width=150),
  
  # Previous Image Button
  absolutePanel(wellPanel(actionButton(icon=NULL, inputId="prev_Button"   , width=100, label="Previous Image")), top=600, height=200, left='170vh', width=150),
  
  # Missing Point Button
  absolutePanel(wellPanel(actionButton(icon=NULL, inputId="missing_Button", width=100, label="Missing point")),  top=750, height=200, left='170vh', width=150),
  
  #Image Table Output
  #absolutePanel(wellPanel(dataTableOutput(outputId="image_table_Output")), top=150, height=300, left=25, width=975),
  #titlePanel("Reliability"),
  
  
  
  
  
  
)
server <- function(input, output, session) {
  
  
  
  # By default, Shiny limits file uploads to 5MB per file.
  # modify this limit by using the shiny.maxRequestSize option. 
  # For example, adding options(shiny.maxRequestSize=30*1024^2) to the top of server.R would increase the limit to 30MB.
  
  options(shiny.maxRequestSize=100*1024^2) 
  
  
  xy_new <- reactiveValues(x= numeric(0), y = numeric(0), line=numeric(0)) # add new points
  
  
  
  
  output$plot.ui <- renderUI({
    plotOutput("distplot",
               click = "plot_click",
               dblclick = "plot_dblclick",
               #dblclick is not shown as a point in plot
               #hover = "plot_hover",
               #brush = "plot_brush",
               height = "80vh",
               width = "160vh"
    )
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
  
  #get screen resolution.
  observe({
    cat(input$GetScreenWidth)
  })
  
  #pointsforplot <- eventReactive(input$clear, ignoreNULL = F,{
  #  tibble(x = NULL, y = NULL)
  #})
  
  # Get the click values on button click
  #pointsforplot <- eventReactive(input$plotpoints, ignoreNULL = F, {
  
  # tibble(x = xy_new$x, y = xy_new$y)
  
  #})
  
  
  #eventReactive for upload photo action button
  #upload_image <- eventReactive(input$upload_Button,{
    
    #to load the image
    #ima <- readJPEG("C:/coordinates/bone.jpg")
    
    #take plot info and set background photo
    #lim <- par()
    #rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  #})
  
  
  output$distplot <- renderPlot({
    
    # Will update on button click, refreshing the plot
    coord <- tibble(x = xy_new$x, y = xy_new$y)
    #to load the image
    ima <- readJPEG("C:/coordinates/bone.jpg")
    
    #to save the coordinates of the dots
    fileName <- file("C:/coordinates/coord.txt")
    imageName <- basename("C:/coordinates/bone.jpg")
    
    coordinates <- c(as.character(xy_new$x), "\n", as.character(xy_new$y))
    writeLines(c(imageName, length(xy_new$x), coordinates),fileName)
    close(fileName)
    
    #to load the image
    #ima <- readJPEG("C:/coordinates/bone.jpg")
    
    #pointsforplot()
    plot(coord$x, coord$y, xlim=c(-600, 600), ylim=c(-600, 600), xlab="X", ylab="Y")
    
    
    #uploads <- upload_image()
    
    #take plot info and set background photo
    lim <- par()
    rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    
    
    
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
      "double_click: ", xy_str(input$plot_dblclick)
      #"hover: ", xy_str(input$plot_hover),
      #"brush: ", xy_range_str(input$plot_brush)
    )
  })
}
# Run in a dialog within R Studio
# runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1200, height = 600))
# runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
shinyApp(ui=ui, server=server)
