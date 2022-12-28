library(shiny);
library(tidyverse)
library(jpeg)
library(shinyWidgets)
library(ggimage)
library(ggplot2)
library(png)
library(shinyjs)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(width=1080,
                 #actionButton("plotpoints", label = "Show Dots"),
                 #actionButton("stopaddpoint", label = "Stop"),
                 
                 verbatimTextOutput("info"),
                 actionButton("clear_button", label = "Clear Points"),
                 
                 #hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                 #setBackgroundImage(file("C:/coordinates/bone.jpg"))
                 #hover = hoverOpts(
                 #  id = "image_hover",
                 #  delay = 500,
                 #  delayType = "throttle"
                 #),
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
  absolutePanel(
    wellPanel(
      actionButton(icon=NULL, inputId="upload_Button ", width=160, label="Upload Photo")),
    top=200, height=200, left='167vh', width=200),
  # Next Image Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="next_Button"   , width=160, label="Next Image")),
    top=300, height=200, left='167vh', width=200),
  # Previous Image Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="prev_Button"   , width=160, label="Previous Image")),
    top=400, height=200, left='167vh', width=200),
  # Missing Point Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="missing_Button", width=160, label="Missing point")),
    top=500, height=200, left='167vh', width=200),
  # Undo Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="undo_Button", width=160, label="Undo")),
    top=600, height=200, left='167vh', width=200),
  #Scale Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="scale_Button"   , width=160, label="Scale")),
    top=700, height=200, left='167vh', width=200),
  # Done Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="done_Button", width=160, label="Done")),
    top=800, height=200, left='167vh', width=200),
)
  
server <- function(input, output, session) {
  points <- reactiveValues(x = numeric(), y = numeric())
  po <- matrix( nrow = 0, ncol = 3)
  point <- data.frame(po)
 
  
  
  
  observeEvent(input$done_Button, {
    write_csv(points_df, "points.csv")
    # Display a temporary message
    showNotification("The points you marked are saved in the points.csv file.")
    #invalidateLater(500)
  })
  
  colnames(point) <- c("id","x","y")
  

  observeEvent(input$undo_Button, {
    if (length(xy_new$x) > 0) {
      xy_new$x <- xy_new$x[1:(length(xy_new$x)-1)]
      xy_new$y <- xy_new$y[1:(length(xy_new$y)-1)]
    }
  })
  #observeEvent(input$add_null_button, {
  #  # write a null value as an x-y coordinate to the TPS file
  #  write.table(data.frame(x=NA, y=NA), "deneme.tps", sep="\t", row.names=FALSE, col.names=FALSE, append=TRUE)
  #})
  
  observeEvent(input$missing_Button, {
    xy_new$x <- c(xy_new$x, NA)
    xy_new$y <- c(xy_new$y, NA)
    
    
  })
  
  observeEvent(input$scale_Button, {
    # Get the minimum and maximum x values from the brush
    x_range <- c(input$plot_brush$xmin, input$plot_brush$xmax)
    if (length(x_range) ==2){
      x1 <- min(x_range)
      x2 <- max(x_range)
      
      # Do something with the x values, such as calculate the distance between them
      scale <- x2-x1
      print(1/scale)
    }
  })


  
  

  # By default, Shiny limits file uploads to 5MB per file.
  # modify this limit by using the shiny.maxRequestSize option. 
  # For example, adding options(shiny.maxRequestSize=30*1024^2) to the top of server.R would increase the limit to 30MB.
  
  options(shiny.maxRequestSize=100*1024^2) 
  
  
  xy_new <- reactiveValues(x= numeric(0), y = numeric(0), line=numeric(0)) # add new points
  
  
  
  output$plot.ui <- renderUI({
    plotOutput("distplot",
               click = "plot_click",
               height = "80vh",
               width = "160vh",
               #dblclick = "plot_dblclick",
               #dblclick is not shown as a point in plot
               #hover = "plot_hover",
               brush = "plot_brush",
               #str(input$plot_hover)
               
    )
  })
  
  # Listen for clicks and store values
  observe({
    if (is.null(input$plot_click)){
      return()
    }
    if(!is.null(input$plot_click)){
      isolate({
        xy_new$x <- c(xy_new$x, input$plot_click$x)
        xy_new$y <- c(xy_new$y, input$plot_click$y)
    })
    }
  })
  

  # Get the click values on button click
  pointsforplot <- eventReactive(input$plot_click, ignoreNULL = F, {
    
    tibble(x = xy_new$x, y = xy_new$y)
    
  })

  output$distplot <- renderPlot({
    
    # Will update on button click, refreshing the plot
    coord <- tibble(x = xy_new$x, y = xy_new$y)
    #to save the coordinates of the dots
    img <- readPNG("Adsız.png",)  # read the image file
    r <- dim(img)[1]  # get the number of rows in the image
    c <- dim(img)[2]  # get the number of columns in the image
    points_df <<- data.frame(x = as.numeric(xy_new$x), y = as.numeric(xy_new$y))
    coordinates <<- c(as.numeric(xy_new$x),as.numeric(xy_new$y))
    point <- rbind(point, coordinates)
    
    plot(coord$x, coord$y, xlim=c(0, dim(img)[1]), ylim=c(dim(img)[2],0), xlab="X", ylab="Y",xaxt = "n")
    axis(3)
    rasterImage(img, -25, 2000, 1000, -25)  # add the image as the background of the plot
    points(coord$x, coord$y, col=c("red", "blue", "green"))  # add the points to the plot
    
    
  })
  observeEvent(input$clear_button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 2), " y=", round(e$y, 2), "\n")
    }
   
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin="   , round(e$xmin, 2),           " xmax=", round(e$xmax, 2),
             " ymin="  , round(e$ymin, 2),           " ymax=", round(e$ymax, 2),
             " xrange=", round(e$xmax-e$xmin, 2),    " yrange=", round(e$ymax-e$ymin,2),
             " diag="  , round(sqrt((e$xmax-e$xmin)^2+(e$ymax-e$ymin)^2)))
    }

    paste0(
        "click: ", round(xy_new$x[length(xy_new$x)],2)," ",round(xy_new$y[length(xy_new$y)],2)
    )
  })
}

shinyApp(ui=ui, server=server)
