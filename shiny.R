library(shiny);
library(tidyverse)
library(jpeg)
library(shinyWidgets)




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
          top=350, height=200, left='167vh', width=200),
  
  # Previous Image Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="prev_Button"   , width=160, label="Previous Image")),
          top=500, height=200, left='167vh', width=200),
  
  # Missing Point Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="missing_Button", width=160, label="Missing point")),
          top=650, height=200, left='167vh', width=200),
  
  # Undo Button
  absolutePanel(
    wellPanel(
      actionButton(
        icon=NULL, inputId="undo_Button", width=160, label="Undo")),
          top=800, height=200, left='167vh', width=200),
  

  )
  

server <- function(input, output, session) {
  po <- matrix( nrow = 0, ncol = 3)
  point <- data.frame(po)
 
  colnames(point) <- c("id","x","y")
  
  observeEvent(input$clear_button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
  })
  

  observeEvent(input$undo_Button, {
    if (length(xy_new$x) > 0) {
      xy_new$x <- xy_new$x[1:(length(xy_new$x)-1)]
      xy_new$y <- xy_new$y[1:(length(xy_new$y)-1)]
    }
  })
  
  observeEvent(input$next_Button, {
      write.csv(point,"deneme.csv")
  })
  
  

  # By default, Shiny limits file uploads to 5MB per file.
  # modify this limit by using the shiny.maxRequestSize option. 
  # For example, adding options(shiny.maxRequestSize=30*1024^2) to the top of server.R would increase the limit to 30MB.
  
  options(shiny.maxRequestSize=100*1024^2) 
  
  
  xy_new <- reactiveValues(x= numeric(0), y = numeric(0), line=numeric(0)) # add new points
  
  
  output$plot.ui <- renderUI({
    plotOutput("distplot",
               click = "plot_click",
               dblclick = "plot_dblclick",
               
               
               height = "80vh",
               width = "160vh",
               #dblclick is not shown as a point in plot
               #hover = "plot_hover",
               #brush = "plot_brush",
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
  
  #get screen resolution.
  observe({
    cat(input$GetScreenWidth)
  })

  


  
  output$distplot <- renderPlot({
    

   
    
    
    # Will update on button click, refreshing the plot
    coord <- tibble(x = xy_new$x, y = xy_new$y)
    #to load the image
    img <- readJPEG("C:/coordinates/bone.jpg")
    
    #to save the coordinates of the dots
    fileName  <- file("coord.txt")
    imageName <- basename("C:/coordinates/bone.jpg")
    
    coordinates <<- c(as.numeric(xy_new$x),as.numeric(xy_new$y))
    point <- rbind(point, coordinates)
    
    writeLines(c(imageName, length(xy_new$x), coordinates),fileName)
    close(fileName)
    
    #to load the image
    ima <- img
    
    #pointsforplot()
    plot(coord$x, coord$y, xlim=c(-1920, 1920), ylim=c(-1080, 1080), xlab="X", ylab="Y")
    
    
    #uploads <- upload_image()
    
    #take plot info and set background photo
    lim <- par()
    #rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])s
    
    
    
    
    inc <- function(x)
    {
      eval.parent(substitute(x <- x + 1))
    }
    
    x <- 1
    observeEvent(input$missing_Button, {
      
      
      conn <- file("coord.txt",open="r")
      linn <-readLines(conn)
      lineeee <- as.integer(linn[2]) + x
     
      write(paste("\n",lineeee,"NULL"),file="coord.txt",append=TRUE)
      close(conn)
      x <<- inc(x)
      
    })
    
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
      
      mylist <- xy_str(input$plot_click),
      for(x in length(mylist)){
        Filter(Negate(is.null),mylist)
      },
      
      
        "click: "       , mylist[1],"\n",
      #print(xy_str(input$plot_click)),
      
      
      
      "double_click: ", xy_str(input$plot_dblclick)
      #"hover: "       ,cat("Hover (throttled):\n")
      #"brush: ", xy_range_str(input$plot_brush)
    )
  })
}
# Run in a dialog within R Studio
# runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1200, height = 600))
# runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
shinyApp(ui=ui, server=server)
