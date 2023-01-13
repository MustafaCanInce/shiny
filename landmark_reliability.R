library(shiny);
library(shinyWidgets)
library(shinyalert)
library(tibble)
library(jpeg)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .radius20 {
        border-radius: 20px;
        padding: 20px;
         margin: 10px;
      }
                
                    "))
  ),
  sidebarLayout(
    sidebarPanel(width=1080,
                 
                 numericInput(inputId = "scale_input", label = "Enter scale value:", value = 1, step = 1),
                 verbatimTextOutput("info"),
    ),
    mainPanel(
      uiOutput(outputId = "plot.ui") # , width = "500",height = "1000"
    )
  ),
  absolutePanel(
    wellPanel(
      fileInput(
        inputId = "image_file", label = NULL, buttonLabel = "Upload Image",multiple = TRUE, accept = ".jpg"),
      actionButton(
        icon=NULL, inputId="next_Button"   , width = 140, label="Next Image"        ,class = "radius20"),
      actionButton(
        icon=NULL, inputId="prev_Button"   , width = 140, label="Previous Image"    ,class = "radius20"),
      actionButton(
        icon=NULL, inputId="missing_Button", width = 140, label="Add Missing Point" ,class = "radius20"),
      actionButton(
        icon=NULL, inputId="undo_Button"   , width = 140, label="Undo Last Point"   ,class = "radius20"),
      actionButton(
        icon=NULL, inputId="clear_button"  , width = 140, label="Clear Points"      ,class = "radius20"),
      actionButton(
        icon=NULL, inputId="scale_Button"  , width = 140, label="Scale"             ,class = "radius20"),
      actionButton(
        icon=NULL, inputId="done_Button"   , width = 140, label="Done"              ,class = "radius20")
      ),
      
    top=200, height=200, left='167vh', width=200),
)

server <- function(input, output, session) {
  points <- reactiveValues(x = numeric(), y = numeric())
  po <- matrix( nrow = 0, ncol = 3)
  point <- data.frame(po)
  scale_value <- reactive({input$scale_input})
  
  observeEvent(input$done_Button, {
    x <- xy_new$x[xy_new$x != 0]
    y <- xy_new$y[xy_new$y != 0]
    df <- data.frame(x, y)
    write.csv(df, file = paste0(image_names$data[index$current], ".csv"), row.names = FALSE)
    shinyalert("Success!", "Image points have been saved.", type = "success")
  })
  
  colnames(point) <- c("id","x","y")
  
  observeEvent(input$undo_Button, {
    if (length(xy_new$x) > 0) {
      xy_new$x <- xy_new$x[1:(length(xy_new$x)-1)]
      xy_new$y <- xy_new$y[1:(length(xy_new$y)-1)]
    }
  })
  
  observeEvent(input$missing_Button, {
    xy_new$x <- c(xy_new$x, NA)
    xy_new$y <- c(xy_new$y, NA)
    showNotification("Success Null points have been added.")
  })
  
  observeEvent(input$scale_Button, {
    validate(need(scale_value()>0, "Scale must be greater than 0"))
    updateNumericInput(session, "scale_input", value = scale_value())
    x_range <- c(input$plot_brush$xmin, input$plot_brush$xmax)
    if (length(x_range) ==2){
      x1 <- min(x_range)
      x2 <- max(x_range)
      scale <- x2-x1
      print(scale_value()/scale)
      xy_new$x <- xy_new$x[0:(length(xy_new$x)-1)]
      xy_new$y <- xy_new$y[0:(length(xy_new$y)-1)]
    }
  })
  
  images_path <- reactiveValues(data = list())
  image_names <- reactiveValues(data = list())
  
  observeEvent(input$image_file, {
    images_path$data <- input$image_file$datapath
    image_names$data <- lapply(input$image_file$datapath, basename)
  })
  
  index <- reactiveValues(current = 1)
  
  observeEvent(input$next_Button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
    if(index$current < length(images_path$data)){
      index$current <<- index$current + 1
    } else {
      shinyalert("Oops!", "This is the last image.", type = "error")
    }
  })
  
  observeEvent(input$prev_Button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
    if(index$current > 1){
      index$current <<- index$current - 1
    } else{
      shinyalert("Oops!", "This is the first image.", type = "error")
    }
  })
  
  options(shiny.maxRequestSize=100*1024^2) 
  
  xy_new <- reactiveValues(x= numeric(0), y = numeric(0), line=numeric(0)) # add new points
  
  output$plot.ui <- renderUI({
    plotOutput("distplot",
               click = "plot_click",
               height = "80vh",
               width = "160vh",
               brush = "plot_brush",
    )
  })
  
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
  
  pointsforplot <- eventReactive(input$plot_click, ignoreNULL = F, {
    tibble(x = xy_new$x, y = xy_new$y)
  })
  
  output$distplot <- renderPlot({
    coord <- tibble(x = xy_new$x, y = xy_new$y)
    if (length(images_path$data) == 0){ return()
    }else{img <- readJPEG(images_path$data[[index$current]])}
    r <- dim(img)[1]
    c <- dim(img)[2]
    points_df <<- data.frame(x = as.numeric(xy_new$x), y = as.numeric(xy_new$y))
    coordinates <<- c(as.numeric(xy_new$x),as.numeric(xy_new$y))
    point <- rbind(point, coordinates)
    plot(coord$x, coord$y, xlim=c(0, dim(img)[1]), ylim=c(dim(img)[2],0), xlab="X", ylab="Y",xaxt = "n")
    axis(3)
    rasterImage(img, 0, dim(img)[2], dim(img)[1], 0)
    points(coord$x, coord$y, col=c("red", "blue", "green"))
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