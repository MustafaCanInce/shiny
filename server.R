server <- function(input, output, session) {
  points <- reactiveValues(x = numeric(), y = numeric())
  po <- matrix( nrow = 0, ncol = 3)
  point <- data.frame(po)
  user_name <- "user"
  knowndistance_options <- c("mm","cm","m")
  known_distance <- 1 #cm
  unitsofmetric <- "cm"
  file_path <- file.path(getwd(), "output")
  l1 <- 1
  l2 <- 2
  
  output$file_names <- renderPrint({
    file_list <- input$image_file
    if (is.null(file_list)) {
      return("")
    } else {
      file_names <- file_list$name
      file_names <- gsub("\\\\", "/", file_names)  # Fixes backslash on paths
      paste(file_names, collapse = "<br>")
    }
  })
  
  
  observeEvent(input$imputation_Button, {
    showModal(modalDialog(
      title = "Imputation Settings",
      numericInput("l1_input", "l1:", value = l1),
      numericInput("l2_input", "l2:", value = l2),
      actionButton("guess_Button", "Guess NA Landmarks"),
      actionButton("submit_imp", "Submit"),
      actionButton("close_modal", "Close"),
    ))
  })
  
  observeEvent(input$submit_imp, {
    if(input$l1_input == ""  || input$l2_input == "" ){
      shinyalert(title = "Error", 
                 text = "Please fill all the fields", 
                 type = "error", 
                 closeOnClick = "cancel"
        )
    }
    else {
      l1 <<- input$l1_input
      l2 <<- input$l2_input
      shinyalert(title = "Success", 
                 text = "Your information saved successfully!", 
                 type = "success", 
                 closeOnClick = "ok"
                 
      )
      removeModal()
      }
    })
  
  observeEvent(input$guess_Button, {
    show_modal_progress_line()
    
    ## File path : The current working directory that should include all csv files.
    ## l1,l2 : The reference anatomic landmarks
    ## We should give an error if the user did not specify the l1 l2 
    
    ## This function impute the missing landmark and fill the given dataset.
    impute.missing <- function(file_path,l1,l2) { 
      
      files <-  list.files(file_path,pattern = '.csv') ##Get all csv files into a list.
      
      # get any of the data to determine the number of the landmark (I mean the number of row)
      suppressWarnings(t_data <-  as.data.frame(read.csv(paste(file_path,files[1],sep="/"),header = TRUE,sep = ",",comment.char = "#")))
      
      nf <-  length(files) #number of image
      nr <-  nrow(t_data) #number of landmark(row)
      nc <- ncol(t_data) #number of dimension(column)(in our case it is x and y)
      rm(t_data)
      #data <- data.frame()
      
      
      #In this part of the code implemented to determine the data that contains 
      #missing landmark and adding it to the beginning of the list.
      
      vector_data <- c()
      nii<-  0 #null_image_index
      nli<- 0 # null_landmark_index 
      #This loop determine the index of the data that contains missing landmark
      i <- 1
      for(elem in files){
        suppressWarnings(temp_data <- as.data.frame(read.csv(paste(file_path,elem,sep="/"),header =TRUE,sep = ",",comment.char = "#")))
        for(is_null in is.na(temp_data)){
          if (is_null == TRUE){
            nii <- i
            break
          }
        }
        i <- i+1
      }
      #Save the null image into a variable
      suppressWarnings(null_image_data <-  as.data.frame(read.csv(paste(file_path,files[nii],sep="/"),header = TRUE,sep=",",comment.char = "#")))
      null_data_frame <-  is.na(null_image_data)
      
      #This loop determine the index of the landmark 
      i <- 1
      for (boolean in null_data_frame){
        if(boolean == TRUE){
          nli <-  i
          break
        } 
        i <- i+1
      }
      rm(null_data_frame)
      i <-  1
      for(elem in files){
        if (i != nii){
          suppressWarnings(temp_data <- as.data.frame(read.csv(paste(file_path,elem,sep="/"),header = TRUE,sep = ",",comment.char = "#")))
          vector_data <-  c(vector_data, unlist(temp_data,use.names=FALSE))
        }
        i <- i+1
      }
      
      #Adding null data to the beginning of the list 
      
      vector_data <- c(unlist(null_image_data,use.names = FALSE),vector_data)
      
      
      #Put all data into a structure
      st_data <-  structure(vector_data,.Dim = as.integer(c(nr,nc,nf)))
      rm(vector_data)
      
      #Determine the landmark that will use as a reference
      #row,column,sublist
      xl1<-st_data[l1,1,1] #x coordinate of the l1
      xl2<-st_data[l2,1,1]  #x coordinate of the l2
      
      yl1<-st_data[l1,2,1] #y coordinate of the l1
      yl2<-st_data[l2,2,1] #y coordinate of the l2
      
      #Create bookstein coordinate from the missing data
      my.dat.book<-bookstein2d(st_data)
      my.dat.book.cor<-my.dat.book$bshpv
      #veri<-my.dat.book.cor
      
      #Reformat the dataset in order to applying the F statistic.
      i=1  
      new_data<-data.frame(matrix(nrow = nf, ncol = 2))
      colnames(new_data)<-c("x", "y")
      for (i in 1:nf) {
        new_data[i,1]<-my.dat.book.cor[nli,1,i]
        new_data[i,2]<-my.dat.book.cor[nli,2,i]
      }
      
      #Applying the F approach
      
      i=2
      distance_null_to_l1<-matrix(nrow = nf, ncol = 1)
      for (i in 2:nf) {       
        temp=my.dat.book.cor[,,i]
        xc1<-temp[l1,1]  #x-coordinate of the l1 
        yc1<-temp[l1,2]  #y-coordinate of the l1
        xc3<-temp[nli,1] #x-coordinate of the null_landmark
        yc3<-temp[nli,2] #y-coordinate of the null_landmark
        distance_null_to_l1[i,1]=sqrt((xc3-xc1)^2+(yc3-yc1)^2)#Euclidean distance
      }
      
      distance_null_to_l1<-distance_null_to_l1[-1,] #remove the NA 
      mean_distance_null_to_l1=mean(distance_null_to_l1) # calculate the mean
      
      i=2
      distance_null_to_l2<-data.frame(matrix(nrow = nf, ncol = 1))
      for (i in 2:nf)  {       
        temp=my.dat.book.cor[,,i]
        xc2<-temp[l2,1]   #x-coordinate of the l2 
        yc2<-temp[l2,2]   #y-coordinate of the l2
        xc3<-temp[nli,1]  #x-coordinate of the null_landmark
        yc3<-temp[nli,2]  #y-coordinate of the null_landmark
        distance_null_to_l2[i,1]=sqrt((xc3-xc2)^2+(yc3-yc2)^2) #Euclidean distance
      }
      distance_null_to_l2<-distance_null_to_l2[-1,] #remove the NA 
      mean_distance_null_to_l2=mean(distance_null_to_l2) # calculate the mean
      
      
      c=(-1/2)*(mean_distance_null_to_l1^2-mean_distance_null_to_l2^2)  
      d=sqrt(mean_distance_null_to_l1^2-(c+0.5)^2)    
      y4<-new_data
      y4<-as.matrix(y4)
      y4[1,1]<-c
      y4[1,2]<-d
      
      #Calculate confidence interval
      d2<-distance_null_to_l1
      d2lb<-mean_distance_null_to_l1-1.96*(sd(d2)/sqrt(length(d2)))  #### lower bound
      d2ub<- mean_distance_null_to_l1 +1.96*(sd(d2)/sqrt(length(d2))) #### upper bound
      
      d3<-distance_null_to_l2
      d3lb<- mean_distance_null_to_l2 - 1.96*(sd(d3)/sqrt(length(d3)))  #### lower bound
      d3ub<- mean_distance_null_to_l2 + 1.96*(sd(d3)/sqrt(length(d3)))  #### upper bound
      
      closeAllConnections()
      
      counter<-0
      fstatt<-matrix(byrow=TRUE)
      ls<-matrix(byrow=TRUE)
      counterS<-matrix(byrow=TRUE)
      cs<-matrix(byrow=TRUE)
      ds<-matrix(byrow=TRUE)
      closeAllConnections()
      
      #Calculate the f-statistic
      for ( c in seq(d2lb, d2ub, 0.2) )  {
        for ( d in seq(d3lb, d3ub, 0.2) ) {
          counter<-counter+1
          y4[1,1]<-c
          y4[1,2]<-d
          gakt=nf*(mean(y4[,1])-mean(y4))^2+nf*(mean(y4[,2])-mean(y4))^2
          cat(gakt, sep="\n", file="gakt.txt", append=TRUE)
          sink("gkt.txt")
          k=2
          for( i in 1:k){
            for (j in 1:nf) {
              t=((y4[j,i]-mean(y4))^2)
              cat(t, sep="\n", file="gkt.txt", append=TRUE)
              j=j+1
            }
            i=i+1
          }
          gkt <- read.table("gkt.txt")
          suppressWarnings(gkt<-as.numeric(unlist(gkt)))
          gktson=sum(gkt)
          gikt=gktson-gakt
          fstat=((gakt/(k-1))/(gikt/(2*nf-k))) 
          fstatt[counter]<-fstat
          closeAllConnections()
          ##ls[counter]<-l
          counterS[counter]<-counter
          cs[counter]<-c
          ds[counter]<-d
        }
        closeAllConnections()
      }
      
      # Arrange the result for Min(F) criterion
      ts<-rbind(counterS, cs, ds, fstatt)
      ts<-t(ts)
      ts<-as.data.frame(ts)
      colnames(ts)<-c("COUNTER", "X", "Y", "F")
      minn<-ts %>% 
        slice(which.min(F))
      
      
      #Reverting from bookstein coordinates to original coordinates for Min(F)
      D=(xl2-xl1)^2+(yl2-yl1)^2
      A=xl2-xl1
      B=yl2-yl1
      ub<-  minn$X
      vb<-  minn$Y
      C= (ub+0.5)
      
      xj2={A*D*C+xl1*(A^2+B^2)-B*vb*D}/(A^2+B^2) #Predicted x-coordinate
      
      yj2={D*C-A*{{A*D*C+xl1*(A^2+B^2)-B*vb*D}/(A^2+B^2)}+A*xl1+B*yl1}/B #Predicted y-coordinate
      
      
      xmin<-as.numeric(unlist(xj2))
      ymin<-as.numeric(unlist(yj2))
      
      return(c(xmin,ymin))
    }
    remove_modal_progress()
    result <-impute.missing(file_path,l1,l2)
    if(!is.null(result)){
      files <- list.files(file_path, pattern = '.csv')
      data <- read.csv(file.path(file_path, files[1]),comment.char = "#")
      data[is.na(data$x), "x"] <- result[[1]]
      data[is.na(data$y), "y"] <- result[[2]]
      write.csv(data, file = file.path(file_path, paste0("predicted_", files[1])), row.names = FALSE)
      lapply(list.files(path = tempdir(), pattern = "file"), close)
      
      shinyalert("Success!", "Predicted landmark are saved.", type = "success")
    }
    else{
      shinyalert("Warning!", "Please check that there are csvs in the output folder.", type = "warning")
    }
  })
  
  # When input is detected, it shows a modal dialog using the "showModal" function. The modal dialog contains a title, a text input field for "name_input", a select input field for "resolution_input" with options from "screen_resolution_options" 
  # and pre-selected as the first option, a submit button with the id "submit_id" and no footer. 
  observeEvent(input$settings_id, {
    showModal(modalDialog(
      title = "Settings",
      textInput("name_input", "Name:", value = user_name, placeholder = "Enter your name"),
      selectInput("knowndistance_units_input", "Units of Lengthn:", knowndistance_options, selected = knowndistance_options[1]),
      numericInput("knowndistance_input", "Known Distance:", value = known_distance),
      actionButton("ratio_button", "Reset the Ratio"),
      actionButton("submit_id", "Submit"),
      actionButton("close_modal", "Close"),
      footer = NULL
    ))
  })
  
  # When input is detected, it checks if the "name_input" or "resolution_input" fields are empty. If either field is empty, it displays an error message using the "shinyalert" function.
  # If both fields are filled, it creates a "user_info" list with the input values, saves the list to the "user_info.rds" file and shows a success message using the "shinyalert" function.
  observeEvent(input$submit_id, {
    if(input$name_input == ""  || input$knowndistance_input == "" || input$knowndistance_units_input == ""){
      shinyalert(title = "Error", 
                 text = "Please fill all the fields", 
                 type = "error", 
                 closeOnClick = "cancel"
      )
    }
    else {
      user_info <- list(name = input$name_input, knowndistance_units = input$knowndistance_units_input, distance = input$knowndistance_input)
      saveRDS(user_info, "user_info.rds")
      shinyalert(title = "Success", 
                 text = "Your information saved successfully!", 
                 type = "success", 
                 closeOnClick = "ok"
                 
      )
      read_user_info()
      output$response <- renderText(input$name_input)
      removeModal()
    }
  })
  
  
  observeEvent(input$ratio_button, {
    ratio <<- 0
    shinyalert(title = "Success", 
               text = "Ratio resetted!", 
               type = "success", 
               closeOnClick = "ok")
  })
  observeEvent(input$close_modal, {
    removeModal()
  })
  
  # If the file exists, it reads the "name" and "resolution" values from the file and assigns them to the variables "user_name" and "screen_resolution" respectively. 
  # The "resolution" value is split by the "x" character and the first part is assigned to the "width" variable, while the second part is assigned to the "height" variable. 
  # If the file does not exist, the "screen_resolution" variable is assigned the value of 1920/1080.
  read_user_info <- function() {
    if (file.exists("user_info.rds")) {
      user_info <- readRDS("user_info.rds")
      user_name <<- user_info$name
      unitsofmetric <<- user_info$knowndistance_units
      known_distance <<- user_info$distance
    }
    else {
      shinyalert("warning!", "user_info.rds not exists.", type = "warning")
    }
  }
  
  observeEvent(input$done_Button,{
    showModal(modalDialog(
      title = "Done Settings",
      actionButton("CsvWithscale_Button", "save csv files at scale"),
      actionButton("CsvWithoutscale_Button", "save csv files without scale")
    ))
  })
  
  observeEvent(input$CsvWithscale_Button, {

    
    x <- xy_new$x[xy_new$x != 0]
    y <- xy_new$y[xy_new$y != 0]
    df <- data.frame(x, y)
    df_new <- df * known_distance/ratio
    file = paste0("scale_",image_names$data[index$current],"_",user_name,".csv")
    if(!dir.exists("output")){
      dir.create("output")
    }
    results_df_string <- ""
    results_df_string <- capture.output(print(results_df))
    results_df_string <- paste( "#", results_df_string,sep="")
    results_df_string <- paste(results_df_string, collapse = "\n")
    
    file_con <- file(paste0("output/", file), open = "w")
    
    write.csv(df_new, file = file_con, row.names = FALSE)
    writeLines(c(results_df_string), file_con)
    writeLines(c("# measured length ", paste0("#", ratio)), file_con)
    writeLines(c("# scale factor ", paste0("#", known_distance/ratio)), file_con)
    close(file_con)
    shinyalert("Success!", "Image landmarks have been saved to 'output' folder.", type = "success")
  })
  
  # The non-zero x and y values from xy_new data to a CSV file with the current index's filename from image_names, and shows an alert.
  observeEvent(input$CsvWithoutscale_Button, {
    
    x <- xy_new$x[xy_new$x != 0]
    y <- xy_new$y[xy_new$y != 0]
    df <- data.frame(x, y)
    file = paste0(image_names$data[index$current],"_",user_name,".csv")
    if(!dir.exists("output")){
      dir.create("output")
    }
    results_df_string <- ""
    results_df_string <- capture.output(print(results_df))
    results_df_string <- paste( "#", results_df_string,sep="")
    results_df_string <- paste(results_df_string, collapse = "\n")
    
    file_con <- file(paste0("output/", file), open = "w")
    
    write.csv(df, file = file_con, row.names = FALSE)
    writeLines(c(results_df_string), file_con)
    close(file_con)
    shinyalert("Success!", "Image landmarks have been saved to 'output' folder.", type = "success")
  })
  
  colnames(point) <- c("id","x","y")
  
  # Checks if there are any landmarks(points) stored in xy_new data, and if so, it removes the last landmark(point).
  observeEvent(input$undo_Button, {
    if (length(xy_new$x) > 0) {
      xy_new$x <- xy_new$x[0:(length(xy_new$x)-1)]
      xy_new$y <- xy_new$y[0:(length(xy_new$y)-1)]
    }
  })
  
  # Adds a missing value (NA) to the x vectors of xy_new data. And shows a notification .
  observeEvent(input$missing_Button, {
    xy_new$x <- c(xy_new$x, NA)
    xy_new$y <- c(xy_new$y, NA)
    showNotification("Success, Null Landmarks have been added.")
  })
  
  
  
  # Captures the selected range of x-axis coordinates from a plot brush and calculates the scale. A variable "known_distance" is set to 1 cm and "known_pixels" is set to 37.7957517575025 pixels. 
  # A scale factor is calculated by dividing known_distance by known_pixels and multiplying by "screen_resolution". 
  # It creates an observer that listens to the "scale_Button" input. When the input is detected it checks if two landmarks(points) are selected on the x-axis of the plot. 
  # If two landmarks(points) are selected, it calculates the distance in pixels between them, multiplies it by the scale factor and shows a notification with the calculated distance in cm. It also removes the two last landmarks(points) from xy_new$x and xy_new$y, 
  # if not it shows a message that at least two landmarks(points) need to be selected before the button is pressed.
  
  # Captures the selected range of x-axis coordinates from the plot brush and calculates the scale.
  
  ratio <- 0
  results_df <- data.frame(distance_between_two_landmarks = numeric(), unitsofmetric = character())
  observeEvent(input$scale_Button, {
    
    if(length(xy_new$x) >= 2 && ratio == 0) {
      delta_x <- xy_new$x[length(xy_new$x)] - xy_new$x[length(xy_new$x)-1]
      delta_y <- xy_new$y[length(xy_new$y)] - xy_new$y[length(xy_new$y)-1]
      result <- sqrt(delta_x^2 + delta_y^2)
      ratio <<- result / known_distance
      showNotification(paste0("scaling was successful calibrated. ratio is:",ratio))
      
      for(i in 1:2){
        xy_new$x <- xy_new$x[0:(length(xy_new$x)-1)]
        xy_new$y <- xy_new$y[0:(length(xy_new$y)-1)]
      }
      
    } 
    else if(ratio != 0){
      delta_x <- xy_new$x[length(xy_new$x)] - xy_new$x[length(xy_new$x)-1]
      delta_y <- xy_new$y[length(xy_new$y)] - xy_new$y[length(xy_new$y)-1]
      result <- sqrt(delta_x^2 + delta_y^2)
      if(unitsofmetric == "cm"){
        distance_between_two_landmarks <- result / ratio
      } else if(unitsofmetric == "mm"){
        result = result * 100
        distance_between_two_landmarks <- result / ratio
      } else if(unitsofmetric == "m"){
        result = result / 100
        distance_between_two_landmarks <- result / ratio
      }
      showNotification(paste0("scaling was successful measured distance: ",distance_between_two_landmarks," ",unitsofmetric))
      results_df <<- results_df %>% add_row(distance_between_two_landmarks = distance_between_two_landmarks, unitsofmetric = unitsofmetric)
      
      for(i in 1:2){
        xy_new$x <- xy_new$x[0:(length(xy_new$x)-1)]
        xy_new$y <- xy_new$y[0:(length(xy_new$y)-1)]
      }
    }
    else {
      showNotification(paste0("Click on at least two landmarks on the plot and then press the scale button."))
      
    }
  })
  
  images_path <- reactiveValues(data = list())
  image_names <- reactiveValues(data = list())
  
  # Assigns the path of the uploaded file to the images_path$data variable,
  # and then applies the basename function to the file path to extract the file name and assigns it to the image_names$data variable.
  # This allows the user to access the path and name of the image.
  observeEvent(input$image_file, {
    images_path$data <- input$image_file$datapath
    image_names$data <- lapply(input$image_file$datapath, basename)
    ratio <<- 0
  })
  
  index <- reactiveValues(current = 1)
  
  # Resets xy_new$x and xy_new$y to empty numeric vectors. 
  # Then it checks if the current index is less than the total number of images, if so, it increments the current index by 1. 
  observeEvent(input$next_Button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
    results_df <<- data.frame(distance_between_two_landmarks = numeric(), unitsofmetric = character())
    if(index$current < length(images_path$data)){
      index$current <<- index$current + 1
      ratio <<- 0
    }
    else {
      shinyalert("Oops!", "This is the last image.", type = "error")
    }
  })
  
  # Resets xy_new$x and xy_new$y to empty numeric vectors. 
  # Then it checks if the current index is greater than 1, if so, it decrements the current index by 1.
  observeEvent(input$prev_Button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
    results_df <<- data.frame(distance_between_two_landmarks = numeric(), unitsofmetric = character())
    if(index$current > 1){
      index$current <<- index$current - 1
      ratio <<- 0
    }
    else {
      shinyalert("Oops!", "This is the first image.", type = "error")
    }
  })
  
  # Function to set the shiny.maxRequestSize option to 100 * 1024 ^ 2. This sets the maximum allowed file size to 100 megabytes.
  options(shiny.maxRequestSize=100*1024^2) 
  
  xy_new <- reactiveValues(x= numeric(0), y = numeric(0), line=numeric(0)) # add new landmarks
  
  # Creates a "distplot" plot output, plot is responsive to clicks with an event handler named "plot_click"
  # and it allows the user to select a range of x-coordinates using the brush tool, with an event handler named "plot_brush".
  height <- 0
  width <- 0
  output$plot.ui <- renderUI({
    if (length(images_path$data) == 0){
      return() 
    }
    else {
      img_extension <- sub(".*\\.([[:alnum:]]+)$", "\\1", images_path$data[[index$current]])
      if(img_extension == "jpg" || img_extension == "jpeg"){
        img <- readJPEG(images_path$data[[index$current]])
      } 
      else if(img_extension == "png"){
        img <- readPNG(images_path$data[[index$current]])
      }
      else if(img_extension == "tif" || img_extension == "tiff"){
        img <- tiff::readTIFF(images_path$data[[index$current]])
      }
      else {
        shinyalert("Oops!", "Invalid file type. Please upload JPEG, JPG, PNG or Tiff image.", type = "error")
        return()
      }
    }
    
    plot_size <- 50
    
    height <- 1250
    width <- dim(img)[2] * (plot_size / dim(img)[1]) * 25
    
    if (is.na(height) || height < 0 || is.na(width) || width < 0) {
      return(NULL) 
    }
    
    tags$div(
      style = "position: absolute; left: 20%;", 
      plotOutput(
        "distplot",
        click = "plot_click",
        height= height,
        width = width,
        hover = "plot_hover"
      )
    )
    
  })
  
  output$coords <- renderPrint({
    input$plot_hover
  })
  
  observe({
    coords <- input$plot_hover
    if (!is.null(coords)) {
      output$coords <- renderText({
        paste0("Hover: ", "X: ", round(coords$x,4), " Y: ", round(coords$y,4))
      })
    }
  })
  
  
  # Watch for a click event on the plot created by the "plot_click" event handler. When a click event occurs,
  # it checks that the event is not null and if it is not, it isolates the event by creating a new environment.
  # Then, it adds the x and y coordinates of the click to the xy_new$x and xy_new$y vectors, respectively.
  # If the event is null, it will return nothing.
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
  
  # Listens to the "plot_click" event and creates a new tibble with the x and y coordinates stored in the xy_new$x and xy_new$y vectors, respectively. 
  # It will update the tibble every time the event is triggered,
  # This tibble is used to plot the landmarks on the plot.
  pointsforplot <- eventReactive(input$plot_click, ignoreNULL = F, {
    tibble(x = xy_new$x, y = xy_new$y)
  })
  

  # First creates a new tibble named coord with x and y coordinates stored in the xy_new$x and xy_new$y vectors. Then, it checks if there are any images available, if not, it returns nothing. 
  # If there are images, it reads the image located at the current index in images_path$data and assigns it to the variable img. Then it creates variables r and c to store the height and width of the image respectively. 
  # It creates a dataframe named points_df and assigns the values of x and y from xy_new. It also creates a variable coordinates and assigns the values of x and y from xy_new. It concatenates the new coordinates to the landmark(point) variable.
  # Finally, it plots the coordinates on the plot, sets the x and y axis limits to the dimensions of the image, adds the image as a background and plots the landmarks(points) with different color options.
  output$distplot <- renderPlot({
    coord <- tibble(x = xy_new$x, y = xy_new$y)
    if (length(images_path$data) == 0){
      return() 
    }
    else {
      img_extension <- sub(".*\\.([[:alnum:]]+)$", "\\1", images_path$data[[index$current]])
      if(img_extension == "jpg" || img_extension == "jpeg"){
        img <- readJPEG(images_path$data[[index$current]])
      } 
      else if(img_extension == "png"){
        img <- readPNG(images_path$data[[index$current]])
      }
      else if(img_extension == "tif" || img_extension == "tiff"){
        img <- tiff::readTIFF(images_path$data[[index$current]])
      }
      else {
        shinyalert("Oops!", "Invalid file type. Please upload JPEG,JPG or PNG image.", type = "error")
        return()
      }
    }
    height <- dim(img)[1]
    width <- dim(img)[2]
    screen_resolution <<- (width/height)
    points_df <<- data.frame(x = as.numeric(xy_new$x), y = as.numeric(xy_new$y))
    coordinates <- c(as.numeric(xy_new$x),as.numeric(xy_new$y))
    point <- rbind(point, coordinates)

    plot(coord$x, coord$y, xlim=c(0, dim(img)[2]), ylim=c(0, dim(img)[1]), xlab="X", ylab="Y", xaxt = "n", yaxt = "n")
    axis(3, at = seq(0, dim(img)[2], by=50))
    axis(2, at = seq(0, dim(img)[1], by=50), las=2)
    
    rasterImage(img, 0, 0, dim(img)[2], dim(img)[1])
    points(coord$x, coord$y, col=c("red"), cex=2, pch=20)
    
    if(nrow(coord)>0){
      for (i in 1:nrow(coord)) {
        text(coord$x[i], coord$y[i], labels = i, pos = 4, col = "red", cex = 2)
      }
    }
  })
  
  # Function that listens to the "input$clear_button" event. When this event is triggered, 
  # the function clears the values of xy_new$x and xy_new$y, by setting them to numeric(0) which is an empty numeric vecto
  observeEvent(input$clear_button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
  })
  
  # Creates two functions xy_str() and xy_range_str() which takes an object that holds x,y coordinates. The first function xy_str() first checks if the passed argument is NULL or not.
  # If it's NULL, it returns the string "NULL\n". If the argument is not NULL the function takes the x and y values of the coordinates and rounds them to two decimal places using round() function.
  # Second function xy_range_str() is similar to the first one, it takes a range object that holds xmin, xmax, ymin, ymax values. It checks if the passed argument is NULL or not. If it's NULL, it returns the string "NULL\n". 
  # Then it calculates xrange, yrange and diagonal distance using euclidean distance formula.
  # After that, the function paste0() is used to concatenate the last click landmark(point) coordinates by getting the last elements of xy_new$x and xy_new$y which are x and y respectively and rounding them to two decimal places.
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
