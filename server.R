server <- function(input, output, session) {
  points <- reactiveValues(x = numeric(), y = numeric())
  po <- matrix( nrow = 0, ncol = 3)
  point <- data.frame(po)
  user_name <- ""
  knowndistance_options <- c("cm", "mm", "m")
  known_distance <- 1 #mm
  unitsofmetric <- "mm"
  file_path <- file.path(getwd(), "output")
  l1 <- 1
  l2 <- 2
  my_file_path <- list(getwd(),"")
  loaded <<- FALSE
  dfs <<- list()


  observeEvent(input$plots_button, {
    shinyjs::show("plot_div")
    shinyjs::hide("ui_div")
    shinyjs::hide("img_div")
  })

  observeEvent(input$close_plots_Button, {
    shinyjs::hide("plot_div")
    shinyjs::show("ui_div")
  })

  observeEvent(input$help_button, {
    shinyjs::show("img_div")
    shinyjs::hide("plot_div")
    shinyjs::hide("ui_div")
  })

  observeEvent(input$close_help_button, {
    shinyjs::hide("img_div")
    shinyjs::show("ui_div")
  })

  panel_names <- c("imputation_panel", "marking_panel", "done_panel", "inter_reliability_panel",
                   "settings_panel", "scale_panel", "help_panel", "plots_panel")

  show_hide_panels <- function(show_panel) {
    for (panel_name in panel_names) {
      if (panel_name == show_panel) {
        shinyjs::show(panel_name)
        if (panel_name == "imputation_panel" || panel_name == "marking_panel" || panel_name == "done_panel" ||
            panel_name == "inter_reliability_panel" || panel_name == "settings_panel" || panel_name == "scale_panel") {
          shinyjs::hide("plot_div")
          shinyjs::hide("img_div")
          shinyjs::show("ui_div")
        }
      } else {
        shinyjs::hide(panel_name)
      }
    }
  }

  os_type <- 0

  if (identical(.Platform$OS.type, "windows")) {
    shinyDirChoose(input, "folder", roots = c(home = 'C:/'), session = session)
    os_type <<- 0
  }
  else if (identical(.Platform$OS.type, "mac")) {
    shinyDirChoose(input, "folder", roots = c(home = '~/'), session = session)
    os_type <<- 1
  }
  else {
    shinyDirChoose(input, "folder", roots = c(home = '/'), session = session)
    os_type <<- 2
  }

  output$filepath_text_print <- renderPrint({
    if (!is.list(input$folder) && 0 == input$folder) {
      return(as.character(my_file_path[[1]]))
    }
    my_string <- (gsub(" ", "/", paste(input$folder)))
    if (os_type == 0) {
      my_file_path <<- gsub("^\\s+|\\s+$", "", paste0("C:", gsub("C:home", "", gsub("\\\\|\\\"|list\\(|\\)|,", "", my_string))))
    }
    else if (os_type == 1) {
      my_file_path <<- gsub("^\\s+|\\s+$", "", paste0("/", gsub("C:home", "", gsub("\\\\|\\\"|list\\(|\\)|,", "", my_string))))
    }
    else if (os_type == 2) {
      my_file_path <<- gsub("^\\s+|\\s+$", "", paste0("/", gsub("C:home", "", gsub("\\\\|\\\"|list\\(|\\)|,", "", my_string))))
    }
    cat(my_file_path[1])
  })

  observeEvent(input$csv_input_plots_button, {
    dfs <<- list()
    for (i in seq_along(input$csv_input_plots_button$name)) {
      if (endsWith(input$csv_input_plots_button$name[i], ".csv")) {
        dfs[[i]] <<- read.csv(input$csv_input_plots_button$datapath[i], comment.char = '#')
      } else{
        shinyalert("Warning!", "Only csv files are accepted. Please upload csv files.", type = "warning")
        return()
      }
    }

    if (length(dfs) == 2) {
      loaded <<- TRUE
    } else {
      shinyalert("Warning!", "Please upload two .csv files with data in them.", type = "warning")
    }

  })


  observeEvent(input$draw_plot_button, {
    if (loaded) {
      if (input$type_plo_radio_button == "Heatmap") {
        dataframe_name <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                   c("x", "y", "raters", "landmark"))
        rater_count = 1
        for (dataframe_i in dfs) {
          dataframe_i$raters <- rater_count
          dataframe_i$landmark <- seq_len(nrow(dataframe_i))
          dataframe_name <- rbind(dataframe_name, dataframe_i)
          rater_count = rater_count + 1
        }
        euclidean_distances <- data.frame(landmarks_rater1 = c(), landmarks_rater2 = c(), distance = c())
        for (i in 1:nrow(dataframe_name)) {
          for (j in 1:nrow(dataframe_name)) {
            if (dataframe_name$raters[i] != dataframe_name$raters[j]) {
              euclidean_distances <- rbind(euclidean_distances, data.frame(landmarks_rater1 = dataframe_name$landmark[i], landmarks_rater2 = dataframe_name$landmark[j], distance_in_pixels = sqrt((dataframe_name$x[i] - dataframe_name$x[j])^2 + (dataframe_name$y[i] - dataframe_name$y[j])^2)))
            }
          }
        }
        euclidean_distances$landmarks_rater1 <- factor(euclidean_distances$landmarks_rater1)
        euclidean_distances$landmarks_rater2 <- factor(euclidean_distances$landmarks_rater2)
        rater_heatmap <- ggplot(euclidean_distances, aes(landmarks_rater1, landmarks_rater2)) +
          geom_tile(aes(fill = distance_in_pixels)) +
          scale_fill_gradient(low = "white", high = "red") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
          labs(fill="Distance",
               title = "Euclidean Distance Heat Map",
               x = "Landmarks Rater 1",
               y = "Landmarks Rater 2")
        output$plot <- renderPlot({
          print(rater_heatmap)
        })
      }
      else{
        dataframe_name <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                   c("x", "y", "raters", "landmark"))

        rater_count = 1
        for (dataframe_i in dfs) {
          dataframe_i$raters <- rater_count
          dataframe_i$landmark <- seq_len(nrow(dataframe_i))
          dataframe_name <- rbind(dataframe_name, dataframe_i)
          rater_count = rater_count + 1
        }

        scatterPlot <- ggplot(dataframe_name, aes(x, y)) +
          geom_point(aes(shape=factor(raters), color=factor(landmark)))+
          labs(shape="Rater ID", colour="Landmark Index", title="Landmark Scatter Plot")
        output$plot <- renderPlot({
          print(scatterPlot)
        })
      }
    }else{
      shinyalert("Warning!", "Please upload two .csv files with data in them.", type = "warning")
    }

  })

  output$file_names <- renderText({
    file_list <- input$image_file
    if (is.null(file_list)) {
      return("")
    }
    else {
      file_names <- file_list$name
      file_names <- gsub("\\\\", "/", file_names)
      file_names <- gsub(" ", "", file_names)
      file_names_list <<- strsplit(file_names, " ")
      file_names_only <- sapply(file_names_list, `[`, 1)
      return(paste(file_names_only, collapse = "/"))
    }
  })



  observeEvent(input$imputation_Button, { show_hide_panels("imputation_panel") })
  observeEvent(input$markings_Button, { show_hide_panels("marking_panel") })
  observeEvent(input$done_Button, { show_hide_panels("done_panel") })
  observeEvent(input$settings_id, { show_hide_panels("settings_panel") })
  observeEvent(input$show_scale_Button, { show_hide_panels("scale_panel") })
  observeEvent(input$help_button, { show_hide_panels("help_panel") })
  observeEvent(input$plots_button, { show_hide_panels("plots_panel") })
  observeEvent(input$show_inrel_Button, { show_hide_panels("inter_reliability_panel") })

  observeEvent(input$close_imp_panel, { shinyjs::hide("imputation_panel") })
  observeEvent(input$close_markings_Button, { shinyjs::hide("marking_panel") })
  observeEvent(input$close_donepanel_Button, { shinyjs::hide("done_panel") })
  observeEvent(input$close_settings_panel, { shinyjs::hide("settings_panel") })
  observeEvent(input$close_scale_panel, { shinyjs::hide("scale_panel") })
  observeEvent(input$close_help_button, { shinyjs::hide("help_panel") })
  observeEvent(input$close_plots_Button, { shinyjs::hide("plots_panel") })
  observeEvent(input$close_interrel_button, { shinyjs::hide("inter_reliability_panel") })






  imp_method <- ""

  observeEvent(input$calculate_imp, {
    # Check if the 'l1_input' or 'l2_input' field is empty
    if (input$l1_input == ""  || input$l2_input == "" ) {

      shinyalert(title = "Error",
                 text = "Please choose the reference anatomic landmarks.",
                 type = "error",

      )
    }
    file_path <- input$imp_csv_input
    files <- list.files(file_path, pattern = '.csv')
    if (is.null(input$imp_csv_input) || input$imp_csv_input == "") {
      shinyalert("Warning!", "imp_csv_input cannot be empty.", type = "warning")
      return()
    } else if (!file.exists(input$imp_csv_input)) {
      shinyalert("Warning!", "Invalid data path: rel_path1_input", type = "error")
      return()
    }


    else {
      # If both fields have been filled, save their values to the global variables 'l1' and 'l2'
      if (input$imp_radio_button == "minF Method") {
        imp_method <<- "minf"
      }
      else if (input$imp_radio_button == "Multiple Regression Method") {
        imp_method <<- "mr"
      }
      else{
        imp_method <<- "Expected Maximization Method"
      }
      l1 <<- input$l1_input
      l2 <<- input$l2_input

    }

    show_modal_progress_line()
    ## File path : The current working directory that should include all csv files.
    ## l1,l2 : The reference anatomic landmarks
    ## We should give an error if the user did not specify the l1 l2

    ## This function impute the missing landmark and fill the given dataset.
    ## File path : The current working directory that should include all csv files.
    ## l1,l2 : The reference anatomic landmarks
    ## We should give an error if the user did not specify the l1 l2
    impute.multiple.missing <- function(file_path,l1,l2,imp_method){

      ####check file path is exist
      if (!file.exists(file_path)) {
        shinyalert("Warning!", "Invalid data path: input_path", type = "error")
        return()
      }


      files <-  list.files(file_path,pattern = '.csv') ##Get all csv files into a list.
      ####check file path is empty
      if(length(files) == 0){
        shinyalert("Error!", "Invalid data path: rel_path1_input", type = "error")
        return()
      }

      landmark_list <- turn.to.df.list(file_path = file_path)
      na_number <- how.many.na.file(landmark_list)
      null_file_name <- vector(mode = 'list',length = as.numeric(na_number[1]))
      i <- 1
      for (file in files) {
        suppressWarnings(temp_data <- as.data.frame(read.csv(paste(file_path,file,sep = "/"),header = TRUE,sep = ",",comment.char = "#")))
        if(this.file.has.na(temp_data)){
          null_file_name[[i]] <- file
          i <- i + 1
        }
      }
      if (na_number[1] == 1) {
        single_vector <- vector(mode = 'list',length = 1)
        result <- impute.missing(file_path,landmark_list,l1,l2,imp_method)
        single_vector[[1]] <- c(result[1],result[2],c(result[3],result[4]))
        return(single_vector)
      }
      else {
        null_files <- vector(mode = 'list',length = length(na_number[2]))
        l <- 1
        for (i in 1:length(landmark_list)) {
          if (this.file.has.na(landmark_list[[i]])) {
            null_files[[l]] <- landmark_list[[i]]
            l <- l + 1

          }
        }

        i <- 1

        for (elem in na_number[[2]]) {
          landmark_list[[elem]] <- NULL
        }

        result_list <- vector(mode = 'list',length = length(null_files))

        #null_files_names <- vector(mode='list',length = length(null_files))
        indices <- vector(mode = 'list',length = length(null_files))

        for (i in 1:length(null_files)){
          temp_list <- landmark_list
          temp_list[[length(temp_list) +1]] <- null_files[[length(null_files)]]
          null_files[[length(null_files)]] <- NULL
          result <- impute.missing(file_path,landmark_list,l1,l2,imp_method)
          indice <- as.numeric(result[1])
          x <- as.numeric(result[3])
          y <- as.numeric(result[4])
          #landmark_list[[length(landmark_list)]]$x[indice] <- x
          #landmark_list[[length(landmark_list)]]$y[indice] <- y
          #result_list[[i]] <- c(x,y)
          #null_files_names[[i]] <- result[2]
          #indices[[i]] <- indice
          result_list[[i]] <-c(indice,null_file_name[[i]],c(x,y))
          remove(result)
        }
        return(result_list)
      }

    }
    ## This function impute the missing landmark and fill the given dataset.



    how.many.na.file <- function(df_list){
      null_number <- 0
      indice_list <- list()
      i <- 1
      for (elem in df_list) {
        for (is_null in is.na(elem)) {
          if (is_null == TRUE) {
            null_number <- null_number + 1
            indice_list <- append(indice_list,i)
            break
          }
        }
        i <- i + 1
      }
      return(c(null_number,lapply(list(as.numeric(indice_list)),sort,decreasing = TRUE)))
    }

    turn.to.df.list <- function(file_path){
      files <-  list.files(file_path , pattern = '.csv')
      df_list <- vector(mode = 'list',length = length(files))
      i <- 1
      for (file in files) {
        suppressWarnings(temp_data <- as.data.frame(read.csv(paste(file_path,file,sep = "/"),header = TRUE,sep = ",",comment.char = "#")))
        df_list[[i]] <- temp_data
        i <- i + 1
      }

      return(df_list)
    }

    this.file.has.na <- function(data){
      for (is_null in is.na(data)) {
        if (is_null == TRUE) {
          return(TRUE)
        }
      }
      return(FALSE)
    }
    ## This function impute the missing landmark and fill the given dataset.

    impute.missing <- function(file_path,lm_list,l1,l2,imp_method) {

      files <-  list.files(file_path,pattern = '.csv') ##Get all csv files into a list.

      #lm_list <- turn.to.df.list(file_path = file_path)
      #na_number <- how.many.na.file(lm_list)


      # get any of the data to determine the number of the landmark (I mean the number of row)
      #suppressWarnings(t_data <-  as.data.frame(read.csv(paste(file_path,files[1],sep = "/"),header = TRUE,sep = ",",comment.char = "#")))

      nf <-  length(lm_list) #number of image
      nr <-  nrow(lm_list[[1]]) #number of landmark(row)
      nc <- ncol(lm_list[[1]]) #number of dimension(column)(in our case it is x and y)
      #rm(t_data)
      #data <- data.frame()

      if(imp_method == 'em' & nr <6 ){
        shinyalert("Error",'Expectation Maximization algorithm runs on at least 6 landmark. Your landmark numbers is not enough for it',type = 'error')
        return()
      }

      for (elem in lm_list) {
        if (nrow(elem) != nr) {
          shinyalert("Error!", "Please check the length of the csv's.", type = "error")
        }
      }


      #In this part of the code implemented to determine the data that contains
      #missing landmark and adding it to the beginning of the list.

      vector_data <- c()
      nii <-  0 #null_image_index
      nli <- 0 # null_landmark_index
      #This loop determine the index of the data that contains missing landmark
      i <- 1
      for (elem in lm_list) {
        for (is_null in is.na(elem)) {
          if (is_null == TRUE) {
            nii <- i
            break
          }
        }
        i <- i + 1
      }
      #Save the null image into a variable
      null_image_data <- lm_list[[nii]]
      null_data_frame <-  is.na(null_image_data)
      null_file_name <- gsub(" ","",files[nii])
      #This loop determine the index of the landmark
      i <- 1
      for (boolean in null_data_frame) {
        if (boolean == TRUE) {
          nli <-  i
          break
        }
        i <- i + 1
      }
      rm(null_data_frame)

      i <-  1
      vector_data <- c()
      for (elem in lm_list) {
        if (i != nii) {
          vector_data <- c(vector_data,unlist(elem,use.names = FALSE))
        }
        i <- i + 1
      }


      #Adding null data to the beginning of the list

      vector_data <- c(unlist(null_image_data,use.names = FALSE),vector_data)
      #Put all data into a structure
      st_data <-  structure(vector_data,.Dim = as.integer(c(nr,nc,nf)))
      rm(vector_data)

      #Determine the landmark that will use as a reference
      #row,column,sublist
      xl1 <- st_data[l1,1,1] #x coordinate of the l1
      xl2 <- st_data[l2,1,1]  #x coordinate of the l2
      yl1 <- st_data[l1,2,1] #y coordinate of the l1
      yl2 <- st_data[l2,2,1] #y coordinate of the l2
      D = (xl2 - xl1)^2 + (yl2 - yl1)^2
      A = xl2 - xl1
      B = yl2 - yl1

      #Create bookstein coordinate from the missing data
      my.dat.book <- bookstein2d(st_data)
      my.dat.book.cor <- my.dat.book$bshpv
      em_data <- my.dat.book.cor

      #Reformat the dataset in order to applying the F statistic.
      i = 1
      new_data <- data.frame(matrix(nrow = nf, ncol = 2))
      colnames(new_data) <- c("x", "y")


      for (i in 1:nf) {
        new_data[i,1] <- my.dat.book.cor[nli,1,i]
        new_data[i,2] <- my.dat.book.cor[nli,2,i]
      }

      #Applying the F approach

      if (imp_method == "minf") {
        i = 2
        distance_null_to_l1 <- matrix(nrow = nf, ncol = 1)
        for (i in 2:nf) {
          temp = my.dat.book.cor[,,i]
          xc1 <- temp[l1,1]  #x-coordinate of the l1
          yc1 <- temp[l1,2]  #y-coordinate of the l1
          xc3 <- temp[nli,1] #x-coordinate of the null_landmark
          yc3 <- temp[nli,2] #y-coordinate of the null_landmark
          distance_null_to_l1[i,1] = sqrt((xc3 - xc1)^2 + (yc3 - yc1)^2)#Euclidean distance
        }

        distance_null_to_l1 <- distance_null_to_l1[-1,] #remove the NA
        mean_distance_null_to_l1 = mean(distance_null_to_l1) # calculate the mean

        i = 2
        distance_null_to_l2 <- data.frame(matrix(nrow = nf, ncol = 1))
        for (i in 2:nf)  {
          temp = my.dat.book.cor[,,i]
          xc2 <- temp[l2,1]   #x-coordinate of the l2
          yc2 <- temp[l2,2]   #y-coordinate of the l2
          xc3 <- temp[nli,1]  #x-coordinate of the null_landmark
          yc3 <- temp[nli,2]  #y-coordinate of the null_landmark
          distance_null_to_l2[i,1] = sqrt((xc3 - xc2)^2 + (yc3 - yc2)^2) #Euclidean distance
        }

        distance_null_to_l2 <- distance_null_to_l2[-1,] #remove the NA

        mean_distance_null_to_l2 = mean(distance_null_to_l2) # calculate the mean


        c = (-1/2)*(mean_distance_null_to_l1^2 - mean_distance_null_to_l2^2)
        d = sqrt(mean_distance_null_to_l1^2 - (c + 0.5)^2)
        y4 <- new_data
        y4 <- as.matrix(y4)
        y4[1,1] <- c
        y4[1,2] <- d

        #Calculate confidence interval
        d2 <- distance_null_to_l1
        d2lb <- mean_distance_null_to_l1 - 1.96*(sd(d2)/sqrt(length(d2)))  #### lower bound
        d2ub <- mean_distance_null_to_l1 + 1.96*(sd(d2)/sqrt(length(d2))) #### upper bound

        d3 <- distance_null_to_l2
        d3lb <- mean_distance_null_to_l2 - 1.96*(sd(d3)/sqrt(length(d3)))  #### lower bound
        d3ub <- mean_distance_null_to_l2 + 1.96*(sd(d3)/sqrt(length(d3)))  #### upper bound

        closeAllConnections()

        counter <- 0

        fstatt <- matrix(byrow = TRUE)
        ls <- matrix(byrow = TRUE)
        counterS <- matrix(byrow = TRUE)
        cs <- matrix(byrow = TRUE)
        ds <- matrix(byrow = TRUE)
        closeAllConnections()

        #Calculate the f-statistic
        for (c in seq(d2lb, d2ub, 0.2) )  {
          for (d in seq(d3lb, d3ub, 0.2) ) {
            counter <- counter + 1
            y4[1,1] <- c
            y4[1,2] <- d
            gakt = nf*(mean(y4[,1]) - mean(y4))^2 + nf*(mean(y4[,2]) - mean(y4))^2
            cat(gakt, sep = "\n", file = "gakt.txt", append = TRUE)
            sink("gkt.txt")
            k = 2
            for (i in 1:k) {
              for (j in 1:nf) {
                t = ((y4[j,i] - mean(y4))^2)
                cat(t, sep = "\n", file = "gkt.txt", append = TRUE)
                j = j + 1
              }
              i = i + 1
            }
            gkt <- read.table("gkt.txt")
            suppressWarnings(gkt <- as.numeric(unlist(gkt)))
            gktson = sum(gkt)
            gikt = gktson - gakt
            fstat = ((gakt/(k - 1))/(gikt/(2*nf - k)))
            fstatt[counter] <- fstat
            closeAllConnections()
            ##ls[counter]<-l
            counterS[counter] <- counter
            cs[counter] <- c
            ds[counter] <- d
          }
          closeAllConnections()
        }

        # Arrange the result for Min(F) criterion
        ts <- rbind(counterS, cs, ds, fstatt)
        ts <- t(ts)
        ts <- as.data.frame(ts)
        colnames(ts) <- c("COUNTER", "X", "Y", "F")
        minn <- ts %>%
          slice(which.min(F))

        unlink(c("gkt.txt","gakt.txt"))
        #Reverting from bookstein coordinates to original coordinates for Min(F)

        ub <- minn$X
        vb <- minn$Y
        C = (ub + 0.5)

        xj2 = {A*D*C + xl1*(A^2 + B^2) - B*vb*D}/(A^2 + B^2) #Predicted x-coordinate

        yj2 = {D*C - A*{{A*D*C + xl1*(A^2 + B^2) - B*vb*D}/(A^2 + B^2)} + A*xl1 + B*yl1}/B #Predicted y-coordinate

        xmin <- as.numeric(unlist(xj2))
        ymin <- as.numeric(unlist(yj2))


        return(c(nli,null_file_name,xmin,ymin))
      }
      else if (imp_method == "mr") {
        counter_mr <- 0
        mr_data <- new_data
        xt_mrs <- matrix(byrow = TRUE)
        yt_mrs <- matrix(byrow = TRUE)
        colnames(mr_data) <- c("v1", "v2")

        imputed_Data <- mice(seed = 100, mr_data, method = "norm",print = FALSE, remove.collinear = FALSE,threshold = 1.0,max.cor = 1.0 )
        xt_mr = imputed_Data$imp$v1[1,1]  ####Tahmin edilen x koordinatı
        yt_mr = imputed_Data$imp$v2[1,1]  ####Tahmin edilen y koordinatı
        xt_mrs[counter_mr] = xt_mr
        yt_mrs[counter_mr] = yt_mr

        ub_mr <- xt_mr
        vb_mr <- yt_mr
        C_mr = (ub_mr + 0.5)

        yj2_mr = {D*C_mr - A*{{A*D*C_mr + xl1*(A^2 + B^2) - B*vb_mr*D}/(A^2 + B^2)} + A*xl1 + B*yl1}/B####Tahmin edilen y koordinatı


        xj2_mr = {A*D*C_mr + xl1*(A^2 + B^2) - B*vb_mr*D}/(A^2 + B^2) ####Tahmin edilen x koordinatı

        #{C_mr*D+A*xl1-B*yj+B*yl1}/A
        xj2_mr <- as.numeric(unlist(xj2_mr))

        yj2_mr <- as.numeric(unlist(yj2_mr))
        return(c(nli,null_file_name,xj2_mr,yj2_mr))
      }

      else if (imp_method == "em") {

        i = 1
        indices_list_1 <- list()
        indices_list_2 <- list()
        indices_list_2 <- append(indices_list_2,1)
        while (i <= nr - 2) {
          indice1 = nf*i
          indice2 = indice1 + 1
          indices_list_1 <- append(indices_list_1,indice1)
          indices_list_2 <- append(indices_list_2,indice2)
          i <- i + 1
        }
        indices_list_2[length(indices_list_2)] <- NULL

        k = nr #9
        j = 1
        new_x_list <- list()
        while (j <= nf) { #1--5
          i = nli #7
          while (i <= k) { # 7--9
            aa <- em_data[i,1,j]
            new_x_list <- append(new_x_list,aa)

            i = i + 1
          }
          j = j + 1
        }

        j = 1
        new_y_list <- list()
        while (j <= nf) {
          i = nli
          while (i <= k) {
            bb <- em_data[i,2,j]
            new_y_list <- append(new_y_list,bb)
            i = i + 1
          }
          j = j + 1
        }
        new_x_list <- as.matrix(as.numeric(new_x_list))
        new_y_list <- as.matrix(as.numeric(new_y_list))
        i <- 1
        start <- as.numeric(indices_list_2[i])
        end <- as.numeric(indices_list_1[i])
        new_em <- data.frame(matrix(nrow = (end - start + 1)))
        while (i <= length(indices_list_1)) {
          start <- as.numeric(indices_list_2[i])
          end <- as.numeric(indices_list_1[i])
          temp_em <- cbind(new_x_list[start:end,],new_y_list[start:end,])
          new_em <- cbind(new_em,temp_em)
          i <- i + 1
        }
        new_em <- new_em[,-1]
        #yeni_em<-cbind(new_x_list[1:n1,], new_y_list[1:n1,],new_x_list[n2:n3,],new_y_list[n2:n3,], new_x_list[n4:n5,], new_y_list[n4:n5,], new_x_list[n6:n7,], new_y_list[n6:n7,])
        result_em <- suppressWarnings(amelia(new_em, m = 1,p2s = 0))
        deneme <- result_em$message
        xt_ems <- matrix(byrow = TRUE)
        yt_ems <- matrix(byrow = TRUE)
        xt_em <- result_em$imputations$imp1[1,1]
        yt_em <- result_em$imputations$imp1[1,2]
        ub <- xt_em
        vb <- yt_em
        C = (ub + 0.5)
        yj2_em = {(A^2 + B^2)*yl1 + B*C*D + vb*A*D}/(A^2 + B^2) ####Tahmin edilen y koordinatı

        xj2_em = {A*D*C + xl1*(A^2 + B^2) - B*vb*D}/(A^2 + B^2)
        xj2_em <- as.numeric(unlist(xj2_em))
        yj2_em <- as.numeric(unlist(yj2_em))

        if (is.null(xt_em) || is.null(yt_em)) {
          return(c(result_em[["message"]]))
        }else{
          return(c(nli, null_file_name, xj2_em,yj2_em))}
      }
    }

    how.many.na.file <- function(df_list){
      null_number <- 0
      indice_list <- list()
      i <- 1
      for (elem in df_list) {
        for (is_null in is.na(elem)) {
          if (is_null == TRUE) {
            null_number <- null_number + 1
            indice_list <- append(indice_list,i)
            break
          }
        }
        i <- i + 1
      }
      return(c(null_number,lapply(list(as.numeric(indice_list)),sort,decreasing = TRUE)))
    }

    turn.to.df.list <- function(file_path){
      files <-  list.files(file_path , pattern = '.csv')
      df_list <- vector(mode = 'list',length = length(files))
      i <- 1
      for (file in files) {
        suppressWarnings(temp_data <- as.data.frame(read.csv(paste(file_path,file,sep = "/"),header = TRUE,sep = ",",comment.char = "#")))
        df_list[[i]] <- temp_data
        i <- i + 1
      }

      return(df_list)
    }

    this.file.has.na <- function(data){
      for (is_null in is.na(data)) {
        if (is_null == TRUE) {
          return(TRUE)
        }
      }
      return(FALSE)
    }
    remove_modal_progress()
    result <- impute.multiple.missing(file_path,l1,l2,imp_method = imp_method)
    if (!is.null(result)) {



      for (i in seq_along(result)) {

        dosya_adi <- result[[i]][2]

        if (dosya_adi %in% files) {

          yeni_veri <- read.csv(paste(file_path,"/", dosya_adi, sep = ""), header = TRUE)
          na_index <- result[[i]][1]
          yeni_veri[na_index, "x"] <- result[[i]][3]
          yeni_veri[na_index, "y"] <- result[[i]][4]
          yeni_veri <- rbind(yeni_veri, paste("#","Index number of Missing Landmark: ", result[[i]][1]))
          dir.create(file.path(my_file_path[1], "imputed"), showWarnings = FALSE)
          write.csv(yeni_veri, file = file.path(my_file_path[1], "imputed", paste0("imputed_", imp_method, "_", dosya_adi, "_", single_name, ".csv")), row.names = FALSE)
        }
      }

      shinyalert("Success!", paste("Predicted landmark are saved in the file located at", my_file_path[1], "folder."), type = "success")

    }
    else{
      shinyalert("Warning!", "Please check that there are csvs in the output folder.", type = "warning")
    }
  })

  observeEvent(input$del_last_meas, {
    if (nrow(results_df) == 0) {
      shinyalert("Measurement has not been made before. please use after measuring.", type = "warning")
      return()
    }
    results_df <<- results_df[-nrow(results_df), ]
    shinyalert(title = "Success",
               text = "Last measurement successfully deleted.",
               type = "success",
               closeOnClick = "cancel"
    )
  })



  single_name <- ""

  observeEvent(input$save_name_button, {


    if (input$rater_type == "Single Rater") {
      if (input$name_input == "") {
        shinyalert(title = "Error",
                   text = "Please fill all area",
                   type = "error",
                   closeOnClick = "cancel"
        )
        return()
      }
      if (input$trial_input == "") {
        shinyalert(title = "Error",
                   text = "Please fill all area",
                   type = "error",
                   closeOnClick = "cancel"
        )
        return()
      }
      user_info <- list(name = paste(input$name_input, input$trial_input, sep = ""))
      single_name <<- paste(input$name_input, input$trial_input, sep = "")


      output$response <- renderText(input$name_input)
      saveRDS(user_info, "user_info.rds")
    } else if (input$rater_type == "Multiple Raters") {
      if (input$name_input_mult == "") {
        shinyalert(title = "Error",
                   text = "Please fill all area",
                   type = "error",
                   closeOnClick = "cancel"
        )
        return()
      }
      single_name <<- input$name_input_mult

      user_info <- list(name = input$name_input_mult)

      output$response <- renderText(input$name_input_mult)
      saveRDS(user_info, "user_info.rds")
    }

    shinyalert(title = "Success",
               text = "Your name saved successfully!",
               type = "success",
               closeOnClick = "ok"
    )


  })

  observeEvent(input$save_scale_unit_dist_button, {
    if (is.na(input$knowndistance_input) || input$knowndistance_input == "" || input$knowndistance_units_input == "") {
      shinyalert(title = "Error",
                 text = "Please fill in the distance and distance units fields",
                 type = "error",
                 closeOnClick = "cancel"
      )
      return()
    } else {
      user_info <- list(knowndistance_units = input$knowndistance_units_input, distance = input$knowndistance_input)
      saveRDS(user_info, "user_info.rds")
      shinyalert(title = "Success",
                 text = "Your distance information saved successfully!",
                 type = "success",
                 closeOnClick = "ok"
      )
      read_user_info()
    }
  })

  observeEvent(input$ratio_button, {
    ratio <<- 0
    cal_check <<- TRUE
    shinyalert(title = "Success",
               text = "Ratio resetted!",
               type = "success",
               closeOnClick = "ok")
  })

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

  observeEvent(input$done_submit_button ,{
    if (input$done_radio_button == "Save scaled coordinates as csv"){
      if (is.null(ratio) || ratio == 0) {
        shinyalert("Error!", "measurement length cannot be NULL or 0.", type = "error")
        return()
      }
      x <- xy_new$x[xy_new$x != 0]
      y <- xy_new$y[xy_new$y != 0]

      df <- data.frame(x, y)
      df_new <- df * 1 / ratio


      file = paste0("scale_",file_names_list[index$current],"_",single_name,".csv")


      dir.create(file.path(my_file_path[1], "scaled"), showWarnings = FALSE)
      file_con <- file(file.path(my_file_path[1], "scaled", file), open = "w")


      write.csv(df_new, file = file_con, row.names = FALSE)
      writeLines(c("# Measurement Length ", paste0("#", ratio)), file_con)
      scale_facc <- 1/ratio
      writeLines(c("# Scale factor (Referance Length / Measurement Length) ", paste0("#", scale_facc)), file_con)
      writeLines(paste0("# Referance Length: ", input$knowndistance_input), file_con)
      close(file_con)
      shinyalert("Success!", paste("Csv files have been saved to", my_file_path[1], "folder."), type = "success")

    }
    else if (input$done_radio_button == "Save unscaled coordinates as csv") {

      x <- xy_new$x[xy_new$x != 0]
      y <- xy_new$y[xy_new$y != 0]
      df <- data.frame(x, y)
      file = paste0(file_names_list[index$current],"_",single_name,".csv")
      if (!dir.exists(file.path(my_file_path[1], "output"))) {
        dir.create(file.path(my_file_path[1], "output"), showWarnings = FALSE)
      }
      file_con <- file(file.path(my_file_path[1], "output", file), open = "w")

      write.csv(df, file = file_con, row.names = FALSE)
      close(file_con)
      shinyalert("Success!", paste("Csv files have been saved to", my_file_path[1], "folder."), type = "success")
    }
  })




  colnames(point) <- c("id","x","y")

  observeEvent(input$undo_Button, {
    if (length(xy_new$x) > 0) {
      xy_new$x <- xy_new$x[0:(length(xy_new$x)-1)]
      xy_new$y <- xy_new$y[0:(length(xy_new$y)-1)]
    }
  })

  observeEvent(input$missing_Button, {
    xy_new$x <- c(xy_new$x, NA)
    xy_new$y <- c(xy_new$y, NA)
    shinyalert("Success!", "Null Landmarks have been added.", type = "success")
  })


  ratio <- 0
  cal_check <- TRUE
  results_df <- data.frame(distance_between_two_landmarks = numeric(), unitsofmetric = character())
  observeEvent(input$scale_cal_Button, {

    if (cal_check) {
      if(length(xy_new$x) >= 2 && ratio == 0) {
        delta_x <- xy_new$x[length(xy_new$x)] - xy_new$x[length(xy_new$x)-1]
        delta_y <- xy_new$y[length(xy_new$y)] - xy_new$y[length(xy_new$y)-1]
        result <- sqrt(delta_x^2 + delta_y^2)
        ratio <<- result / known_distance
        shinyalert("Success!", paste("scaling was successful calibrated. ratio is: ", ratio), type = "success")

        for (i in 1:2) {
          xy_new$x <- xy_new$x[0:(length(xy_new$x)-1)]
          xy_new$y <- xy_new$y[0:(length(xy_new$y)-1)]
        }
        cal_check <<- FALSE
      }
      else {
        shinyalert("Warning!", "Click on at least two landmarks on the plot and then press the 'Calibrate' button.", type = "warning")
      }
    }
    else{
      shinyalert("Warning!", "Calibration has already been done.", type = "warning")
    }
  })
  observeEvent(input$scale_meas_Button, {

    if (ratio != 0 && length(xy_new$x) >= 2) {
      delta_x <- xy_new$x[length(xy_new$x)] - xy_new$x[length(xy_new$x) - 1]
      delta_y <- xy_new$y[length(xy_new$y)] - xy_new$y[length(xy_new$y) - 1]

      result <- sqrt(delta_x^2 + delta_y^2)
      if (unitsofmetric == "cm") {
        distance_between_two_landmarks <- result / ratio
      } else if (unitsofmetric == "mm") {
        result = result * 100
        distance_between_two_landmarks <- result / ratio
      } else if (unitsofmetric == "m") {
        result = result / 100
        distance_between_two_landmarks <- result / ratio
      }
      shinyalert("Success!", paste("Scaling was successful measured distance: ", distance_between_two_landmarks," ",unitsofmetric), type = "success")
      results_df <<- results_df %>% add_row(distance_between_two_landmarks = distance_between_two_landmarks, unitsofmetric = unitsofmetric)

      for (i in 1:2) {
        xy_new$x <- xy_new$x[0:(length(xy_new$x)-1)]
        xy_new$y <- xy_new$y[0:(length(xy_new$y)-1)]
      }
    }
    else {
      shinyalert("Warning!", "Click on at least two landmarks on the plot and then press the 'Measure Distance' button.", type = "warning")
    }
  })

  observeEvent(input$rel_submit_button, {
    number_of_dimension <- input$rel_dimension_input
    number_of_subject <- input$rel_subject_input
    number_of_landmark <- input$rel_landmark_input
    path1 <- input$rel_path1_input
    path2 <- input$rel_path2_input
    if (is.na(number_of_dimension) || number_of_dimension == "") {
      shinyalert("Warning!", "rel_dimension_input cannot be empty.", type = "warning")
      return()
    } else if (is.na(number_of_subject) || number_of_subject == "") {
      shinyalert("Warning!", "rel_subject_input cannot be empty.", type = "warning")
      return()
    } else if (is.na(number_of_landmark) || number_of_landmark == "") {
      shinyalert("Warning!", "rel_landmark_input cannot be empty.", type = "warning")
      return()
    } else if (is.null(path1) || path1 == "") {
      shinyalert("Warning!", "rel_path1_input cannot be empty.", type = "warning")
      return()
    } else if (is.null(path2) || path2 == "") {
      shinyalert("Warning!", "rel_path2_input cannot be empty.", type = "warning")
      return()
    } else if (!file.exists(path1)) {
      shinyalert("Warning!", "Invalid data path: rel_path1_input", type = "error")
      return()
    } else if (!file.exists(path2)) {
      shinyalert("Warning!", "Invalid data path: rel_path2_input", type = "error")
      return()
    } else {
      path1_csv_count <- length(list.files(path1, pattern = ".csv$"))
      path2_csv_count <- length(list.files(path2, pattern = ".csv$"))
      if (path1_csv_count != path2_csv_count) {
        shinyalert("Warning!", "The number of CSV files in rel_path1_input and rel_path2_input must be equal.", type = "warning")
        return()
      }

      reliability_IR <- function(number_of_dimension,number_of_subject,number_of_landmark,path1,path2) {
        files1 <-  list.files(path1,pattern = '.csv')
        files2 <-  list.files(path2,pattern = '.csv')

        if (number_of_dimension < 2) {
          shinyalert("Error!","Number of dimension is too small please provide at least 2 dimension",type = 'error')
          return()
        }
        if (length(files1) != number_of_subject |  length(files2) != number_of_subject) {
          shinyalert("Error!", "The number of subjects you provided does not match the number of subjects actually obtained", type = "error")
          return()
        }
        else if (length(files1) != length(files2)) {
          shinyalert("Error!", "Please provide equal number of subject", type = "error")
          return()
        }

        rater_1_landmarks <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                                      c("x", "y"))

        for (file in files1) {
          suppressWarnings(temp_data <- as.data.frame(read.csv(paste(path1,file,sep = "/"),header = TRUE,sep = ",",comment.char = "#")))

          if (number_of_landmark != nrow(temp_data)) {
            shinyalert("Error!", "A file with no lines equal to the number of lines you provided has been detected.",type = 'error')
            return()
          }else{
            rater_1_landmarks <- rbind(rater_1_landmarks,temp_data)
          }
        }


        #for (dataframe_i in rater_1_landmarks_list) {
        #  rater_1_landmarks <- rbind(rater_1_landmarks, dataframe_i)
        #}

        rater_2_landmarks <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                                      c("x", "y"))


        for (file in files2) {
          suppressWarnings(temp_data <- as.data.frame(read.csv(paste(path2,file,sep = "/"),header = TRUE,sep = ",",comment.char = "#")))
          if (number_of_landmark != nrow(temp_data)) {
            shinyalert("Error!", "A file with no lines equal to the number of lines you provided has been detected.",type = 'error')
            return()
          }else{
            rater_2_landmarks <- rbind(rater_2_landmarks,temp_data)
          }
        }

        #for (dataframe_i in rater_2_landmarks_list) {
        #  rater_2_landmarks <- rbind(rater_2_landmarks, dataframe_i)
        #}

        a = 0; rr = 0; ka = 0; kb = 0; sn1 = 1; tlsr = 0; tlr2 = 0; tr2 = 0; tr1a = 0; tr1b = 0;x = 0
        tl2 = 0; ts2 = 0; tlsb2 = 0; td = 0; tlre = 0; tsre = 0; sn2 = 0; d = 0; trs2 = 0; srkt = 0;i = 0;
        C = choose(number_of_landmark,2); sn2 = (number_of_subject*(C)); s = C*number_of_subject;

        R <- matrix(nrow = 2*number_of_subject*C,ncol = 4)

        P <- matrix(nrow = number_of_subject*C,ncol = 4)
        V <- matrix(nrow = number_of_subject*C,ncol = 4)
        for (a in 1:2) {
          for (b in 1:number_of_subject) {
            for (c in 1:C) {
              d = d + 1; R[d,1] = a; R[d,2] = b; R[d,3] = c;
            }
          }
        }


        if (number_of_dimension < 3) {
          for (p in seq(from = 1, to = number_of_subject*number_of_landmark, by = number_of_landmark)) {
            t = number_of_landmark + p; m = t - number_of_landmark;
            for (j in m:(t - 1)) {
              for (i in m:(t - 1)) {
                if ((j < i) & (i != j)) {
                  acia1 = sqrt(((rater_1_landmarks[j,1] - rater_1_landmarks[i,1])^2) + ((rater_1_landmarks[j,2] - rater_1_landmarks[i,2])^2));
                  R[sn1,4] = acia1; sn2 = sn2 + 1;
                  acib1 = sqrt(((rater_2_landmarks[j,1] - rater_2_landmarks[i,1])^2) + ((rater_2_landmarks[j,2] - rater_2_landmarks[i,2])^2));
                  R[sn2,4] = acib1;
                  tlsr = tlsr + (acia1^2) + (acib1^2);
                  tr1a = tr1a + acia1; tr1b = tr1b + acib1;
                  td = td + acia1 + acib1;
                  sn1 = sn1 + 1;
                }
              }
            }
          }
        }
        if (number_of_dimension > 2) {
          for (p in seq(from = 1, to = number_of_subject*number_of_landmark, by = number_of_landmark)) {
            t = number_of_landmark + p; m = t - number_of_landmark;
            for (j in m:(t - 1)) {
              for (i in m:(t - 1)) {
                if ((j < i) & (i != j)) {
                  acia1 = sqrt(((rater_1_landmarks[j,1] - rater_1_landmarks[i,1])^2) + ((rater_1_landmarks[j,2] - rater_1_landmarks[i,2])^2) + ((rater_1_landmarks[j,3] - rater_1_landmarks[i,3])^2));
                  R[sn1,4] = as.integer(acia1); sn2 = sn2 + 1;
                  acib1 = sqrt(((rater_2_landmarks[j,1] - rater_2_landmarks[i,1])^2) + ((rater_2_landmarks[j,2] - rater_2_landmarks[i,2])^2) + ((rater_2_landmarks[j,3] - rater_2_landmarks[i,3])^2));
                  R[sn2,4] = as.integer(acib1);
                  tlsr = tlsr + (acia1^2) + (acib1^2);
                  tr1a = tr1a + acia1; tr1b = tr1b + acib1;
                  td = td + acia1 + acib1;
                  sn1 = sn1 + 1;
                }
              }
            }
          }
        }

        df = (td^2)/(2*C*number_of_subject)
        tr2 = (tr1a^2) + (tr1b^2); rkt = (tr2/(C*number_of_subject)) - df; rko = ((tr2/(C*number_of_subject)) - df)/1; ms_r = rko;

        for (i in 1:s) {
          for (j in 1:4) {
            P[i,j] = R[i,j];
          }
        }

        v = 0
        xyz = s + 1
        for (i in xyz:(2*s)) {
          v = v + 1;
          for (j in 1:4) {
            V[v,j] = R[i,j];
          }
        }

        for (l in 1:C) {
          tl1 = 0;
          for (i in 1:s) {
            if (l > P[i,3] - 1 & (l < P[i,3] + 1)) {
              tl1 = tl1 + P[i,4];}
            if (l > (V[i,3] - 1) & (l < V[i,3] + 1)) {
              tl1 = tl1 + V[i,4];}
          }
          tl2 = tl2 + (tl1^2);
        }

        lkt = (tl2/(number_of_subject*2)) - df

        for (sb in 1:number_of_subject) {
          ts1 = 0;
          for (i in 1:s) {
            if ((sb > (P[i,2] - 1) & sb < (P[i,2] + 1))) {
              ts1 = ts1 + P[i,4];}
            if ((sb > (V[i,2] - 1) & sb < (V[i,2] + 1))) {
              ts1 = ts1 + V[i,4];}
          }
          ts2 = ts2 + (ts1^2);
        }

        skt = (ts2/(C*2)) - df;

        for (l in 1:C) {
          for (sb in 1:number_of_subject) {
            tls1 = 0;
            for (i in 1:s) {
              if ((l > (P[i,3] - 1)) & (l < (P[i,3] + 1))) {
                if ((sb > (P[i,2] - 1)) & (sb < (P[i,2] + 1))) {
                  tls1 = tls1 + P[i,4];}
              }

              if ((l > (V[i,3] - 1) & l < (V[i,3] + 1))) {
                if ((sb > (V[i,2] - 1) & sb < (V[i,2] + 1))) {
                  tls1 = tls1 + V[i,4];}
              }
            }
            tlsb2 = tlsb2 + (tls1^2);
          }
        }

        lskt = (tlsb2/2) - df - lkt - skt;

        for (l in 1:C) {
          tlrp = 0;
          tlrv = 0;
          for (i in 1:s) {
            if ((l > (P[i,3] - 1) & l < (P[i,3] + 1))) {
              tlrp = tlrp + P[i,4]; tlrv = tlrv + V[i,4];}
          }
          tlre = tlre + (tlrp^2 + tlrv^2);
        }

        lrkt = (tlre/number_of_subject) - df - lkt - rkt;

        for (r in 1:2) {
          for (sb in 1:number_of_subject) {
            trsp = 0; trsv = 0;
            for (i in 1:s) {
              if ((r > (P[i,1] - 1) & r < (P[i,1] + 1))) {
                if ((sb > (P[i,2] - 1) & sb < (P[i,2] + 1))) {
                  trsp = trsp + P[i,4];
                }
              }
              if ((r > (V[i,1] - 1) & r < (V[i,1] + 1))) {
                if ((sb > (V[i,2] - 1) & sb < (V[i,2] + 1))) {
                  trsv = trsv + V[i,4];
                }
              }
            }
            trs2 = trs2 + (trsp^2) + (trsv^2);
          }
        }

        srkt = (trs2/C) - df - rkt - skt;

        tlsrkt = tlsr - df - lkt - rkt - skt - lrkt - lskt - srkt

        ms_lrs = tlsrkt/((number_of_subject - 1)*(C - 1));
        ms_lr = lrkt/(C - 1);
        ms_ls = lskt/((number_of_subject - 1)*(C - 1));
        ms_rs = srkt/(number_of_subject - 1);
        ms_l = lkt/(C - 1);
        ms_s = skt/(number_of_subject - 1);

        var_lrs = ms_lrs;
        var_lr = (ms_lr - ms_lrs)/number_of_subject;
        var_ls = (ms_ls - ms_lrs)/2;
        var_rs = (ms_rs - ms_lrs)/C;
        var_l = (ms_l - ms_lr - ms_ls + ms_lrs)/(2*number_of_subject);
        var_r = (ms_r - ms_lr - ms_rs + ms_lrs)/(C*number_of_subject);
        var_s = (ms_s - ms_ls - ms_rs + ms_lrs)/(2*C);

        if (var_lrs < 0) {
          var_lrs = 0;
        }

        if (var_lr < 0) {
          var_lr = 0;
        }

        if (var_ls < 0) {
          var_ls = 0;
        }

        if (var_rs < 0) {
          var_rs = 0;
        }

        if (var_l < 0) {
          var_l = 0;
        }

        if (var_r < 0) {
          var_r = 0;
        }
        if (var_s < 0) {
          var_s = 0;
        }

        if (!file.exists(file.path(my_file_path[1], "reliability"))) {
          dir.create(file.path(my_file_path[1], "reliability"), showWarnings = FALSE)
        }
        var_rel = (var_lr/2) + (var_ls/number_of_subject) + (var_lrs/(2*number_of_subject));

        G = var_l/(var_l + var_rel);
        R_df <- data.frame(R)
        colnames(R_df) <- c("Rater","Subject","Landmark","Euclidean Distances")

        euclidean_file_name <- paste0("reliability_euclidean_distances_between_the_landmark_pairs_", single_name, ".csv")
        variance_file_name <- paste0("estimated_variance_components_", single_name, ".csv")

        write.csv(R_df, file = file.path(my_file_path[1], "reliability",euclidean_file_name),row.names = FALSE)

        sos <- c(lkt,rkt,skt,lrkt,lskt,srkt,tlsrkt)
        ms <- c(ms_r,ms_l,ms_s,ms_ls,ms_lr,ms_rs,ms_lrs)
        var <- c(var_r,var_l,var_s,var_ls,var_lr,var_rs,var_lrs)
        output_df <- data.frame(sos,ms,var,row.names = c("landmark(l)","rater(r)","subject(s)","lXr","lXs","rXs","lXrXs"))

        colnames(output_df) <- c("sum of square","mean square","variance")
        write.csv(output_df, file = file.path(my_file_path[1], "reliability", variance_file_name),row.names = TRUE)
        write("\n",file = variance_file_name, append = TRUE)
        write("\n",file = variance_file_name, append = TRUE)
        write(paste("#Variance of rel : ", var_rel),file = file.path(my_file_path[1], "reliability", variance_file_name), append = TRUE)
        write(cat("\n"),file = variance_file_name, append = TRUE)
        write(paste( "#G COEFFICIENT : ", G),file = file.path(my_file_path[1], "reliability", variance_file_name),append = TRUE)

        shinyalert("Success!", paste("G COEFFICIENT: ", round(G,4), "\n\nCsv files have been saved to\n", my_file_path[1], "folder."), type = "success")


      }
      reliability_IR(number_of_dimension,number_of_subject,number_of_landmark,path1,path2)

    }

  })


  images_path <- reactiveValues(data = list())
  image_names <- reactiveValues(data = list())

  observeEvent(input$image_file, {
    images_path$data <- input$image_file$datapath
    image_names$data <- lapply(input$image_file$datapath, basename)
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
    results_df <<- data.frame(distance_between_two_landmarks = numeric(), unitsofmetric = character())
    ratio <<- 0
    index$current <<- 1
  })

  index <- reactiveValues(current = 1)

  observeEvent(input$next_Button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
    results_df <<- data.frame(distance_between_two_landmarks = numeric(), unitsofmetric = character())
    if (index$current < length(images_path$data)) {
      index$current <<- index$current + 1
      ratio <<- 0
    }
    else {
      shinyalert("Oops!", "This is the last image.", type = "error")
    }
  })

  observeEvent(input$prev_Button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
    results_df <<- data.frame(distance_between_two_landmarks = numeric(), unitsofmetric = character())
    if (index$current > 1) {
      index$current <<- index$current - 1
      ratio <<- 0
    }
    else {
      shinyalert("Oops!", "This is the first image.", type = "error")
    }
  })

  options(shiny.maxRequestSize = 100*1024^2)

  xy_new <- reactiveValues(x = numeric(0), y = numeric(0), line = numeric(0)) # add new landmarks

  height <- 0
  width <- 0
  output$plot.ui <- renderUI({
    if (length(images_path$data) == 0) {
      return()
    }
    else {
      img_extension <- sub(".*\\.([[:alnum:]]+)$", "\\1", images_path$data[[index$current]])
      if (img_extension == "jpg" || img_extension == "jpeg") {
        img <- readJPEG(images_path$data[[index$current]])
      }
      else if (img_extension == "png") {
        img <- readPNG(images_path$data[[index$current]])
      }
      else if (img_extension == "tif" || img_extension == "tiff") {
        img <- tiff::readTIFF(images_path$data[[index$current]])
      }
      else {
        shinyalert("Oops!", "Invalid file type. Please upload JPEG, JPG, PNG or Tiff image.", type = "error")
        return()
      }
    }


    screen_size <- input$screenSize
    img_ratio <- dim(img)[2] / dim(img)[1]
    plot_height <- screen_size[2]
    plot_width <- screen_size[1]
    if (is.null(plot_height) || is.null(plot_width)) {
      plot_height <- 500
      plot_width <- 500
    }
    if (plot_width / plot_height > img_ratio) {
      plot_width <- plot_height * img_ratio
    } else {
      plot_height <- plot_width / img_ratio
    }


    if (is.na(plot_height) || plot_height <= 0 || is.na(plot_width) || plot_width <= 0) {
      return(NULL)
    }

    tags$div(
      style = "position: absolute; left: 20%;",
      plotOutput(
        "distplot",
        click = "plot_click",
        height = plot_height,
        width = plot_width,
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
        paste0("Hover: ", "X: ", round(coords$x,2), " Y: ", round(coords$y,2))
      })
    }
  })

  observe({
    if (is.null(input$plot_click)) {
      return()
    }
    if (!is.null(input$plot_click)) {
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
    if (length(images_path$data) == 0) {
      return()
    }
    else {
      img_extension <- sub(".*\\.([[:alnum:]]+)$", "\\1", images_path$data[[index$current]])
      if (img_extension == "jpg" || img_extension == "jpeg") {
        img <- readJPEG(images_path$data[[index$current]])
      }
      else if (img_extension == "png") {
        img <- readPNG(images_path$data[[index$current]])
      }
      else if (img_extension == "tif" || img_extension == "tiff") {
        img <- tiff::readTIFF(images_path$data[[index$current]])
      }
      else {
        shinyalert("Oops!", "Invalid file type. Please upload JPEG,JPG or PNG image.", type = "error")
        return()
      }
    }
    points_df <<- data.frame(x = as.numeric(xy_new$x), y = as.numeric(xy_new$y))
    coordinates <- c(as.numeric(xy_new$x),as.numeric(xy_new$y))
    point <- rbind(point, coordinates)

    plot(coord$x, coord$y, xlim = c(0, dim(img)[2]), ylim = c(0, dim(img)[1]), xlab = "X", ylab = "Y", xaxt = "n", yaxt = "n")
    axis(3, at = seq(0, dim(img)[2], by = 50))
    axis(2, at = seq(0, dim(img)[1], by = 50), las = 2)

    rasterImage(img, 0, 0, dim(img)[2], dim(img)[1])
    points(coord$x, coord$y, col = c("red"), cex = 2, pch = 20)

    if (nrow(coord) > 0) {
      for (i in 1:nrow(coord)) {
        text(coord$x[i], coord$y[i], labels = i, pos = 4, col = "red", cex = 2)
      }
    }

  })
  observeEvent(input$save_img_Button, {


    coord <- tibble(x = xy_new$x, y = xy_new$y)

    if (length(images_path$data) == 0) {
      return()
    }
    else {
      img_extension <- sub(".*\\.([[:alnum:]]+)$", "\\1", images_path$data[[index$current]])
      if (img_extension == "jpg" || img_extension == "jpeg") {
        img <- readJPEG(images_path$data[[index$current]])
      }
      else if (img_extension == "png") {
        img <- readPNG(images_path$data[[index$current]])
      }
      else if (img_extension == "tif" || img_extension == "tiff") {
        img <- tiff::readTIFF(images_path$data[[index$current]])
      }
      else {
        shinyalert("Oops!", "Invalid file type. Please upload JPEG,JPG or PNG image.", type = "error")
        return()
      }
    }

    points_df <<- data.frame(x = as.numeric(xy_new$x), y = as.numeric(xy_new$y))
    coordinates <- c(as.numeric(xy_new$x),as.numeric(xy_new$y))
    point <- rbind(point, coordinates)
    par(mar=c(1,1,1,1))
    plot(coord$x, coord$y, xlim = c(0, dim(img)[2]), ylim = c(0, dim(img)[1]), xlab = "X", ylab = "Y", xaxt = "n", yaxt = "n")

    axis(3, at = seq(0, dim(img)[2], by = 50))
    axis(2, at = seq(0, dim(img)[1], by = 50), las = 2)

    rasterImage(img, 0, 0, dim(img)[2], dim(img)[1])
    points(coord$x, coord$y, col = c("red"), cex = 2, pch = 20)
    if (is.null(xy_new$x)) {
      shinyalert("Oops!", "No click has been made yet.", type = "error")
      return()
    }


    if (nrow(coord) > 0) {
      for (i in 1:nrow(coord)) {
        text(coord$x[i], coord$y[i], labels = i, pos = 4, col = "red", cex = 2)
      }
      file_name <- file_names_list[index$current]

      if (!file.exists("output")) {
        dir.create("output", showWarnings = FALSE)
      }

      dir.create(file.path(my_file_path[1], "markedImage"), showWarnings = FALSE)
      output_file <- file.path(my_file_path[1], "markedImage", paste0("marked_",substring(file_name, 1, regexpr("\\.", file_name) - 1),"_", single_name, ".png"))
      dev.copy(png, output_file, width = dim(img)[2], height = dim(img)[1])
      dev.off()
      shinyalert("Success!", paste("Image Output have been saved to", my_file_path[1], "folder."), type = "success")

    }

  })

  observeEvent(input$clear_button, {
    xy_new$x <- numeric(0)
    xy_new$y <- numeric(0)
  })

  output$info <- renderText({
    xy_str <- function(e) {
      if (is.null(e)) {
        return("NULL\n")
      }
      paste0("x=", round(e$x, 2), " y=", round(e$y, 2), "\n")
    }

    xy_range_str <- function(e) {
      if (is.null(e)) {
        return("NULL\n")
      }
      paste0("xmin="   , round(e$xmin, 2),           " xmax=", round(e$xmax, 2),
             " ymin="  , round(e$ymin, 2),           " ymax=", round(e$ymax, 2),
             " xrange=", round(e$xmax - e$xmin, 2),    " yrange=", round(e$ymax - e$ymin, 2),
             " diag="  , round(sqrt((e$xmax - e$xmin) ^ 2 + (e$ymax - e$ymin) ^ 2)))
    }

    paste0(
      "click: ", round(xy_new$x[length(xy_new$x)],2)," ",round(xy_new$y[length(xy_new$y)],2)
    )
  })
}
