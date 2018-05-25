library(shiny)
library(stringr)
library(tibble)
library(DT)

shinyServer(function(input, output, session) {
  
  # Container to pass result to download handler.
  
  reactive_output <- reactiveValues(df = numeric())
  
  # Function to convert the files -----
  
  schrapkaart2Testvision <- function(teleform_data, exam_date, test_id, omrekentabel, pos_open_questions) {
   
   # Extract student number from input.
   
   stud_nbr <- teleform_data[ , 1]
   
   # Switch to NetID for students that still have one.
   
   if(any(stud_nbr %in% omrekentabel$std_nr)) {
     stud_nbr[stud_nbr %in% omrekentabel$std_nr] <- omrekentabel$net_id[omrekentabel$std_nr %in% stud_nbr]
   }
   
   # Test ID. 
   
   test_id <- rep(as.character(test_id), length(stud_nbr))
   
   # Production Code.
   
   toets_versie <- teleform_data[ , 2 ]
   productieCode <- paste0("0000",as.character(toets_versie))
   
   # Test Datum. Format is YYYYMMDDHHmm! 
   
   exam_date <- exam_date
   
   exam_date <- as.character(rep(as.Date(exam_date,format = "%Y%m%d%H%M"),length(stud_nbr)))
   
   exam_date <- str_replace_all(exam_date,pattern = "-",replacement = "")
   
   # Extracting the answers. 
   
   answers <- teleform_data[, 3:dim(teleform_data)[2]]
   
   names(answers) <- paste0("MC",1:ncol(answers))
  
   ## Cut off all columns that are completely NA.
   
   is_not_NA <- apply(answers,2,function(x) !(sum(is.na(x)) == length(x)))
   
   answers <- answers[, is_not_NA]
   
   # Replace NA with spaces 
   answers[is.na(answers) | answers == "NA"] <- " "
  
   # Original number of MC answers
   no_of_mc_answers <- ncol(answers)
   
  # Add dummy text for open questions
   
   if(pos_open_questions != ""){
     # split the comma delimited values, clean up spaces etc.
    positions <- str_split(pos_open_questions,",",simplify = TRUE)
    
    no_open_questions <- length(positions)
    
    # Add columns at the desired positions
      for(i in 1:no_open_questions) {
        if(as.numeric(positions[i]) <= no_of_mc_answers){
          answers <- answers %>% add_column(!!paste0("Open",i) := "Zie papieren afname.",.before = paste0("MC", positions[i]))
        }
        if(as.numeric(positions[i]) > no_of_mc_answers){
          answers <- answers %>% add_column(!!paste0("Open",i) := "Zie papieren afname.",.after = paste0("MC",  no_of_mc_answers))
        }
      }
    testvision_data <-
      data.frame(stud_nbr, test_id, productieCode, exam_date , answers)
   } else{
     testvision_data <-
       data.frame(stud_nbr, test_id, productieCode, exam_date , answers)
   }
  
  
   return(testvision_data)
   
 }
  
 
  # Validation function for the open questions input bar 
  open_question_checker <- function(open_question_positions) {
    if (open_question_positions == "") {
      return(TRUE)
    }
    if (grepl("[^0-9,]", open_question_positions)) {
      return(FALSE)
    }
    if (grepl("[0-9]", open_question_positions)) {
      if (all(str_split(open_question_positions, ",", simplify = TRUE) != "")) {
        return(TRUE)
      }
    } else
      return(FALSE)
  }
  
  # Output preview table ----

  
  output$contents <- renderDT({
 
    # Store the uploaded file object
    inFile <- input$teleform_data
    
    # Return null if no input
    if (is.null(inFile))
      return(NULL)
    
    # Read in the uploaded file.
    
    data <- read.csv(inFile$datapath, header=FALSE,na.strings = 9, colClasses = "character", as.is = TRUE)
    
    # Read in transformation table for stud nr to net id.
    
    omrekentabel <- readRDS("omrekentabel.RDS")
    
    # Validate the open questions input
    validate(
      need(open_question_checker(input$text_open_questions_insertion), "Please enter the positions of the open questions in a valid format"))
    # Transform the input.
    
    result <- schrapkaart2Testvision(data,input$exam_date,test_id = input$test_id, omrekentabel = omrekentabel, pos_open_questions = input$text_open_questions_insertion)
    
    # Store result in reactive container to pass to download handler
    reactive_output$df <<- result
    # Display preview
    datatable(result,
              class = "compact",
              options = list(dom = 'lfrtip', scrollY = 400, scrollX = 600, lengthMenu = list(c(100,-1), c(100,"All"))), editable = TRUE) %>% 
      formatStyle(grep("MC",names(result)),
                  backgroundColor = styleEqual(c((input$num_answ_alternatives + 1):50),
                                               rep('red', 50-input$num_answ_alternatives)))
  })
  
  table = dataTableProxy('contents')
  
  observeEvent(input$contents_cell_edit, {
    info = input$contents_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    reactive_output$df[i, j] <<- as.character(v)
    replaceData(table, reactive_output$df, resetPaging = FALSE)# important
  })
  
  # Download handler -----
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste0(input$teleform_data,"_output.txt") 
    },
    content = function(file) {
      write.table(reactive_output$df, file, row.names = FALSE, col.names = FALSE, sep = ",", na = " ", quote = TRUE)
    }
  )
  
})
