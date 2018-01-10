library(shiny)
library(stringr)

shinyServer(function(input, output) {
  
  # Container to pass result to download handler.
  
  reactive_output <- reactiveValues(df = numeric())
  
  # Function to convert the files -----
  
  schrapkaart2Testvision <- function(teleform_data, exam_date, test_id, omrekentabel, open_questions) {
   
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
  
   ## Cut off all columns that are completely NA.
   
   is_not_NA <- apply(answers,2,function(x) !(sum(is.na(x)) == length(x)))
   
   answers <- answers[, is_not_NA]
   
   # Append dummy text for open questions if necessary.
   
   if(open_questions > 0){
     open_answer_dummies <- matrix("Zie papieren afname.", nrow = dim(answers)[1],ncol = open_questions)
     
     testvision_data <-
       data.frame(stud_nbr, test_id, productieCode, exam_date , answers,open_answer_dummies)
   } else{
     testvision_data <-
       data.frame(stud_nbr, test_id, productieCode, exam_date , answers)
   }
  
   return(testvision_data)
   
 }
  

  observeEvent(input$check_wrong_answers,{
    
    # If there is no data, show a dialogue stating this.
    
    if(is.null(input$teleform_data)){
      showModal(modalDialog("Please load a data file.", title = "Easy there.", footer = modalButton("Dismiss"),
                  size = c("s"), easyClose = FALSE, fade = TRUE))
    } else{
      # Load the data
      data <- reactive_output$df
        
      # Extract the answers minus open questions (if any)
      answers <- 
        data[, 5:(dim(data)[2] - input$num_open_questions)]
      
      # Check where the wrong answers are
      
      rows_with_impossible_data <-
        which(answers > input$num_answ_alternatives, arr.ind = TRUE)[, 1]
      
      # If there are wrong answers, show a dialogue with the student nunmbers
      
      if (length(rows_with_impossible_data) > 0) {
        # Extract student numbers
        students_with_bad_data <- data[rows_with_impossible_data, 1]
        
        # Show modal dialog
        showModal(
          modalDialog(
            verbatimTextOutput("modal_dialogue") ,
            title = "Uh Oh",
            footer = modalButton("Dismiss"),
            size = c("m"),
            easyClose = FALSE,
            fade = TRUE
          )
        )
        # Paste all the student numbers and show dialogue.
        output$modal_dialogue <- renderText({
          c(
            "The following students seem to have impossible MC answers:\n",
            paste(
              students_with_bad_data,
              sep = "\n",
              collapse = "\n"
            ),
            "\n Please fix the impossible values in the input file and try again."
          )
        })
      } else{
        showModal(
          modalDialog(
            "No problems found.",
            title = "Easy there.",
            footer = modalButton("Dismiss"),
            size = c("s"),
            easyClose = FALSE,
            fade = TRUE
          )
        )
      }
    }
     
      }
     
  )
  
  output$contents <- renderTable(colnames = FALSE,{
    
    # Store the uploaded file object
    inFile <- input$teleform_data
    
    # Return null if no input
    if (is.null(inFile))
      return(NULL)
    
    # Read in the uploaded file.
    
    data <- read.csv(inFile$datapath, header=FALSE,na.strings = 9, colClasses = "character", as.is = TRUE)
    
    # Read in transformation table for stud nr to net id.
    
    omrekentabel <- readRDS("omrekentabel.RDS")
    
    # Transform the input.
    
    result <- schrapkaart2Testvision(data,input$exam_date,test_id = input$test_id, omrekentabel = omrekentabel, open_questions = input$num_open_questions)
    
    # Store result in reactive container to pass to download handler
    reactive_output$df <<- result
    # Display preview
    result
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
