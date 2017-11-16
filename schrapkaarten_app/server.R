library(shiny)
library(stringr)

shinyServer(function(input, output) {
  
  # Container to pass result to download handler.
  
  reactive_output <- reactiveValues(df = numeric())
  
  # Function to convert the files -----
  
  schrapkaart2Testvision <- function(teleform_data, exam_date, test_id, omrekentabel) {
   
   # Extract student number from input.
   
   stud_nbr <- teleform_data[ , 1]
   
   # Switch to NetID for students that still have one.
   
   if(any(stud_nbr %in% omrekentabel$std_nr)) {
     stud_nbr[stud_nbr %in% omrekentabel$std_nr] <- omrekentabel$net_id[omrekentabel$std_nr %in% stud_nbr]
   }
   
   # Test ID. 
   
   test_id <- rep(test_id, length(stud_nbr))
   
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
   
   
   # Put everything into a data frame.
   
   testvision_data <-
     data.frame(stud_nbr, test_id, productieCode, exam_date , answers)
   
   return(testvision_data)
 }
  
   
  
  
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
    
    result <- schrapkaart2Testvision(data,input$exam_date,test_id = input$test_id, omrekentabel = omrekentabel)
    
    # Store result in reactive container to pass to download handler
    reactive_output$df <<- result
    # Display preview
    result
  })
  
  # Download handler -----
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$teleform_data,"_output.txt", sep='') 
    },
    content = function(file) {
      write.table(reactive_output$df, file, row.names = FALSE, col.names = FALSE, sep = ",", na = "NA",quote = TRUE)
    }
  )
  
})
