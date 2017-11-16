
library(shiny)
library(shinydashboard)


dashboardPage(
  dashboardHeader(title = "Schrapkaarten2Testvision App",titleWidth = "100%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(align = "center",
             div(
               fileInput("teleform_data", "Upload teleform File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv",
                                    ".txt"))),
               dateInput("exam_date", "Exam date", Sys.Date(), "2015-01-01",
                         "3000-01-01", "dd-mm-yyyy", "month", 1),
               numericInput("test_id", "Test ID",0,
                            0, 99999, 1),
             downloadButton("downloadData", "Download Testvision file")
             ),
    fluidRow(align = "center",
             h3("Preview of output table:"),
              wellPanel(
                tableOutput('contents')
              )
             )
           
    
      
      
  )
)