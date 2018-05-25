library(shiny)
library(shinydashboard)
library(DT)
library(shinyBS)


dashboardPage(
  dashboardHeader(title = "Schrapkaarten2Testvision App",titleWidth = "100%"),

  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "table_styling.css")
    ),
    fluidRow(align = "center",
             div(
               fileInput("teleform_data", "1. Upload teleform File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv",
                                    ".txt"))),
               dateInput("exam_date", "2. Enter Exam date", Sys.Date(), "2015-01-01",
                         "3000-01-01", "dd-mm-yyyy", "month", 1),
               numericInput("test_id", list("3. Enter Test ID", tags$i(id = "test_id_circle_icon", class = "fa fa-question-circle")),0,
                            0, 99999, 1),
             bsTooltip(id = "test_id_circle_icon", title = "This number can be found in the footer of each exam printout."),
             textInput("text_open_questions_insertion", list("4. Add open questions", tags$i(id = "open_question_circle_icon",class = "fa fa-question-circle")),value = ""),
             bsTooltip(id = "open_question_circle_icon", 
                       title = "Please enter the column positions delimited by a comma (e.g.: 1,2,3). To add columns at the end of the exam, enter numbers bigger than the max number of mc questions"),
             numericInput("num_answ_alternatives", list("5. Enter the number of MC answer alternatives and check for faulty answers",
                                                        tags$i(id = "max_mc_circle_icon",class = "fa fa-question-circle")), 3,2, 10, 1)),
    bsTooltip(id = "max_mc_circle_icon", title = "You can scroll through the preview. Answers that are above the entered number will be displayed in red. You can correct them directly in the preview table before downloading the output file. "),
             div(downloadButton("downloadData", "Download Testvision file"),align = "center"),
    fluidRow(align = "center",
             h3("Preview of output table:"),
                 wellPanel(div(style = list('overflow-x: scroll'),DTOutput('contents')))
           
             )
  )
)