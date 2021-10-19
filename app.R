#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(matlab)
library(DT)
source("gcdcalc.R")
#source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Modular App",
                          titleWidth = 500)
sidebar <- dashboardSidebar(
  width = 200,
  numericInput("inputNum", "Enter non-Prime Number:", 4, min = 1, max = 53),
  actionButton("btnTable", "Display Table")
)
body <- dashboardBody(
  fluidRow(stylesheet,
    box(
    column(width=12,
      h3("Multiplication Table"),
      # dataTableOutput("tblmult")
      tableOutput("multable")
    )
    ),
    column(width = 12,
           # plotOutput("barPlot"),
           tableOutput("tempPlot")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "red") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images, audio, or stylesheet must go into a subfolder named www

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  #Variables that are shared among server functions (use <<-)
  # num <- 1
  
  #Initialization
  
  #Functions that respond to events in the input
  observeEvent(input$btnTable,{
    n <<- input$inputNum
    v <<- gcd.multiplicativeGroup(n)
    if(as.logical(isprime(n))) {
      showModal(modalDialog(title = "Please Enter a non-Prime Number",
                            id = "inv",
                            h3("Please Enter a non-Prime Number")))
    } else {
      tbl <- outer(v,v,"*")%%n
      colnames(tbl) <- v
      rownames(tbl) <- v
      # counts <- table(tbl)
      counts <- gcd.orders(v, n)
      cat(counts)
      # output$tblmult <- renderDataTable(tbl, options = list(dom = "t"))
      output$multable <- renderTable(tbl, rownames = TRUE, digits = 0)
      
      output$tempPlot <- renderTable(counts, rownames = TRUE, digits = 0)
      
      # output$barPlot <- renderPlot(barplot(counts, main = "Order of Elements"))
    }
  })
}

#Run the app
shinyApp(ui = ui, server = server)