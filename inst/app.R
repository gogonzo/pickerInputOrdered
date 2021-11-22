library(shiny)
library(pickerInputOrdered)

shinyApp(
  ui = fluidPage(
    title = "elo",
    fluidRow(
      column(
        6,
        div(
          pickerInputNew(inputId = "a", label = "toggle", choices = c("a", "b", "c"), selected = "a", multiple = TRUE),
          br(),
          br(),
          br(),
          br(),
          verbatimTextOutput("outa")
        )
      )
    )
  ),
  server = function(input, output, server) {
    output$outa <- renderPrint(input$a)
  }
)
