devtools::load_all()
library(shiny)
library(htmltools)

shinyApp(
  ui = fluidPage(
    title = "elo",
    pickerInputNew(inputId = "a", label = "toggle", choices = c("a", "b", "c"), selected = "a", multiple = TRUE),
    verbatimTextOutput("out")
  ),
  server = function(input, output, server) {
    output$out <- renderPrint(input$a)
  }
)
