# Shiny app entry point for PopMosaic

library(shiny)

ui <- fluidPage(
  h2("PopMosaic")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
