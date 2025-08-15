# Shiny app entry point for PopCanvas

library(shiny)

ui <- fluidPage(
  h2("PopCanvas")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
