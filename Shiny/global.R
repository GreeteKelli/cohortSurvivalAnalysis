#
# Entrypoint of the Shiny app
#
# This application is created as a part of a student project.

source("helpers.R")

runGUI <- function(){
  # Load the UI and server code from separate files
  ui <- source("ui.R", local = TRUE)$value
  server <- source("server.R", local = TRUE)$value

  # Launch the app
  shiny::shinyApp(ui = ui, server = server)
}
