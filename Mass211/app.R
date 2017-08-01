source("Mass211/init.R")
source("Mass211/ui.R")
source("Mass211/server.R")

# Run the application 
shinyApp(ui = ui, server = server)