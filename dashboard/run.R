library(shiny)

port <- Sys.getenv("PORT")

print(getwd())
print(list.files())

shiny::runApp(
  appDir = ".",
  host = '0.0.0.0',
  port = as.numeric(port)
)
