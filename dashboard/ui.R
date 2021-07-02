library(shiny)
library(lubridate)

shinyUI(pageWithSidebar(

  headerPanel("Dairy Cow Dashboard"),
  
  sidebarPanel(
    id="sidebar_panel",

    dateRangeInput(
      "date_range",
      "Dates",
      start = today() - years(1),
      end = NULL,
      min = NULL,
      max = NULL
    ),
    
    selectInput(
      "cow_selection",
      "Cows",
      unique(dashboard_full_analysis[["Insentec"]][["Feeding and drinking analysis"]][["Cow"]]),
      multiple = TRUE,
      selectize = TRUE
    )
  ),
  
  mainPanel(
    id="main_panel",
    dataTableOutput("table")
  )
))