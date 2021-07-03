library(shiny)
library(lubridate)

#load("../data/full_10_month_analysis_HOBO_result_summary_only_dashboard.Rda")
#load("../data/full_10_month_analysis_Insentec_result_summary_only_dashboard.Rda")
#load("../data/full_10_month_analysis_result_summary_only_dashboard.Rda")

shinyUI(fluidPage(

  title = "Dairy Cow Dashboard",
  
  fluidRow(
    column(6,
      dateRangeInput(
        "date_range",
        "Dates",
        start = today() - years(1),
        end = NULL,
        min = NULL,
        max = NULL
      )
    ),
    column(6,
      selectInput(
        "cow_selection",
        "Cows",
        unique(dashboard_full_analysis[["Insentec"]][["Feeding and drinking analysis"]][["Cow"]]),
        multiple = TRUE,
        selectize = TRUE
      )
    )
  ),
  fluidRow(
    column(6,
      plotOutput("feed_range")
    ),
    column(6,
      plotOutput("water_range")
    )
  )
  
))