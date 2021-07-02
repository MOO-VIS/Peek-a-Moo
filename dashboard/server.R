library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
  open_data <- reactive({
    load("../data/full_10_month_analysis_HOBO_result_summary_only_dashboard.Rda")
    load("../data/full_10_month_analysis_Insentec_result_summary_only_dashboard.Rda")
    load("../data/full_10_month_analysis_result_summary_only_dashboard.Rda")
  })
  
  filter_date_range <- function(df, col){
    df %>%
      filter({{col}} >= input$date_range[[1]]) %>%
      filter({{col}} <= input$date_range[[2]])
  }
  
  filter_cows <- function(df, col){
    df %>%
      filter({{col}} %in% input$cow_selection)
  }

  get_file <- reactive({
    df <- dashboard_full_analysis[["Insentec"]][["Feeding and drinking analysis"]] %>%
      mutate(Cow = as.character(Cow))
    average_rows <- df %>%
      group_by(date) %>%
      summarise(.,
                across(where(is.character), ~"Average"),
                across(where(is.numeric), mean))

    return(
       df %>%
        filter_date_range(date) %>%
        filter_cows(Cow) %>%
        bind_rows(average_rows)
    )
  })

  output$table <- renderDataTable({
    get_file() 
  })
})