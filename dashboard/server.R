library(shiny)
library(tidyverse)

shinyServer(function(input, output) {
  
  # filter by user input date range
  filter_date_range <- function(df, col){
    df %>%
      filter({{col}} >= input$date_range[[1]]) %>%
      filter({{col}} <= input$date_range[[2]])
  }

  # filter by user input cow slection
  filter_cows <- function(df, col){
    df %>%
      filter({{col}} %in% input$cow_selection)
  }
  
  range_data <- function(df){
    
    # convert Cow col to character to add summary stats later and filter by date
    df <- df %>%
      mutate(Cow = as.character(Cow)) %>%
      filter_date_range(date)
    
    # calculate summary stats
    average_rows <- df %>%
      group_by(date) %>%
      summarise(.,
                across(where(is.character), ~"Average"),
                across(where(is.numeric), mean))
    
    # filter by cow and add summary rows
    df %>%
      filter_cows(Cow) %>%
      bind_rows(average_rows) %>%
      group_by(Cow)
  }

  range_plot <- function(df, ycol, var_name){

    # filter table
    df <- range_data(df)
    
    # generate table
    output[[paste0(var_name, "_table")]] <- DT::renderDataTable(
      {df},
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        dom = 'Bftip',
        buttons = c("csv")
      )
    )
    
    # generate plot
    output[[paste0(var_name, "_range")]] <- renderPlot({
      df %>%
        ggplot(aes(x = date, y = {{ycol}}, colour = Cow)) + 
        geom_line()
    })
  }
  
  # add plots and tables to the UI
  feed_drink_df <- dashboard_full_analysis[["Insentec"]][["Feeding and drinking analysis"]]
  standing_bout_df <- dashboard_full_analysis[["HOBO"]][["lying_standing_summary_by_date"]]

  observe({
    range_plot(
      feed_drink_df,
      `Feeding_Duration(s)`, 
      "feed"
    )
  
    range_plot(
      feed_drink_df,
      `Drinking_Duration(s)`,
      "drink"
    ) 
  
    range_plot(
      standing_bout_df,
      `standing_time(seconds)`,
      "standing_time"
    ) 
    range_plot(
      standing_bout_df,
      `standing_bout`,
      "standing_bout"
    ) 
  })
})