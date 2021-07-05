library(shiny)
library(tidyverse)
library(plotly)
library(shinyWidgets)

shinyServer(function(input, output, session) {
  
  # filter by user input date range
  filter_date_range <- function(df, col){
    if(input$time_type == 'date_range'){
      df %>%
        filter({{col}} >= input$date_range[[1]]) %>%
        filter({{col}} <= input$date_range[[2]])
    }
    else{
      df %>%
        filter({{col}} == input$day)
    }
  }

  # filter by user input cow selection
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
    orig_df <- df
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
    output[[paste0(var_name, "_plot")]] <- renderPlotly({
      plt <- df %>%
        ggplot(aes(x = date, y = {{ycol}}, colour = Cow)) +
        geom_line() 
      plt %>%
        ggplotly()
    })
  }
  
  # add plots and tables to the UI
  hobo <- dashboard_full_analysis[["HOBO"]]
  insentec <- dashboard_full_analysis[["Insentec"]]
  
  standing_bout_df <- hobo[["lying_standing_summary_by_date"]]
  feed_drink_df <- insentec[["Feeding and drinking analysis"]]
  non_nutritive_df <- enframe(insentec[["non_nutritive_visits"]], name = "date") %>%
    mutate(date = as.Date(date)) %>%
    unnest(value)

  # update cow selection
  observe({
    cow_choices <- filter_date_range(feed_drink_df, date) %>%
      select("Cow") %>%
      unique()
    colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " cows with data in date range")

    updatePickerInput(
      session = session,
      inputId = "cow_selection",
      choices = cow_choices
    )
  })

  # render plots
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
    
    range_plot(
      non_nutritive_df,
      `number_of_non_nutritive_visits`,
      "non_nutritive"
    )
  })
})