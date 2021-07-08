
shinyServer(function(input, output, session) {
  
  warning_table <- insentec[["Insentec warning"]]
  output$warning_table <- format_dt_table(warning_table)
  
  output$notifications <- renderMenu({
    get_warnings(warning_table)
  })
  
  
  #' Generate the plot and data tabs for time range plots
  #'
  #' @param df The dataframe containing data to be displayed
  #' @param y_col The column of interest
  #' @param var_name The name of the UI output variable
  #'
  #' @return NULL
  plot_cow_date_range <- function(df, y_col, var_name){

    # filter table
    df <- process_range_data(df, input$agg_type, input$cow_selection, input$date_range)

    # generate table
    output[[paste0(var_name, "_table")]] <- format_dt_table(df)

    # generate plot
    output[[paste0(var_name, "_plot")]] <- renderPlotly({
      cow_date_range_plot(df, {{y_col}}, input$show_average)
    })
  }

  # add plots and tables to the UI
  standing_bout_df <- hobo[["lying_standing_summary_by_date"]]
  feed_drink_df <- insentec[["Feeding and drinking analysis"]]
  non_nutritive_df <- convert_date_col(insentec[["non_nutritive_visits"]])
  feeding_together_df <- convert_date_col(insentec[["average number of feeding buddies"]])

  # update cow selection
  observe({
    cow_choices <- filter_date_range(feed_drink_df, date, input$date_range) %>%
      select("Cow") %>%
      unique()
    colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " cows with data in date range")

    updatePickerInput(
      session = session,
      inputId = "cow_selection",
      choices = cow_choices
    )
  })

  # render network
  observe({
    raw_graph_data <- hobo[["paired lying total time"]]
    combo_df <- combine_data(raw_graph_data, input$relationship_date_range[[1]], input$relationship_date_range[[2]])
    g <- .make_tidygraph(raw_graph_data, combo_df)
    output$network_plot <- visNetwork::renderVisNetwork({
      plot_network(g)
    })
    output$network_table <- format_dt_table(combo_df)
  })

  # render activity plots
  observe({
    plot_cow_date_range(
      feed_drink_df,
      `Feeding_Duration(s)`,
      "feed"
    )
    
    plot_cow_date_range(
      feed_drink_df,
      `Drinking_Duration(s)`,
      "drink"
    ) 
  
    plot_cow_date_range(
      standing_bout_df,
      `standing_time(seconds)`,
      "standing_time"
    ) 
    
    plot_cow_date_range(
      standing_bout_df,
      `standing_bout`,
      "standing_bout"
    )
    
    plot_cow_date_range(
      non_nutritive_df,
      `number_of_non_nutritive_visits`,
      "non_nutritive"
    )
    
    plot_cow_date_range(
      feeding_together_df,
      `average_num_other_cows_feeding_together`,
      "feeding_together"
    )

  })
})
