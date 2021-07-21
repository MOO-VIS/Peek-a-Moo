# Set up shiny server
shinyServer(function(input, output, session) {
  
  # Warning section
  observe({
    warning_df <- combine_warnings(
      insentec, 
      food_cuttoff = input$food_intake, 
      water_cuttoff = input$water_intake,
      bin_cuttoff = input$bin_volume
    )
    
    output$warning_table <- format_dt_table(warning_df, page_length = 20)
    
    output$warning_plot <- DT::renderDataTable({
      warning_df %>%
        filter(date == max(date)) %>%
        t() %>%
        as.data.frame() %>%
        filter(V1 != "")
    },
    colnames = c("Warning Type", "Warning"),
    options = list(
      pageLength = 50,
      dom = "ft"
      )
    )
    
    # Warning notifications menu
    output$notifications <- renderMenu({
      get_warning_dropdown(warning_df)
    })
    observeEvent(input$linkClicked,{
      updateTabItems(session,"sidemenu",selected = "warnings")
      output$dropdown=renderMenu({get_warning_dropdown(warning_df)})
    })
  })


  # update cow selections based on selected dates
  observe({
    update_cow_selection(input$activity_date_range, "activity_cow_selection", session)
  })
  observe({
    update_cow_selection(input$daily_date, "daily_cow_selection", session, select_all = TRUE)
  })
  observe({
    update_cow_selection(input$relationship_date_range, "relationship_cow_selection", session)
  })

  # render network
  observe({
    req(input$relationship_date_range)
    
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
    
    #' Generate the plot and data tabs for time range plots
    #'
    #' @param df The dataframe containing data to be displayed
    #' @param y_col The column of interest
    #' @param var_name The name of the UI output variable
    plot_cow_date_range <- function(df, y_col, var_name){
      
      # filter table
      df <- process_range_data(df, input$activity_agg_type, input$activity_cow_selection, input$activity_date_range)
      
      # generate table
      output[[paste0(var_name, "_table")]] <- format_dt_table(df)
      
      # generate plot
      output[[paste0(var_name, "_plot")]] <- renderPlotly({
        cow_date_range_plot(df, {{y_col}}, input$show_average)
      })
    }
    
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
  
  observe({
    req(input$daily_date)
    req(input$daily_cow_selection)
    
    # Create feeding, drinking, and lying_standing dataframes
    feeding <- insentec[["Cleaned_feeding_original_data"]]
    drinking <- insentec[["Cleaned_drinking_original_data"]]
    lying_standing <- hobo[["duration_for_each_bout"]]
    
    # Render daily behavior plot
    df <- daily_schedu_moo_data(feeding, drinking, lying_standing, cow_id = input$daily_cow_selection, date = input$daily_date)
    output$daily_table <- format_dt_table(drop_na(df,Cow))
    output$daily_plot <- renderPlotly(daily_schedu_moo_plot(df))
  })
  
  observe({
    req(input$relationship_cow_selection)
    req(input$relationship_date_range)
    
    df <- actor_reactor_analysis(make_analysis_df(insentec[["Replacement behaviour by date"]]))
    output$bullying_table <- format_dt_table(df)
    output$bullying_plot <- renderPlotly({
      plot_bully_analysis(df, input$relationship_cow_selection, input$relationship_date_range[[1]], input$relationship_date_range[[2]])
    })
  })


  # Feed Bin selection
  observe({
    update_bin_selection(input$bin_date, "activity_bin_selection", session)
  })
  
  
  # feed bin tab
  observe({
    req(input$bin_date)
    req(input$activity_bin_selection)
    
    bin_df <- select_feed_bin_data(feed_df, 
                                   feed_date = input$bin_date, 
                                   bin_selection = input$activity_bin_selection)
    # plot
    output$feed_bin_plot <- renderPlot({
      plot_feed_bin_data(
        hourly_df = bin_df,
        hr = input$obs_hr,
        max_wt = input$bin_weight
      )
    })
    # CSV output
    output$feed_bin_table <- format_dt_table(bin_df)
  })
  
  observe({
    req(input$bin_date)
    req(input$activity_bin_selection)
    
    df <- filter_dates(insentec[["bin_empty_total_time_summary"]], date, input$bin_date) %>%
      parse_hunger_df(input$activity_bin_selection)
    
    output$hunger_table <- format_dt_table(df)
    output$hunger_plot <- renderPlotly({
      hunger_plot(df)
    })
    
  })
})
