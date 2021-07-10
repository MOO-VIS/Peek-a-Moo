# Set up shiny server
shinyServer(function(input, output, session) {
  
  # Warning section
  observe({
    warning_df <- combine_warnings(
      insentec, 
      food_cuttoff = input$food_intake, 
      water_cuttoff = input$water_intake
    )
    
    output$warning_table <- format_dt_table(warning_df, page_length = 20)
    
    # Warning notifications menu
    output$notifications <- renderMenu({
      get_warning_dropdown(warning_df)
    })
    observeEvent(input$linkClicked,{
      print(input$linkClicked)
      updateTabItems(session,"sidemenu",selected = "warnings")
      output$dropdown=renderMenu({get_warning_dropdown(warning_df)})
    })
  })
  
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

  # update cow selections based on selected dates
  observe({
    update_cow_selection(input$activity_date_range, "activity_cow_selection", session)
  })
  observe({
    update_cow_selection(input$daily_date, "daily_cow_selection", session)
  })
  observe({
    update_cow_selection(input$relationship_date_range, "relationship_cow_selection", session)
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
  
  #' Filter and format feeding, drinking, standing, and lying data for plotting
  #'
  #' @param dashboard_full_analysis 
  #' @param cow_id integer id of the cow whose schedule is to be plotted
  #' @param date character string indicating the date of interest for plotting in the form "YYYY-MM-DD".
  #'
  #' @return a formatted data table for plotting
  #'
  #' @examples 
  #' daily_schedu_moo_data(dashboard_full_analysis=dashboard_full_analysis, cow_id = 6047, date = '2020-07-15')
  daily_schedu_moo_data <-
    function(dashboard_full_analysis, cow_id, date) {
      cow_id <- as.numeric(cow_id)
      date <- as.character(date)
      
      # Create feeding, drinking, and lying_standing dataframes
      feeding <-
        insentec[["Cleaned_feeding_original_data"]]
      drinking <-
        insentec[["Cleaned_drinking_original_data"]]
      lying_standing <-
        hobo[["duration_for_each_bout"]]
      
      # Filter feeding, drinking, and lying_standing dataframes keeping only data from the date of interest
      drinking <- drinking[[date]] %>%
        mutate(Behaviour = 'drinking') %>%
        select(Cow, Behaviour, Start, End, Intake)
      
      feeding <- feeding[[date]] %>%
        mutate(Behaviour = 'feeding') %>%
        select(Cow, Behaviour, Start, End, Intake)
      
      lying_standing <- lying_standing %>%
        filter(floor_date(Start, 'day') == as_date(date) |
                 floor_date(End, 'day') == as_date(date)) %>%
        mutate(Intake = NA) %>%
        select(Cow, Behaviour, Start, End, Intake)
      
      # Concatenate dataframes and only keep data for the cow of interest.
      df <- bind_rows(drinking, feeding, lying_standing) %>%
        filter(Cow %in% cow_id) %>%
        mutate(Cow = paste0('Cow ', as.character(Cow))) %>%
        mutate(event_id = row_number()) %>%
        # Trim events starting before the beginning of the day of interest or after the end of the day of interest
        mutate(Start = case_when(
          floor_date(Start, 'day') < as.POSIXct(date, tz = 'America/Los_Angeles') ~
            as.POSIXct(date, tz = 'America/Los_Angeles'),
          TRUE ~ Start
        )) %>%
        mutate(End = case_when(
          floor_date(End, 'day') > as.POSIXct(date, tz = 'America/Los_Angeles') ~
            as.POSIXct(paste0(date, ' 23:59:59'), tz = 'America/Los_Angeles'),
          TRUE ~ End
        )) %>%
        pivot_longer(cols = Start:End,
                     names_to = 'StartEnd',
                     values_to = 'Time')
      
      # Create new dataframe with an extra row inserted between each event.  Extra row contains Time=NA and Cow=NA.  These rows are required to ensure breaks between line segments are displayed during plotting
      df_blanks = df[1, ]
      for (i in seq(from = 2, to = nrow(df))) {
        df_blanks <- rbind(df_blanks, df[i, ])
        if (i %% 2 == 0) {
          narow = df[i, ]
          narow$Time = NA
          narow$Cow = NA
          df_blanks = rbind(df_blanks, narow)
        }
      }
      
      df_blanks
    }

  #' Generate plot of daily schedule from a dataframe
  #'
  #' @param df dataframe generated by daily_schedu_moo_data() function
  #'
  #' @return plotly figure object
  #'
  #' @examples
  #' daily_schedu_moo_plot(df)
  daily_schedu_moo_plot <- function() {
    # Instantiate figure object
    fig <-
      plot_ly(
        type = 'scatter',
        mode = 'lines',
        width = 800,
        height = 200,
        yaxis=list(type='category'),
      )
    if (!is.null(input$daily_cow_selection)){
      # Generate data frame for date and cow_id of interest
      df <- daily_schedu_moo_data(dashboard_full_analysis=dashboard_full_analysis, cow_id = input$daily_cow_selection, date = input$daily_date)
      
      # Set plotting colours
      standing_colour <- 'rgb(214,26,219)'
      lying_colour <- 'rgb(219,117,26)'
      drinking_colour <- 'rgb(26,127,219)'
      feeding_colour <- 'rgb(31,219,26)'
      
  
      
      # Plot traces for standing, lying, feeding, and drinking events.
      for (i in c('standing', 'lying', 'feeding', 'drinking')) {
        plot_df <- df %>% filter(Behaviour == i)
        color <-  case_when(
          i == 'standing' ~ standing_colour,
          i == 'lying' ~ lying_colour,
          i == 'drinking' ~ drinking_colour,
          i == 'feeding' ~ feeding_colour ,
        )
        fig <-
          add_trace(
            fig,
            data = plot_df,
            y =  ~ Cow,
            x =  ~ Time,
            line = list(
              color = color,
              width = 20,
              opacity = 0.8
            ),
            text =  ~ Intake,
            name = str_to_title(i),
            hovertemplate = paste0(
              str_to_title(i),
              " <br>",
              ifelse(i == 'drinking' |
                       i == 'feeding', "Intake = %{text} kg", ""),
              "<extra></extra>"
            )
          )
        fig <- layout(fig,yaxis=list(title=""))
        
      }
      output[["daily_plot"]] <- renderPlotly(fig)
    }
  }
  
  # Render daily schedule plot
  observe({
    daily_schedu_moo_plot()
  })
})
