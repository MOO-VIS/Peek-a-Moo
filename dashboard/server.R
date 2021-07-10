
shinyServer(function(input, output, session) {
  
  # Warning section
  observe({
    warning_df <- combine_warnings(
      insentec, 
      food_cuttoff = input$food_intake, 
      water_cuttoff = input$water_intake
    )
    
    output$warning_table <- format_dt_table(warning_df, page_length = 20)
    print(warning_df %>%
      filter(date == max(date)))
    # Warning notifications menu
    output$notifications <- renderMenu({
      get_warning_dropdown(warning_df)
    })
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
      select(Cow) %>%
      unique() %>%
      arrange(desc(Cow))
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
  
  ## feed bin plot section
  feed_bin_df <- convert_date_col(insentec$Cleaned_feeding_original_data) %>%
    mutate(Startweight = case_when(Startweight < 0 ~ 0, # set to zero where weight is negative
                                  TRUE ~ Startweight),
           Endweight = case_when(Endweight < 0 ~ 0,
                                  TRUE ~ Endweight))
  # choose date
  one_bin_df <- feed_bin_df %>% group_by(Bin) %>% arrange(Start) %>% slice(1)
  # img from https://www.flaticon.com/free-icon/lunch-box_3649211?term=container&page=1&position=34&page=1&position=34&related_id=3649211&origin=tag#
  # Load png file from imgur as binary
  container_img <- readPNG("www/container.png")
  h <- dim(container_img)[1]
  w <- dim(container_img)[2]  
  
  # Find the rows where image starts & ends
  pos1 <- which(apply(container_img[,,1], 1, function(y) any(y==1)))
  mn1 <- min(pos1)
  mx1 <- max(pos1)
  pospct <- round((mx1-mn1)*one_bin_df$Startweight+mn1)

  imgmtx <- container_img[h:1,,1]
  whitemtx <- (imgmtx==1)
  colmtx <- matrix(rep(FALSE,h*w),nrow=h)
  # midpt <- round(w/2)-10
  colmtx[mx1:pospct, 1:w] <- TRUE
  
  ## testing this section
  dim(colmtx) <- c(dim(colmtx)[1], dim(colmtx)[-1], 1)
  ## testing above
  
  imgmtx[whitemtx & colmtx] <- 0.5
  # Need to melt the matrix into a data.frame that ggplot can understand
  df <- reshape2::melt(imgmtx)
  cols <- c(rgb(255,255,255,maxColorValue = 255),  # bg
            rgb(128,128,128,maxColorValue = 255),  # empty
            rgb(42,128,183,maxColorValue = 255))   # full
  # Then use a heatmap with 3 colours for background, and percentage fills
  # Converting the fill value to a factor causes a discrete scale.
  # geom_tile takes three columns: x, y, fill corresponding to 
  # x-coord, y-coord, and colour of the cell.
  ggplot(df, aes(x = Var2, y = Var1, fill = factor(value)))+
    geom_tile() +
    scale_fill_manual(values = cols) +
    theme(legend.position = "none") +
    facet_wrap(~Bin)
  
  })
