library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(visNetwork)
library(lubridate)

#' Helper function for converting dataframes to having dates in a single column
#'
#' @param df The dataframe to convert
#'
#' @return Dataframe with dates in a single date column
convert_date_col <- function(df){
  enframe(df, name = "date") %>%
    mutate(date = as.Date(date)) %>%
    unnest(value)
}

#' Helper function to filter by user input date range
#'
#' @param df The dataframe to filter
#' @param col The date column
#' @param date_obj A list containing start and end date or a single date
#'
#' @return Filtered dataframe with only dates within selected range
filter_dates <- function(df, col, date_obj){
  if(length(date_obj) > 1){
    df %>%
      filter({{col}} >= date_obj[[1]]) %>%
      filter({{col}} <= date_obj[[2]])
  }
  else{
    df %>%
      filter({{col}} == date_obj)
  }
}

#' Helper function to filter by user input cow selection
#'
#' @param df The dataframe to filter
#' @param col The cow column
#' @param cow_selection A list of cows Ids to display
#'
#' @return Filtered dataframe with only selected cows
filter_cows <- function(df, col, cow_selection){
  df %>%
    filter({{col}} %in% cow_selection)
}

# load in plot/table creation scripts
source(here::here("R/notifications.R"))
source(here::here("R/activities.R"))
source(here::here("R/network.R"))

# load data if not already in memory
if(!exists("hobo") || !exists("feed_drink_df")){
    load(here::here("data/full_10_month_analysis_result_summary_only_dashboard.Rda"))
    
    hobo <- dashboard_full_analysis[["HOBO"]]
    insentec <- dashboard_full_analysis[["Insentec"]]
    
    rm(dashboard_full_analysis)
}

# create dataframes for plots and tables
standing_bout_df <- hobo[["lying_standing_summary_by_date"]]
feed_drink_df <- insentec[["Feeding and drinking analysis"]]
non_nutritive_df <- convert_date_col(insentec[["non_nutritive_visits"]])
feeding_together_df <- convert_date_col(insentec[["average number of feeding buddies"]])

#' Helper function for creating boxes with plot and data tab
#'
#' @param title The title to display for the box
#' @param var_name The beginning of the variable name used by server.R
#' @param width The width of the box, defaults to 6
#' @param output_fun Function for producing the plot output, defaults to plotlyOutput
#'
#' @return tabBox
default_tabBox <- function(title, var_name, width = 6, output_fun = plotlyOutput){
  tabBox(
    title = title, side = "right", selected = "Plot", width = width,
    tabPanel("Data", shinycssloaders::withSpinner(
      image = "loading_cow5.gif",
      DT::dataTableOutput(paste0(var_name, "_table")))
    ),
    tabPanel("Plot", shinycssloaders::withSpinner(
      image = "loading_cow2.gif",
      output_fun(paste0(var_name, "_plot")))
    )
  )
}

#' Helper function to format tables with export option
#'
#' @param df The dataframe to convert
#' @param page_length Number of pages to show, defaults to 5
#'
#' @return DT datatable
format_dt_table <- function(df, page_length=5){
  DT::renderDataTable(
    df,
    extensions = "Buttons",
    options = list(
      scrollX = TRUE,
      pageLength = page_length,
      dom = "Bftip",
      buttons = c("csv")
    )
  )
}

# widget helper functions: 
aggregation_widget <- function(inputId){
  radioButtons(
    inputId = inputId,
    label = "Aggregate",
    selected = "month", 
    choiceNames = c("By Day", "By Month"),
    choiceValues = c("day", "month"),
  )
}

date_range_widget <- function(inputId){
  dateRangeInput(
    inputId = inputId,
    label = "Date Range",
    start = lubridate::today() - lubridate::years(1),
    end = NULL,
    min = NULL,
    max = NULL
  )
}

cow_selection_widget <- function(inputId){
  pickerInput(
    inputId = inputId,
    label = "Cows",
    choices = list(),
    multiple = TRUE,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select cows")
  )
}

date_widget <- function(inputId){
  dateInput(
    inputId = inputId,
    label = "Date"
  )
}


#' Helper function for updating cow selection picker input widgets
#'
#' @param date_obj The date or date range to filter by
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
update_cow_selection <- function(date_obj, inputId, session){
  
  # find cows that exist in date range
  cow_choices <- filter_dates(feed_drink_df, date, date_obj) %>%
    select(Cow) %>%
    unique() %>%
    arrange(desc(Cow))
  colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " cows with data in date range")
  
  # update widget
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = cow_choices
  )
}


