library(Rcpp)
library(dplyr)
library(tidyr)
library(purrr)
library(forcats)
library(stringr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(igraph)
library(lubridate)
library(visNetwork)
library(shinyBS)
library(shinyalert)
library(tibble)
library(graphics)
library(ggplot2)
library(DBI)
library(RPostgres)

# load in plot/table creation scripts
source("../R/notifications.R")
source("../R/behaviour.R")
source("../R/daily_behavior.R")
source("../R/network.R")
source("../R/elo.R")
source("../R/bully_analysis.R")
source("../R/THI_analysis.R")
source("../R/FAQ.R")

# use environment variable
Postgres_user <- Sys.getenv("POSTGRES_USER")
Postgres_password <- Sys.getenv("POSTGRES_PASSWORD")
Postgres_host <- Sys.getenv("POSTGRES_HOST")
Postgres_dbname <- "cowbonds"
Postgres_timezone <- "America/Los_Angeles"

# Connect to PostgresSQL database
con <- dbConnect(RPostgres::Postgres(),
  user = Postgres_user,
  password = Postgres_password,
  host = Postgres_host,
  port = 5432,
  dbname = Postgres_dbname,
  timezone = Postgres_timezone
)

master_feed_replacement_all <- tbl(con, "master_feed_replacement_all") %>%
  as.data.frame()

Cleaned_feeding_original_data <- tbl(con, "Cleaned_feeding_original_data") %>%
  as.data.frame()

Cleaned_drinking_original_data <- tbl(con, "Cleaned_drinking_original_data") %>%
  as.data.frame()

non_nutritive_visits <- tbl(con, "non_nutritive_visits") %>%
  as.data.frame()

Replacement_behaviour_by_date <- tbl(con, "Replacement_behaviour_by_date") %>%
  as.data.frame()

elo_24h_na_filled <- tbl(con, "elo_24h_na_filled") %>%
  as.data.frame()

Feeding_and_drinking_analysis <- tbl(con, "Feeding_and_drinking_analysis") %>%
  as.data.frame()

Insentec_warning <- tbl(con, "Insentec_warning") %>%
  as.data.frame()

lying_standing_summary_by_date <- tbl(con, "lying_standing_summary_by_date") %>%
  as.data.frame()

master_summary <- tbl(con, "master_summary") %>%
  as.data.frame()

# load data if not already in memory
if (!exists("THI")) {
  THI <- master_summary

  rm(master_summary)
}

# create dataframes for plots and tables
standing_bout_df <- lying_standing_summary_by_date
feed_drink_df <- Feeding_and_drinking_analysis
non_nutritive_df <- non_nutritive_visits
feeding_intake_df <- Feeding_and_drinking_analysis
replacement_df <- master_feed_replacement_all
dominance_df <- elo_24h_na_filled

max_date <- max(feed_drink_df[["date"]])
min_date <- min(feed_drink_df[["date"]])

#' Filter by user input date range
#'
#' @param df The dataframe to filter
#' @param col The date column
#' @param date_obj A list containing start and end date or a single date
#'
#' @return Filtered dataframe with only dates within selected range
filter_dates <- function(df, col, date_obj) {
  if (is.null(date_obj)) date_obj <- list(min_date, max_date)

  if (length(date_obj) > 1) {
    df %>%
      filter({{ col }} >= date_obj[[1]]) %>%
      filter({{ col }} <= date_obj[[2]])
  } else {
    df %>%
      filter({{ col }} == date_obj)
  }
}

#' Filter data by completion density
#'
#' @param df The dataframe to filter
#' @param col The date column
#' @param cd_range A list of a selected completion density range
#'
#' @return Filtered dataframe within the selected completion density range
filter_cd <- function(df, col, cd_range) {
  df %>%
    filter({{ col }} >= cd_range[[1]]) %>%
    filter({{ col }} <= cd_range[[2]])
}

#' Filter by user input cow selection
#'
#' @param df The dataframe to filter
#' @param col The cow column
#' @param cow_selection A list of cows Ids to display
#'
#' @return Filtered dataframe with only selected cows
filter_cows <- function(df, col, cow_selection) {
  df %>%
    filter({{ col }} %in% cow_selection)
}

#' Creating boxes with plot and data tab
#'
#' @param title The title to display for the box
#' @param var_name The beginning of the variable name used by server.R
#' @param width The width of the box, defaults to 6
#' @param height The height of the box, defaults to 6
#' @param output_fun Function for producing the plot output, defaults to plotlyOutput
#' @param popover An input for a popover message to include in the boxes, defaults to NULL
#' @return tabBox
default_tabBox <- function(title, var_name, width = 6, height = "500px", output_fun = plotlyOutput, popover = NULL) {
  tabBox(
    title = title, side = "right", selected = "Plot", width = width,
    height = height,
    popover,
    tabPanel("Data", shinycssloaders::withSpinner(
      image = "loading_cow_table.gif",
      DT::dataTableOutput(paste0(var_name, "_table"))
    )),
    tabPanel("Plot", shinycssloaders::withSpinner(
      image = paste0("loading_cow6.gif"),
      output_fun(paste0(var_name, "_plot"))
    ))
  )
}

#' Format tables with export option
#'
#' @param df The dataframe to convert
#' @param page_length Number of pages to show, defaults to 5
#' @param data_config User authenticity
#'
#' @return DT datatable
format_dt_table <- function(df, page_length = 5, data_config) {
  if ((length(data_config) == 3 && data_config[[3]] == "t") || is.null(data_config)) {
    . <- c("Data access is limited to Admin and user only.")
    df <- data.frame(.)

    DT::renderDataTable(
      df,
      server = FALSE,
      options = data_config
    )
  } else if ((length(data_config) == 3 && data_config[[3]] == "Bftip")) {
    DT::renderDataTable(
      df,
      server = FALSE,
      options = data_config
    )
  } else {
    DT::renderDataTable(
      df,
      server = FALSE,
      extensions = "Buttons",
      options = data_config
    )
  }
}

# widget helper functions:

#' Widget to add aggregate choices
#'
#' @param inputId The session of input
#'
#' @return A set of radio buttons
aggregation_widget <- function(inputId) {
  radioButtons(
    inputId = inputId,
    label = "Aggregate",
    selected = "day",
    choiceNames = c("By Day", "By Month"),
    choiceValues = c("day", "month"),
  )
}

#' Widget to download the report
#'
#' @param inputId The session of input
#'
#' @return A set of radio buttons
download_format_widget <- function(inputId) {
  radioButtons(
    inputId = inputId,
    label = "Document format",
    selected = "PDF",
    choiceNames = c("PDF", "HTML"),
    choiceValues = c("PDF", "HTML"),
  )
}

#' Widget to select date range
#'
#' @param inputId The session of input
#'
#' @return A pair of text inputs
date_range_widget <- function(inputId) {
  dateRangeInput(
    inputId = inputId,
    label = "Date Range",
    start = lubridate::as_date("2020-8-1"),
    end = lubridate::as_date("2020-8-14"),
    min = lubridate::as_date(min_date),
    max = lubridate::as_date(max_date)
  )
}

#' Widget to select interested cow
#'
#' @param inputId The session of input
#' @param multiple A boolean value represents if multiple inputs can be selected
#' @param label A string of widget title
#'
#' @return A select list
cow_selection_widget <- function(inputId, multiple = TRUE, label = "Cows") {
  pickerInput(
    inputId = inputId,
    label = label,
    choices = list(),
    selected = NULL,
    multiple = multiple,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select cows"
    )
  )
}

#' Widget to select interested network
#'
#' @param inputId The session of input
#' @param multiple A boolean value represents if multiple inputs can be selected
#'
#' @return A select list
network_selection_widget <- function(inputId, multiple = FALSE) {
  pickerInput(
    inputId = inputId,
    label = p(
      "Network",
      tags$style(type = "text/css", "#button_network_info{border-radius: 0px;border-width: 0px}"),
      bsButton("button_network_info", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
    ),
    choices = c("Neighbour", "Synchronicity", "Displacement", "Displacement Star*", "Displacement Paired"),
    selected = NULL,
    multiple = multiple,
    options = pickerOptions(
      maxOptions = 1,
      noneSelectedText = "Select network"
    )
  )
}

#' Widget to select interested threshold
#'
#' @param inputId The session of input
#' @param multiple A boolean value represents if multiple inputs can be selected
#'
#' @return A select list
threshold_selection_widget <- function(inputId, multiple = FALSE) {
  pickerInput(
    inputId = inputId,
    label = paste0("Threshold"),
    choices = c("5%", "10%", "25%", "All"),
    selected = NULL,
    multiple = multiple,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select % top connected cows"
    )
  )
}

#' Widget to select interested layout type
#'
#' @param inputId The session of input
#' @param multiple A boolean value represents if multiple inputs can be selected
#'
#' @return A select list
layout_selection_widget <- function(inputId, multiple = FALSE) {
  pickerInput(
    inputId = inputId,
    label = paste0("Layout type"),
    choices = c("Circle", "Force-directed"),
    selected = NULL,
    multiple = multiple,
    options = list(
      "actions-box" = TRUE,
      "none-selected-text" = "Select layout type"
    )
  )
}

#' Widget to select date
#'
#' @param inputId The session of input
#'
#' @return A text inputs
date_widget <- function(inputId) {
  dateInput(
    inputId = inputId,
    label = "Date",
    value = lubridate::as_date("2020-8-1"),
    max = lubridate::as_date("2020-8-14")
  )
}


#' Updating cow selection picker input widgets
#'
#' @param date_obj The date or date range to filter by
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
#' @param select_all A boolean value that dictates whether all cows are selected upon initialization, defaults to FALSE
#'
#' @return An updated session of input
update_cow_selection <- function(date_obj, inputId, session, select_all = FALSE) {

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
    choices = cow_choices,
    selected = NULL
  )
}

#' Helper function for updating cow selection picker for Neighbours network
#' Updating cow selection picker input widgets for neighbour network
#'
#' @param date_obj The date or date range to filter by
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
#' @param select_all A boolean value that dictates whether all cows are selected upon initialization, defaults to FALSE
#'
#' @return An updated session of input
update_cow_selection_neighbour <- function(date_obj, inputId, session, select_all = FALSE) {

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
    choices = cow_choices,
    selected = NULL,
    options = pickerOptions(maxOptions = 1)
  )
}

#' Updating cow selection picker input widgets for displacement network
#'
#' @param relationship_type The string represents the interested network type, defaults to: Displacement Star*
#' @param date_obj The date or date range to filter by
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
#'
#' @return An updated session of input
update_cow_selection_displacement <- function(relationship_type = "Displacement Star*", date_obj, inputId, session) {
  if (relationship_type != "Displacement Star*") {
    update_cow_selection(date_obj, inputId, session)
  } else {
    # find cows that exist in date range
    cow_choices <- filter_dates(replacement_df, date, date_obj) %>%
      # filter_cd(occupied_bins_with_feed_percent, cd_range) %>%
      select(from) %>%
      unique() %>%
      arrange(desc(from))

    colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " cows with data in date and cd range")

    # update widget
    updatePickerInput(
      session = session,
      inputId = inputId,
      choices = cow_choices,
      selected = NULL
    )
  }
}

#' Updating cow selection picker input widgets for displacement network
#'
#' @param date_obj The date or date range to filter by
#' @param inputId The id of the picker input widget to update
#' @param session The current server session
#' @param cow_id_1 A vector of the 1st interested cow, defaults to NULL
#' @param CD_min A vector of selected minimum completion density, defaults to NULL
#' @param CD_max A vector of selected maximum completion density, defaults to NULL
#'
#' @return An updated session of input
update_2nd_cow_selection_displacement <- function(date_obj,
                                                  inputId,
                                                  session,
                                                  cow_id_1 = NULL,
                                                  CD_min = NULL,
                                                  CD_max = NULL) {
  # find cows that exist in date range
  edges <- combine_replace_edges_star(
    "master_feed_replacement_all", date_obj[1], date_obj[2],
    cow_id_1, CD_min, CD_max
  )

  cow_choices <- data.frame(id = unique(c(
    edges$from,
    edges$to
  ))) %>%
    filter(id != cow_id_1)

  colnames(cow_choices) <- paste0(length(cow_choices[[1]]), " paired cows correlated to ", cow_id_1)

  # update widget
  updatePickerInput(
    session = session,
    inputId = inputId,
    choices = cow_choices,
    selected = NULL
  )
}

#' Custom theme setting function for colors in dashboard
custom_theme <- function() {
  theme <- tags$head(tags$style(HTML(
    ".headerStyling { 
        font-size: 28px;
        line-height: 50px;
        text-align: left;
        padding: 0 10px;
        overflow: hidden;
        color: white;
      }
      .skin-blue .main-header .logo {
        background-color: #6b96c7;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #6b96c7;
      }
      .skin-blue .main-header .navbar {
        background-color: #6b96c7;
      }
        .skin-blue .main-sidebar {
        background-color: #013551;
        }
      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #012a41;
      }
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #012a41;
         }
    .content-wrapper, .right-side {
      background-color: #f0fbff;
    }
    .box.box-solid.box-primary>.box-header {
    color:#fff;
     background:#6b96c7
                    }
    .box.box-solid.box-primary  {
    border-bottom-color:#6b96c7;
    border-left-color:#6b96c7;
    border-right-color:#6b96c7;
    border-top-color:#6b96c7;
    } .nav-tabs-custom>.nav-tabs>li.active {
    border-top-color: #6b96c7;
} .skin-blue .sidebar-menu>li.active>a {
    border-left-color: #6b96c7;
} .skin-blue .sidebar-menu>li:hover>a {
    border-left-color: #6b96c7;}"
  )))

  return(theme)
}
