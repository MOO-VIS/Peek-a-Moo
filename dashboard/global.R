library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(visNetwork)

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


#' Helper function for creating boxes with plot and data tab
#'
#' @param title The title to display for the box
#' @param var_name The beginning of the variable name used by server.R
#' @param width The width of the box, defaults to 6
#' @param output_fun Function for producting the plot output, defaults to plotlyOutput
#'
#' @return tabBox
default_tabBox <- function(title, var_name, width = 6, output_fun = plotlyOutput){
  tabBox(
    title = title, side = "right", selected = "Plot", width = width,
    tabPanel("Data", shinycssloaders::withSpinner(
      image = "loading_cow2.gif",
      DT::dataTableOutput(paste0(var_name, "_table")))
    ),
    tabPanel("Plot", shinycssloaders::withSpinner(
      image = "loading_cow5.gif",
      output_fun(paste0(var_name, "_plot")))
    )
  )
}

#' Helper function to format tables with export option
#'
#' @param df The dataframe to convert
#'
#' @return DT datatable
format_dt_table <- function(df){
  DT::renderDataTable(
    df,
    extensions = "Buttons",
    options = list(
      scrollX = TRUE,
      pageLength = 5,
      dom = "Bftip",
      buttons = c("csv")
    )
  )
}

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