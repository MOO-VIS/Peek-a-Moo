library(shinydashboard)
library(shinyWidgets)
library(lubridate)
library(here)
library(plotly)

#load(here("data/full_10_month_analysis_result_summary_only_dashboard.Rda"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Activity Patterns", icon = icon("bar-chart-o"), tabName = "activities"),
    menuItem("Daily Behavior", icon = icon("calendar"), tabName = "daily_behavior"),
    menuItem("Relationships", icon = icon("heart"), tabName = "relationships"),
    menuItem("Bins", icon = icon("grain", lib = "glyphicon"), tabName = "bins"),
    menuItem("Warnings", icon = icon("exclamation-triangle"), tabName = "warnings"),
    menuItem("Source code", icon = icon("file-code-o"),
             href = "https://github.com/MOO-VIS/Peek-a-Moo"
    )
  )
)

#' Helper function for creating boxes with plot and data tab
#'
#' @param title The title to display for the box
#' @param var_name The beginning of the variable name used by server.R
#'
#' @return tabBox
default_tabBox <- function(title, var_name){
  tabBox(
    title = title, side = "right", selected = "Plot",
    tabPanel("Data", DT::dataTableOutput(paste0(var_name, "_table"))),
    tabPanel("Plot", plotlyOutput(paste0(var_name, "_plot")))
  )
}

activities_tab <- tabItem(
  "activities",
  fluidRow(
    box(
      title="Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(2,
             radioButtons(
               inputId = "agg_type",
               label = "Aggregate",
               choiceNames = c("By Day", "By Month"),
               choiceValues = c("day", "month"),
             )
      ),
      column(2,
             checkboxInput(
               inputId = "show_average",
               label = "Show Dashed Average Lines",
               value = FALSE
             )
      ),
      column(4,
             dateRangeInput(
               inputId = "date_range",
               label = "Date Range",
               start = today() - years(1),
               end = NULL,
               min = NULL,
               max = NULL
             )
      ),
      column(4,
             pickerInput(
               inputId = "cow_selection",
               label = "Cows",
               choices = list(),
               multiple = TRUE,
               options = list(
                 "actions-box" = TRUE,
                 "none-selected-text" = "Select cows")
             )
      )
    )
  ),
  fluidRow(
    default_tabBox("Feeding Duration", "feed"),
    default_tabBox("Drinking Duration", "drink")
  ),
  fluidRow(
    default_tabBox("Standing Duration", "standing_time"),
    default_tabBox("Standing Bout Duration", "standing_bout")
  ),
  fluidRow(
    default_tabBox("Non-nutritive visits", "non_nutritive")
  )
)

daily_tab <-  tabItem(
  "daily_behavior"
)

relationships_tab <- tabItem(
  "relationships"
)

bins_tab <- tabItem(
    "bins"
)

warnings_tab <- tabItem(
  "warnings"
)

body <- dashboardBody(
  tabItems(
    activities_tab,
    daily_tab,
    relationships_tab,
    bins_tab,
    warnings_tab
  )
)

notifications <- dropdownMenu(
  type = "notifications", badgeStatus = "warning",
  notificationItem(
    text = "Warning message example",
    icon = icon("exclamation-triangle"),
    status = "warning"
  )
)

header <- dashboardHeader(
  title = "Dairy Cow Dashboard",
  notifications
)

shinyUI(dashboardPage(header, sidebar, body, skin = "blue"))