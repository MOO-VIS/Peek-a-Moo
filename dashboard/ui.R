library(shinydashboard)
library(lubridate)
library(here)

#load(here("data/full_10_month_analysis_result_summary_only_dashboard.Rda"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Activities", icon = icon("bar-chart-o"), tabName = "activities"),
    menuItem("Relationships", icon = icon("heart"), tabName = "relationships"),
    menuItem("Bins", icon = icon("grain", lib = "glyphicon"), tabName = "bins"),
    menuItem("Source code", icon = icon("file-code-o"),
             href = "https://github.com/MOO-VIS/Peek-a-Moo"
    )
  )
)

default_tabBox <- function(title, var_name){
  tabBox(
    title = title, side = "right", selected = "Chart",
    tabPanel("Data", DT::dataTableOutput(paste0(var_name, "_table"))),
    tabPanel("Chart", plotOutput(paste0(var_name, "_plot")))
  )
}

activities_tab <- tabItem(
  "activities",
  fluidRow(
    box(
      title="Customizations", width = 12, solidHeader = TRUE, status = "primary",
      column(3,
             radioButtons(
               inputId = "time_type",
               label = "Time Range Type",
               choiceNames = c("Date Range", "24 Hour"),
               choiceValues = c("date_range", "day")
             )
      ),
      column(4,
             conditionalPanel(
               condition = "input.time_type == 'date_range'",
               dateRangeInput(
                 inputId = "date_range",
                 label = "Date Range",
                 start = today() - years(1),
                 end = NULL,
                 min = NULL,
                 max = NULL
               )
             ),
             conditionalPanel(
               condition = "input.time_type == 'day'",
               dateInput(
                 inputId = "day",
                 label = "Date"
               )
             )
      ),
      column(4,
             selectInput(
               inputId = "cow_selection",
               label = "Cows",
               unique(dashboard_full_analysis[["Insentec"]][["Feeding and drinking analysis"]][["Cow"]]),
               multiple = TRUE,
               selectize = TRUE
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
  )
)


relationships_tab <- tabItem(
  "relationships"
)

bins_tab <- tabItem(
    "bins"
)

body <- dashboardBody(
  tabItems(
    activities_tab,
    relationships_tab,
    bins_tab
  )
)

notifications <- dropdownMenu(
  type = "notifications", badgeStatus = "warning",
  notificationItem(
    text = "Example",
    icon("users")
  ),
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