library(shinydashboard)
library(lubridate)
library(here)

#load(here("data/full_10_month_analysis_result_summary_only_dashboard.Rda"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Analysis", icon = icon("bar-chart-o"),
             menuSubItem("Actor/Reactor", tabName = "subitem1"),
             menuSubItem("Networking", tabName = "subitem2")
    ),
    menuItem("Source code", icon = icon("file-code-o"),
             href = "https://github.com/MOO-VIS/Peek-a-Moo"
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      "dashboard",
      fluidRow(
        box(
          title="Customizations", width = 12, solidHeader = TRUE, status = "primary",
          column(6,
            dateRangeInput(
             "date_range",
             "Date",
             start = today() - years(1),
             end = NULL,
             min = NULL,
             max = NULL
            )
          ),
          column(6,
            selectInput(
             "cow_selection",
             "Cows",
             unique(dashboard_full_analysis[["Insentec"]][["Feeding and drinking analysis"]][["Cow"]]),
             multiple = TRUE,
             selectize = TRUE
            )
          )
        )
      ),
      fluidRow(
        tabBox(
          title = "Feeding Duration", side = "right", selected = "Over Time",
          tabPanel("Data", DT::dataTableOutput("feed_table")),
          tabPanel("Over Time", plotOutput("feed_range"))
        ),
        tabBox(
          title = "Drinking Duration", side = "right", selected = "Over Time",
          tabPanel("Data", DT::dataTableOutput("drink_table")),
          tabPanel("Over Time", plotOutput("drink_range"))
        )
      ),
      fluidRow(
        tabBox(
          title = "Standing Duration", side = "right", selected = "Over Time",
          tabPanel("Data", DT::dataTableOutput("standing_time_table")),
          tabPanel("Over Time", plotOutput("standing_time_range"))
        ),
        tabBox(
          title = "Standing Bout Duration", side = "right", selected = "Over Time",
          tabPanel("Data", DT::dataTableOutput("standing_bout_table")),
          tabPanel("Over Time", plotOutput("standing_bout_range"))
        )
      )
    )
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