
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

activities_tab <- tabItem(
  "activities",
  fluidRow(
    box(
      title="Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(2,
             radioButtons(
               inputId = "agg_type",
               label = "Aggregate",
               selected = "month", 
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
               start = lubridate::today() - lubridate::years(1),
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
    default_tabBox("Non-nutritive Visits", "non_nutritive"),
    default_tabBox("Average # Feeding Buddies", "feeding_together")
  )
)

daily_tab <-  tabItem(
  "daily_behavior"
)

relationships_tab <- tabItem(
  "relationships",
  default_tabBox("Social Network", "network", width = 12, output_fun = visNetworkOutput)
)

bins_tab <- tabItem(
    "bins",
    fluidRow(
      box(
        title="Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
        column(4,
               dateInput(
                 inputId = "bin_date",
                 label = "Date"
               )
        )
      )
    ),
    fluidRow(
      default_tabBox("Hunger Plot", "bins")
    )
    
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
