header <- dashboardHeader(
  title = "Dairy Cow Dashboard",
  dropdownMenuOutput("notifications")
)

sidebar <- dashboardSidebar(
  sidebarMenu(id="sidemenu",
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
      column(2, aggregation_widget("activity_agg_type")),
      column(2,
             checkboxInput(
               inputId = "show_average",
               label = "Show Dashed Average Lines",
               value = FALSE
             )
      ),
      column(4, date_range_widget("activity_date_range")),
      column(4, cow_selection_widget("activity_cow_selection"))
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
  "daily_behavior",
  fluidRow(
    box(
      title="Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(4, date_widget("daily_date")),
      column(4, cow_selection_widget("daily_cow_selection"))
    )
  ),
  fluidRow(
    default_tabBox("Daily Behavior", "daily")
  ),
)

relationships_tab <- tabItem(
  "relationships",
  fluidRow(
    box(
      title="Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(2, aggregation_widget("relationship_agg_type")),
      column(4, date_range_widget("relationship_date_range")),
      box(
        title="Actor/Reactor Customizations", width = 6,
        column(12, cow_selection_widget("relationship_cow_selection"))
      )
    )
  ),
  fluidRow(
    default_tabBox("Social Network", "network", width = 12, output_fun = visNetworkOutput)
  ),
  fluidRow(
    default_tabBox("Actor/Reactor", "bullying", width = 12)
  )
)

bins_tab <- tabItem(
    "bins",
    fluidRow(
      box(
        title="Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
        column(4, date_widget("bin_date"))
      )
    ),
    fluidRow(
      default_tabBox("Hunger Plot", "bins", width = 12)
    )
    
)

warnings_tab <- tabItem(
  "warnings",
  box(
    title="Customizations", width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
    column(4,
           numericInput(
             inputId = "food_intake",
             label = "Food Intake Cuttoff",
             value = 0,
             min = 0
          )
    ),
    column(4,
           numericInput(
             inputId = "water_intake",
             label = "Water Intake Cuttoff",
             value = 0,
             min = 0
           )
    )
  ),
  default_tabBox("Warnings", "warning", width = 12)
)

body <- dashboardBody(
  tags$script(
    HTML(
      "function clickFunction(link){
        Shiny.onInputChange('linkClicked',link);
      }"
    )
  ),
  tabItems(
    activities_tab,
    daily_tab,
    relationships_tab,
    bins_tab,
    warnings_tab
  )
)

shinyUI(dashboardPage(header, sidebar, body, skin = "blue"))