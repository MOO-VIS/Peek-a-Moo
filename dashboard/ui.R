library(shinymanager)

header <- dashboardHeader(
  title = "Dairy Cow Dashboard",
  dropdownMenuOutput("contact")
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidemenu",
    menuItem(HTML(paste("&nbsp; Relationships")), icon = icon("connectdevelop"), tabName = "relationships"),
    menuItem("Activity Patterns", icon = icon("chart-line"), tabName = "activities"),
    menuItem("Daily Behavior", icon = icon("calendar"), tabName = "daily_behavior"),
    menuItem("Bins", icon = icon("chart-bar"), tabName = "bins"),
    menuItem("Warnings", icon = icon("exclamation-triangle"), tabName = "warnings"),
    menuItem("Source code",
      icon = icon("file-code"),
      href = "https://github.com/UBC-AWP/Peek-a-Moo"
    )
  )
)

activities_tab <- tabItem(
  "activities",
  fluidRow(
    box(
      title = p(
        "Customizations",
        tags$style(type = "text/css", "#button_activity{border-radius: 0px;border-width: 0px}"),
        bsButton("button_activity", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ),
      width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(2, aggregation_widget("activity_agg_type")),
      column(
        2,
        checkboxInput(
          inputId = "show_average",
          label = "Show Average for Selected Group",
          value = FALSE
        )
      ),
      column(4, date_range_widget("activity_date_range")),
      column(4, cow_selection_widget("activity_cow_selection")),
      bsPopover(
        id = "button_activity", title = "Activity Patterns Tab",
        content = paste("This tab shows six activity patterns over a given timeline. Charts are for the herd average, and selected cow(s).",
          "",
          "<b>Customizations:</b>",
          "<u>Aggregate</u> - aggregates the data on the daily, or monthly level.",
          "<u>Date Range</u> - timeline for the plots.",
          "<u>Cows</u> - cow(s) to showcase in the plots.",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  ),
  fluidRow(
    default_tabBox("Feeding Duration", "feed"),
    default_tabBox("Drinking Duration", "drink")
  ),
  fluidRow(
    default_tabBox("Standing Duration", "standing_time"),
    default_tabBox("Standing Bouts", "standing_bout")
  ),
  fluidRow(
    default_tabBox("Non-nutritive Visits", "non_nutritive"),
    default_tabBox("Feed Intake", "feed_intake")
  )
)


daily_tab <- tabItem(
  "daily_behavior",
  fluidRow(
    box(
      title = p(
        "Customizations",
        tags$style(type = "text/css", "#button_daily{border-radius: 0px;border-width: 0px}"),
        bsButton("button_daily", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ),
      width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(4, date_widget("daily_date")),
      column(4, cow_selection_widget("daily_cow_selection")),
      column(4, h5(br(), "Please select a valid date and cow(s) to generate the plots below")),
      bsPopover(
        id = "button_daily", title = "Daily Behaviour Tab",
        content = paste("This tab depicts the feeding, lying, standing, and drinking behaviours of selected cows. Selecting more than a single cow, will aggregate the results.",
          "",
          "<b>Customizations:</b>",
          "<u>Date</u> - the date for the plots to showcase.",
          "<u>Cows</u> - the cow(s) to showcase in the plots.",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  ),
  fluidRow(
    tags$style(".small-box.bg-yellow { background-color: #94B4D6 !important; color: #FFFFFF !important; }"),
    valueBoxOutput("total_standing", width = 3),
    valueBoxOutput("total_lying", width = 3),
    valueBoxOutput("total_feeding", width = 3),
    valueBoxOutput("total_drinking", width = 3)
  ),
  fluidRow(
    default_tabBox(
      title = p(
        "Daily Behaviour",
        tags$style(type = "text/css", "#button_daily_plot{border-radius: 0px;border-width: 0px}"),
        bsButton("button_daily_plot",
          label = "", icon = icon("info-circle", lib = "font-awesome"),
          size = "extra-small"
        )
      ),
      "daily",
      width = 12,
      popover = bsPopover(
        id = "button_daily_plot", title = "Data note:",
        content = paste("Behaviour totals may not match those on the activity patterns tab, as this visualization does not include behaviours that started before or went beyond the given date. It only considers time for behaviours fully contained in the given date, so as to match the timeline plot."
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  )
)

relationships_tab <- tabItem(
  "relationships",
  fluidRow(
    box(
      title = p(
        "Customizations",
        tags$style(type = "text/css", "#button_network{border-radius: 0px;border-width: 0px}"),
        bsButton("button_network", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ),
      width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      column(3, date_range_widget("relationship_date_range")),
      column(3, network_selection_widget("relationship_network_selection", multiple = FALSE)),
      conditionalPanel(
        condition = "input.relationship_network_selection != 'Displacement Star*' && input.relationship_network_selection != 'Displacement Paired'",
        column(3, threshold_selection_widget("relationship_threshold_selection", multiple = FALSE))
      ),
      conditionalPanel(
        condition = "input.relationship_network_selection == 'Feeding Sychronicity' || input.relationship_network_selection == 'Lying Synchronicity' || input.relationship_network_selection == 'Feeding Neighbours'",
        column(3, layout_selection_widget("relationship_layout_selection", multiple = FALSE))
      ),
      conditionalPanel(
        condition = "input.relationship_network_selection == 'Displacement'",
        column(12, sliderInput("cd_range", "Competition Density", min = 0, max = 1, value = c(0.2, 0.5), step = 0.1))
      ),
      conditionalPanel(
        condition = "input.relationship_network_selection == 'Displacement Star*'",
        column(3, cow_selection_widget("star_cow_selection", multiple = FALSE, label = "Cow of Interest")),
        column(12, sliderInput("star_cd_range", "Competition Density", min = 0, max = 1, value = c(0.2, 0.5), step = 0.1))
      ),
      conditionalPanel(
        condition = "input.relationship_network_selection == 'Displacement Paired'",
        column(3, cow_selection_widget("paired_cow_selection_1", multiple = FALSE, label = "First Cow of Interest")),
        column(3, cow_selection_widget("paired_cow_selection_2", multiple = FALSE, label = "Second Cow of Interest")),
        column(12, sliderInput("paired_cd_range", "Competition Density", min = 0, max = 1, value = c(0.2, 0.5), step = 0.1))
      ),
      bsPopover(
        id = "button_network", title = "Relationships Tab",
        content = paste("This tab shows six activity patterns over a given timeline. Charts are for the herd average, and selected cow(s).",
          "",
          "<b>Customizations (all) :</b>",
          "<u>Date Range</u> - timeline for the given plots",
          "<u>Network</u> - type of network to display",
          "<u>Threshold</u> - top weighted % of connections to display in the network",
          "",
          "<b>Customizations (Feeding/Lying networks) :</b>",
          "<u>Layout Type</u> - shape of the displayed network",
          "",
          "<b>Customizations (Displacement networks) :</b>",
          "<u>Competition Density</u> - threshold of competition density to filter connections in the network by",
          "<u>Cow of Interest</u> - cow to be showcased in the network",
          "<u>First/Second Cow of Interest</u> - the two cows to be showcased in the network together",
          sep = "<br>"
        ),
        placement = "right",
        trigger = "hover",
        options = list(container = "body")
      )
    )
  ),
  fluidRow(
    default_tabBox("Social Network", "network",
      width = 12,
      output_fun = visNetworkOutput
    )
  ),
  fluidRow(
    conditionalPanel(
      condition = "input.relationship_network_selection == 'Displacement Star*' || input.relationship_network_selection == 'Displacement Paired'",
      default_tabBox("Dominance", "elo", width = 12)
    )
  ),
  fluidRow(
    default_tabBox("THI", "THI", width = 12)
  )
)

bins_tab <- tabItem(
  "bins",
  fluidRow(
    box(
      title = p(
        "Customizations",
        tags$style(type = "text/css", "#button_bins{border-radius: 0px;border-width: 0px}"),
        bsButton("button_bins", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ),
      width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      fluidRow(
        column(4, date_widget("bin_date"), ),
        column(8, bin_selection_widget("activity_bin_selection")),
        bsPopover(
          id = "button_bins", title = "Activity Patterns Tab",
          content = paste("This tab shows six activity patterns over a given timeline. Charts are for the herd average, and selected cow(s).",
            "",
            "<b>Customizations:</b>",
            "<u>Date</u> - the date for the plots to showcase",
            "<u>Bins</u> - feed bin(s) to showcase in the hunger plot",
            sep = "<br>"
          ),
          placement = "right",
          trigger = "hover",
          options = list(container = "body")
        )
      )
    )
  ),
  fluidRow(
    default_tabBox("Hunger Plot", "hunger", width = 12)
  ),
  fluidRow(
    box(
      title = p(
        "Customizations",
        tags$style(type = "text/css", "#button_other{border-radius: 0px;border-width: 0px}"),
        bsButton("button_other", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
      ),
      width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
      fluidRow(
        column(8, sliderInput("obs_hr", "Hour", min = 0, max = 23, value = 12)),
        column(4, bin_wt_widget("bin_weight")),
        bsPopover(
          id = "button_other", title = "Activity Patterns Tab",
          content = paste("This tab shows six activity patterns over a given timeline. Charts are for the herd average, and selected cow(s).",
            "",
            "<b>Customizations:</b>",
            "<u>Hour</u> - hour of the selected date to observe in the plot",
            "<u>Full Bin Weight</u> - TBD",
            sep = "<br>"
          ),
          placement = "right",
          trigger = "hover",
          options = list(container = "body")
        )
      ),
    )
  ),
  fluidRow(
    default_tabBox("Hourly Feed Bin Data", "feed_bin", width = 12, output_fun = plotOutput)
  )
)

warnings_tab <- tabItem(
  "warnings",
  box(
    title = p(
      "Customizations",
      tags$style(type = "text/css", "#button_warning{border-radius: 0px;border-width: 0px}"),
      bsButton("button_warning", label = "", icon = icon("info-circle", lib = "font-awesome"), size = "extra-small")
    ),
    width = 12, solidHeader = TRUE, status = "primary", collapsible = TRUE,
    column(
      4,
      numericInput(
        inputId = "food_intake",
        label = "Food Intake Cuttoff",
        value = 0,
        min = 0
      )
    ),
    column(
      4,
      numericInput(
        inputId = "water_intake",
        label = "Water Intake Cuttoff",
        value = 0,
        min = 0
      )
    ),
    column(
      4,
      numericInput(
        inputId = "bin_volume",
        label = "Bin Volume Cuttoff",
        value = 0,
        min = 0
      )
    ),
    bsPopover(
      id = "button_warning", title = "Activity Patterns Tab",
      content = paste("This tab shows six activity patterns over a given timeline. Charts are for the herd average, and selected cow(s).",
        "",
        "<b>Customizations:</b>",
        "<u>Aggregate</u> - aggregates the data on the daily, or monthly level.",
        "<u>Date Range</u> - timeline for the plots.",
        "<u>Cows</u> - cow(s) to showcase in the plots.",
        sep = "<br>"
      ),
      placement = "right",
      trigger = "hover",
      options = list(container = "body")
    )
  ),
  default_tabBox("Warnings", "warning", width = 12, output_fun = DT::dataTableOutput)
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

ui <- fluidPage(dashboardPage(header, sidebar, body))

ui <- secure_app(ui,
  tags_top =
    tags$div(
      tags$h2("Peek-a-Moo\nDashboard", style = "align:center"),
      tags$img(
        src = "../loading_cow1.gif", width = 100
      ),
      tags$p(
        "General visitors can contact dashboard admin for user name and password."
      )
    ),
  tags_bottom = tags$div(
    tags$p(
      "For any questions, please  contact ",
      tags$a(
        href = "mailto:someone@example.com?Subject=Shiny%20aManager",
        target = "_top", "administrator"
      )
    )
  ),
  enable_admin = FALSE,
  download = NULL
)
