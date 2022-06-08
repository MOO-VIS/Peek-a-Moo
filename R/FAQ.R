#' A helper function for calling the 'methodologies'
#' written section for the FAQ into ui.R
#' 
#' 
methodologies_FAQ <- function(){
  fluidRow(
    h2(paste0('Methodologies by Tab:')),
    tags$u(tags$b( "Relationships")), 
    tags$li(tags$b("Networks:"),
            "Created with the ",
            tags$a("visNetwork ",
                   href = "https://datastorm-open.github.io/visNetwork/"),
            "package. The network is calcualted with nodes representing individual cows, and the edges representing total synchronized lying/standing time, or count of bouts/interactions between cows."),
    tags$li(tags$b("THI:"),
            "Created with the ",
            tags$a("Plotly ",
                   href = "https://plotly.com/r/"),
            "and ",
            tags$a("ggplot2 ",
                   href = "https://github.com/tidyverse/ggplot2/blob/HEAD/R/plot.r"),
            "packages. The plot shows the daily max, min and average THI values over a given timeline, with the grey dashed line representing the threshold for when cows go into \"heat stress\". The value boxes above show the total max, min, and average THI for the given period."),
    tags$u(tags$b( "Activity Patterns")),
    tags$li(tags$b("All plots:"),
            "Created with the ",
            tags$a("Plotly ",
                   href = "https://plotly.com/r/"),
            "and ",
            tags$a("ggplot2 ",
                   href = "https://github.com/tidyverse/ggplot2/blob/HEAD/R/plot.r"),
            "packages. These plots show a timeseries of total daily values for various behaviours over a selected timeline. The herd average is shown as a default in red."),
    tags$u(tags$b( "Daily Behaviour")),
    tags$li(tags$b("Daily Behaviour:"),
            "Created with the ",
            tags$a("Plotly ",
                   href = "https://plotly.com/r/"),
            "package. This plot shows the lying, standing, drinking, and feeding timeline of the selected cow(s) over a given day. Each behaviour is plotted in a separate colour, by minute. The value boxes show the total cummulative time spent per each behaviour, aggregated for the selected cow(s)."),
    tags$u(tags$b( "Bins")),
    tags$li(tags$b("Hunger Plot"),
            "Created with the ",
            tags$a("Plotly ",
                   href = "https://plotly.com/r/"),
            "and ",
            tags$a("ggplot2 ",
                   href = "https://github.com/tidyverse/ggplot2/blob/HEAD/R/plot.r"),
            "packages. This plot shows the amount of seconds that a bin was empty for the selected date."),
    tags$li(tags$b("Hourly Feed Bin Data:"),
            "Created using the ",
            tags$a("ggplot2 ",
                   href = "https://github.com/tidyverse/ggplot2/blob/HEAD/R/plot.r"),
            "package, and adapted from the heatmap method discussed ",
            tags$a("here.",
                   href = "https://stackoverflow.com/questions/48522350/create-an-image-filled-chart-in-r-using-ggplot"),
            "This plot visually demonstrates how full each bin is on average over the selected hours of a given date. It also displays the amount of times the bin was visited within that same timeline."),
  )
}

#' A helper function for calling the 'about'
#' written section for the FAQ into ui.R
#' 
#' 
about_FAQ <- function(){
  fluidRow(
    h2(paste0('About:')),
    tags$p("This dashboard was created as part of a capstone project for the ",
           tags$a("UBC Master of Data Science",
                  href = "https://masterdatascience.ubc.ca/"),
           " program. The dashboard was first started in 2021 by Sasha Babicki, Elanor Boyle-Stanley, Steffen Pentelow, Ify Anene, Rafael P.H. and Selma. It was further contributed to in 2022 by Allyson Stoll, Kristin Bunyan, Chaoran Wang, and Jiwei Hu."
    )
  )
}

#' A helper function for calling the 'data'
#' written section for the FAQ into ui.R
#' 
#' 
data_FAQ <- function(){
  fluidRow(
    h2(paste0('Data:')),
    tags$p("The data presented in this dashboard was collected at the ",
           tags$a( "UBC Dairy Education and Research Centre",
                   href = "https://dairycentre.landfood.ubc.ca/?login",
           ),
           "in Agassiz, BC, Canada."),
    tags$p("The data was collected using various Instantec and HOBO smart technology within the barn. For more information on the data, it's collection, and usage expectations, please contact the ",
           tags$a("Animal Welfare Department at UBC.", href = "mailto:animal.welfare@ubc.ca"))
  )
}

#' A helper function for calling the 'statistical analysis'
#' written section for the FAQ into ui.R
#' 
#' 
statistical_FAQ <- function(){
  fluidRow(
    h2(paste0('Statistical Analysis:')),
    tags$p("As part of the work on this dashboard, a statistical analysis was developed to contribute to the ongoing research of identifying significant social connections and networks amongst cows. Although the analysis is not explicitly included in the dashboard, an example simulation can be run by naviagting to the \'Relationships\' tab, selecting the \'Neighbours\' network, then the \'Analysis\' tab, and initiating a download of the report. The full statistical analysis and code can also be viewed on it's separate repo ",
           tags$a("here.",
                  href = "https://github.com/UBC-AWP/Peek-a-Moo"))
  )
}

#' A helper function for calling the 'statistical analysis'
#' written section for the FAQ into ui.R
#' 
#' 
references_FAQ <- function(){
  fluidRow(
    h2(paste0('References and Citation:')),
    tags$p("For a full list of references for this dashboard, as well as a citation example, please download the report below.")
  )
}
