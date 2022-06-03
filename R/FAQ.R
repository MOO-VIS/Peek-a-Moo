#' A helper function for calling the methodologies and data
#' written section into ui.R
#' 
methodologies_data <- function(){
  fluidRow(
    h2(paste0('Methodologies and Data:')),
    tags$li( "PLACEHOLDER", 
             tags$a(tags$i("PLACEHOLDER LINK"),
                    href = "https://awp.landfood.ubc.ca/",
                    target = "_blank"),
    ),
  )
}