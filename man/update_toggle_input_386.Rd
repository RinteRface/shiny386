% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inputs.R
\name{update_toggle_input_386}
\alias{update_toggle_input_386}
\title{Update \link{toggle_input_386} on the client}
\usage{
update_toggle_input_386(session, inputId, label = NULL, value = NULL)
}
\arguments{
\item{session}{The \code{session} object passed to function given to
\code{shinyServer}. Default is \code{getDefaultReactiveDomain()}.}

\item{inputId}{The id of the input object.}

\item{label}{The label to set for the input object.}

\item{value}{Initial value (\code{TRUE} or \code{FALSE}).}
}
\description{
Update \link{toggle_input_386} on the client
}
\examples{
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  button_386("update", "Go!", class = "btn-lg"),
  toggle_input_386("toggle", "Switch", value = TRUE)
 )

 server <- function(input, output, session) {
   observe(print(input$toggle))
   observeEvent(input$update, {
     update_toggle_input_386(
       session,
       "toggle",
       value = !input$toggle
     )
   })
 }

 shinyApp(ui, server)
}
}
