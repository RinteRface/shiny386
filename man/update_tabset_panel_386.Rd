% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layout.R
\name{update_tabset_panel_386}
\alias{update_tabset_panel_386}
\title{Update a Bootstrap 386 tabset panel on the client}
\usage{
update_tabset_panel_386(
  session = getDefaultReactiveDomain(),
  inputId,
  selected = NULL
)
}
\arguments{
\item{session}{The \code{session} object passed to function given to
\code{shinyServer}. Default is \code{getDefaultReactiveDomain()}.}

\item{inputId}{The id of the \code{tabsetPanel}, \code{navlistPanel},
or \code{navbarPage} object.}

\item{selected}{The \code{value} (or, if none was supplied, the \code{title})
of the tab that should be selected by default. If \code{NULL}, the first
tab will be selected.}
}
\description{
Update a Bootstrap 386 tabset panel on the client
}
\examples{
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(sidebarLayout(
 sidebarPanel(
   radio_input_386("controller", "Controller", choices = c(1, 2, 3))
 ),
 mainPanel(
   tabset_panel_386(id = "inTabset",
               tab_panel_386(title = "Panel 1", value = "panel1", "Panel 1 content"),
               tab_panel_386(title = "Panel 2", value = "panel2", "Panel 2 content"),
               tab_panel_386(title = "Panel 3", value = "panel3", "Panel 3 content")
   )
 )
 ))

 server <- function(input, output, session) {
   observeEvent(input$controller, {
     update_tabset_panel_386(session, "inTabset",
                       selected = paste0("panel", input$controller)
     )
   })
 }

 shinyApp(ui, server)
}
}
