#' Create Bootstrap 386 page skeleton
#'
#' @param ... Slot for shiny386 layout elements.
#' @param title The browser window title (defaults to the host URL of the page).
#' Can also be set as a side effect of the titlePanel() function.
#'
#' @return A list of tags
#' @export
page_386 <- function(..., title = NULL) {
  tagList(
    # Head
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$title(title),
      tags$meta(
        name = "viewport",
        content = "
          width=device-width,
          initial-scale=1"
      ),
      tags$meta(`http-equiv` = "X-UA-Compatible", content = "IE=edge")
    ),
    # body
    tags$body(
      div(
        class = "container",
        ...
      )
    ) %>% use_bs4_deps()
  )
}



#' Create a Bootstrap 386 tabset panel
#' @inheritParams shiny::tabsetPanel
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   tabset_panel_386(
#'     id = "tabset",
#'     selected = "Tab 2",
#'     tabPanel("Tab 1", "Content 1"),
#'     tabPanel("Tab 2", "Content 2")
#'   )
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe(print(input$tabset))
#'  }
#'  shinyApp(ui, server)
#'
#' }
tabset_panel_386 <- function(..., id = NULL, selected = NULL,
                             type = c("tabs", "pills"), position = NULL) {
  type <- match.arg(type)

  # We run the Shiny tabsetPanel function, to edit it later. This
  # is to avoid to rewrite all internal functions...
  temp_tabset <- tabsetPanel(
    ...,
    id = id,
    selected = selected,
    type = type,
    position = position
  )

  # Some edit below since Bootstrap 4 significantly changed the layout
  nav_items <- temp_tabset$children[[1]]$children[[1]]
  found_active <- FALSE
  bs4_nav_items <- lapply(nav_items, function(x) {
    if (!is.null(x$attribs$class)) {
      if (grep(x = x$attribs$class, pattern = "active")) {
        x$attribs$class <- NULL
        found_active <- TRUE
      }
    }
    x$attribs$class <- if (is.null(x$attribs$class)) {
      "nav-item"
    } else {
      paste("nav-item",  x$attribs$class)
    }
    x$children[[1]]$attribs$class <- if (found_active) {
      "nav-link active"
    } else {
      "nav-link"
    }
    x
  })

  temp_tabset$children[[1]]$children[[1]] <- bs4_nav_items
  temp_tabset
}


#' Create a Bootstrap 386 tab panel
#' @inheritParams shiny::tabPanel
#' @export
tab_panel_386 <- shiny::tabPanel


#' Update a Bootstrap 386 tabset panel on the client
#' @inheritParams shiny::updateTabsetPanel
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(sidebarLayout(
#'  sidebarPanel(
#'    radio_input_386("controller", "Controller", choices = c(1, 2, 3))
#'  ),
#'  mainPanel(
#'    tabset_panel_386(id = "inTabset",
#'                tab_panel_386(title = "Panel 1", value = "panel1", "Panel 1 content"),
#'                tab_panel_386(title = "Panel 2", value = "panel2", "Panel 2 content"),
#'                tab_panel_386(title = "Panel 3", value = "panel3", "Panel 3 content")
#'    )
#'  )
#'  ))
#'
#'  server <- function(input, output, session) {
#'    observeEvent(input$controller, {
#'      update_tabset_panel_386(session, "inTabset",
#'                        selected = paste0("panel", input$controller)
#'      )
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#' }
update_tabset_panel_386 <- shiny::updateTabsetPanel
