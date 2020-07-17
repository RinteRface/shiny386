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



#' Create a Bootstrap 386 navbar page
#'
#' @inheritParams shiny::navbarPage
#'
#' @return A shiny tag
#' @export
#' @rdname navbar
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- navbar_page_386(
#'   "App Title",
#'   id = "tabset",
#'   tab_panel_386(
#'     "Tab 1",
#'     radio_input_386(
#'       "dist", "Distribution type:",
#'       c("Normal" = "norm",
#'         "Uniform" = "unif",
#'         "Log-normal" = "lnorm",
#'         "Exponential" = "exp")
#'     ),
#'     plotOutput("distPlot")
#'   ),
#'   tab_panel_386(
#'     "Tab 2",
#'     select_input_386(
#'       "variable", "Variable:",
#'       c("Cylinders" = "cyl",
#'         "Transmission" = "am",
#'         "Gears" = "gear")
#'     ),
#'     tableOutput("data")
#'   ),
#'   navbar_menu_386(
#'    "More",
#'    tab_panel_386("Summary", "Extra content 1"),
#'    "----",
#'    "Section header",
#'    tab_panel_386("Table", "Extra content 2")
#'   )
#'  )
#'
#'  server <- function(input, output, session) {
#'    output$distPlot <- renderPlot({
#'      dist <- switch(input$dist,
#'                     norm = rnorm,
#'                     unif = runif,
#'                     lnorm = rlnorm,
#'                     exp = rexp,
#'                     rnorm)
#'
#'      hist(dist(500))
#'    })
#'
#'    output$data <- renderTable({
#'      mtcars[, c("mpg", input$variable), drop = FALSE]
#'    }, rownames = TRUE)
#'
#'    observe(print(input$tabset))
#'
#'  }
#'  shinyApp(ui, server)
#' }
navbar_page_386 <- function (title, ..., id = NULL, selected = NULL,
                        position = c("static-top", "fixed-top", "fixed-bottom"),
                        header = NULL, footer = NULL, inverse = FALSE,
                        windowTitle = title) {
  pageTitle <- title
  navbarClass <- "navbar navbar-expand-lg navbar-dark bg-primary"
  position <- match.arg(position)
  if (!is.null(position))
    navbarClass <- paste(navbarClass, " ", position,
                         sep = "")
  if (inverse)
    navbarClass <- paste(navbarClass, "navbar-inverse")
  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)
  tabs <- list(...)
  tabset <- buildTabset(tabs, "nav navbar-nav", NULL, id, selected)

  # Some edit below since Bootstrap 4 significantly changed the layout
  nav_items <- tabset$navList$children[[1]]
  bs4_nav_items <- lapply(nav_items, function(x) {
    if (!is.null(x$attribs$class)) {
      if (x$attribs$class == "dropdown") {
        x$attribs$class <- paste("nav-item",  x$attribs$class)
        x$children[[1]]$attribs$class <- "nav-link"

        subnav_items <- x$children[[2]]$children[[1]]
        x$children[[2]]$children[[1]] <- lapply(subnav_items, function(y) {
          y$attribs$class <- if (!is.null(y$attribs$class)) {
            paste("nav-item", y$attribs$class)
          } else {
            "nav-item"
          }

          if (length(y$children) > 0) {
            if (inherits(y$children[[1]], "shiny.tag")) {
              y$children[[1]]$attribs$class <- "nav-link"
            }
          }
          y
        })

      } else {
        if (length(grep(x = x$attribs$class, pattern = "active")) > 0) {
          x$attribs$class <- NULL
          x$children[[1]]$attribs$class <- "nav-link active"
        } else {
          x$children[[1]]$attribs$class <- "nav-link"
        }
        x$attribs$class <- "nav-item"
      }
    } else {
      x$attribs$class <- "nav-item"
      x$children[[1]]$attribs$class <- "nav-link"
    }
    x
  })

  tabset$navList$children[[1]] <- bs4_nav_items

  # layout a bit different from vanilla shiny since there is no collapsible
  navId <- paste("navbar-collapse-", p_randomInt(1000, 10000), sep = "")
  containerDiv <- div(
    class = "container",
    a(class = "navbar-brand", pageTitle, href = "#"),
    tags$button(
      type = "button",
      class = "navbar-toggler",
      `data-toggle` = "collapse",
      `data-target` = paste0("#", navId),
      `aria-controls` = navId,
      `aria-label` = "Toggle navigation",
      span(class = "navbar-toggler-icon")
    ),
    div(class = "collapse navbar-collapse", id = navId, tabset$navList)
  )

  contentDiv <- div(class = "container")
  if (!is.null(header))
    contentDiv <- tagAppendChild(contentDiv, div(class = "row", header))
  contentDiv <- tagAppendChild(contentDiv, tabset$content)
  if (!is.null(footer))
    contentDiv <- tagAppendChild(contentDiv, div(class = "row", footer))

  page_tag <- page_386(title = title)
  page_tag[[2]][[1]]$children <- tagList(
    tags$nav(class = navbarClass, containerDiv),
    br(),
    contentDiv
  )

  page_tag
}



#' Create a Bootstrap 386 navbar menu
#' @inheritParams shiny::navbarMenu
#' @export
#' @rdname navbar
navbar_menu_386 <- shiny::navbarMenu
