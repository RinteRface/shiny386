webr::install(
  "shiny386",
  repos = c("https://rinterface.github.io/rinterface-wasm-cran/", "https://repo.r-wasm.org")
)

library(shiny)
library(shiny386)
ui <- page_386(
 card_386(
  title = "My card",
  "This is my card",
  br(),
  card_link_386(href = "https://www.google.com", "More"),
  footer = "Card footer"
 )
)

server <- function(input, output, session) {}
shinyApp(ui, server)