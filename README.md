
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shiny386

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/shiny386)](https://CRAN.R-project.org/package=shiny386)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/RinteRface/shiny386/workflows/R-CMD-check/badge.svg)](https://github.com/RinteRface/shiny386/actions)
<!-- badges: end -->

The goal of shiny386 is to provide an old school Bootstrap 4 template
for Shiny. It is built on top of the [Bootstrap
386](http://kristopolous.github.io/BOOTSTRA.386/demo.html) HTML
template.

## Installation

You can install the released version of shiny386 from Github with:

``` r
pak::paks("RinteRface/shiny386")
```

## Example

<img src="man/figures/penguins-demo.png" alt="shiny366 demo image" width="100%">

<p class="text-center">
<a 
class="btn btn-primary" 
data-bs-toggle="collapse" 
href="#demo-code" 
role="button" 
aria-expanded="false" 
aria-controls="demo-code"> Toggle code </a>
</p>

<div id="demo-code" class="collapse">

``` r
library(shiny)
library(shiny386)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(thematic)

thematic_shiny(font = "auto")

penguins_csv <- "https://raw.githubusercontent.com/jcheng5/simplepenguins.R/main/penguins.csv"

df <- readr::read_csv(penguins_csv)
# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric), -Year)

ui <- page_386(
  h1("Penguins App"),
  br(),

  h3("This app is just amazing..."),

  fluidRow(
    column(
      width = 4,
      card_386(
        title = "Aesthetics",
        status = "warning",
        select_input_386("xvar", "X variable", colnames(df_num), selected = "Bill Length (mm)"),
        select_input_386("yvar", "Y variable", colnames(df_num), selected = "Bill Depth (mm)"),
        checkbox_group_input_386(
          "species", "Filter by species",
          choices = unique(df$Species),
          selected = unique(df$Species)
        )
      ),
      card_386(
        title = "Other options",
        status = "warning",
        checkbox_input_386("by_species", "Show species", TRUE),
        checkbox_input_386("show_margins", "Show marginal plots", TRUE),
        checkbox_input_386("smooth", "Add smoother")
      ),
      button_386("run", "Do plot!", class = "btn-lg btn-block")
    ),
    column(
      width = 8,
      jumbotron_386(plotOutput("scatter"))
    )
  )
)

server <- function(input, output, session) {

  subsetted <- eventReactive(input$run, {
    req(input$species)
    df |> filter(Species %in% input$species)
  })

  output$scatter <- renderPlot(
    {
      p <- ggplot(subsetted(), aes(.data[[input$xvar]], .data[[input$yvar]])) +
        list(
          theme(legend.position = "bottom"),
          if (input$by_species) aes(color = Species),
          geom_point(),
          if (input$smooth) geom_smooth()
        )

      if (input$show_margins) {
        margin_type <- if (input$by_species) "density" else "histogram"
        p <- ggExtra::ggMarginal(p,
          type = margin_type, margins = "both",
          size = 8, groupColour = input$by_species, groupFill = input$by_species
        )
      }

      p
    },
    res = 100
  )
}

shinyApp(ui, server)
```

</div>
