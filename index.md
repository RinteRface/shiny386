
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
pak::pak("RinteRface/shiny386")
```

## Demo

<div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-full-screen="false" data-require-bs-caller="card()" data-require-bs-version="5" id="bslib-card-5778" style="margin: 0 auto; float: none;">
<div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
<iframe class="html-fill-item" src="https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6EAATCBYdgAs2ATwDMADgBs43ENEM4qIu2EBeYQUFhJpUqnaIA9FYZtyDAGZQCcDAHMWpSQFc6GFiJbezgnFzgAWmoodhgIggYoCCtVMRMzC2tbLSIMBiiYmAwiBndxAEohSoghbhZGKAYZPilZarqGppbpCHlldvrErrp2DoHO5oATVG4ZBnGh5vd3GaJSACYFxqX3AFEAD1JEra7vOBgoUhYCaqEzi6uCAH1W3r5HElJ9NKgfUiIKkIhKgKO4fGx2E8COwAG7CAA8ETSpnMlhsiWoHi8vjoPnYoWIZAopAwxBgVgAVgRJKCAKxWdgsGAzLSg8EcbBWC5sKwgiBgiGk2HiIEQSaOBFIzRQSZMRDSyZQ2F8PkCjhKmHVADEwgAYmxJsJ2H58V8iBLiNwfDAOMJvJdhI04EbwVw6NxnR8GEaCJcHMJVqQhOKnhBrZLhOLhAAfAB8RrgHoIpD41Bpmj4LHYGDD8DsN1wwgiAE04I1bmGWBHUFB3HAnoolIIRMJJABGYwABTZEOEAEF0BU1C3GHxysP1K25MYACrSXRoVDCLPCCl4r6wKAALzY7gw+6HotEjitLEmWCI1GbolElutEGvN9E1DP3m+ABZh0-b41FY3H9+dpeB63ziH2cCcDSjzsKok6AZwlx4qBNCNBAu6wS28GJnAyZPGwqB-A2yjGPsMKNKk4gABrCGRdhQO6cCpJa0DwOwfAhrm44Jkm5CGgY4gAEIsLwwgADKgm+fAwDAlRgOOcHfviPF4RABGkERTbiDItEUWAxY0Y0LD0R6TFENwLEQexjihtaXFKThvHIUJIkACJaJJ0myfJmHftSOEANZ0EQ+xPO4DBED4qAqWpGkAYBaTsCCBAsBBun6tw-p0DIRpJSlMF4ApgHUkQ1wQd8PhoQAjj4cBWQAJAAyrlEHefFN72cmcB8cIFUsNVtXio1zXsNUbWjd+rW+b+sWFTeVykCB-FgAA8mc3pEKgVwkPlX7xQhpBIUt0QMGh-IYW1fkEIFwXRYR-7iFlLzDbpDWSJeOU4XlqQzlgACquyTUVNJXUFIX4XdxHiFIl5PBcpQQi9b3UMIcOeNA3ABtwaw7cIP3-YDvnA9dYOqRDmkSDARBrJIul9pMhqxFTa2Aj5ogE6IuJmCQM1gAwFW6S5RCY2sACETHcDEuhLXQpAQBE3DuMIMty+6RBXSzT4E3eNpxc+r6SN8Ci7U+a4wEFRzc-+gbLX8anGOwvpmKEFTjWzk7VBW+IMDCoQRo4FXJoED7g6QhYReYfyFvi7CMiQ5TCCAR4uiMcBO4aiLCHAPtkFgZaBz7mak6QdV8xAhaJz5miVYXal1Yln0tQpUZxsIjjCQ4fBNQ3ugAKRsD3y5F3Xw3jQAvhWojh7XDt+r7GeaGKoSdljKYKRXgFLhnyyBi0Jqp7xY6FlAlkYJMlxQMAwAh3VpGNAAunfhan+fl-X9p993+U8cANSzaIdScF1t+e4tUPR1jFBgbQjItoiGlmsf4MBDys2-CwCUNc-h1UevXZKLVHSWUtCUb4XccEjWNvFOsRAYBPG0PYQ+f8byoOEOg4ujNqbxwoVQ1h3gxz0InigtB19obUFho0NGI0E70NRmwJ4pAZAggjIw5hmCZBPW7vHcQkwKDQJkOITO3B8TIizP8MKsARTINEJvJEywDhHCgMwZYABZURbAeAqjIYBWR8iDBSIgDIuRcBCw+Klmkc2NMCrmPaiwLczoDBG2EGFCKqAADCZkIregMNfLBw1CwJMiulDGGSh5ZO7rw2ao8k5PlQApUeZDNDBLbAABgae7IQ5SagcB6DIAcqA+DgijqEH28whBgFHnfIAA&amp;h=0" height="700" width="100%" style="border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem;" allowfullscreen="" allow="autoplay" data-external="1"></iframe>
</div>
<bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
<template>Expand</template>
<button aria-expanded="false" aria-label="Expand card" class="bslib-full-screen-enter badge rounded-pill"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" style="height:1em;width:1em;fill:currentColor;" aria-hidden="true" role="img"><path d="M20 5C20 4.4 19.6 4 19 4H13C12.4 4 12 3.6 12 3C12 2.4 12.4 2 13 2H21C21.6 2 22 2.4 22 3V11C22 11.6 21.6 12 21 12C20.4 12 20 11.6 20 11V5ZM4 19C4 19.6 4.4 20 5 20H11C11.6 20 12 20.4 12 21C12 21.6 11.6 22 11 22H3C2.4 22 2 21.6 2 21V13C2 12.4 2.4 12 3 12C3.6 12 4 12.4 4 13V19Z"/></svg></button>
</bslib-tooltip>
<script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
</div>
<p class="text-center mt-2">
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

<br/>

You can play with the demo app below and modify the code.

<div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-full-screen="false" data-require-bs-caller="card()" data-require-bs-version="5" id="bslib-card-1185" style="margin: 0 auto; float: none;">
<div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
<iframe class="html-fill-item" src="https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6EAATCBYdgAs2ATwDMADgBs43ENEM4qIu2EBeYQUFhJpUqnaIA9FYZtyDAGZQCcDAHMWpSQFc6GFiJbezgnFzgAWmoodhgIggYoCCtVMRMzC2tbLSIMBiiYmAwiBndxAEohSoghbhZGKAYZPilZarqGppbpCHllap8WYQAeCOFUKHc4AH1FJUERAkaAE1nlBdFSL244fTSAWRlDFdV1NIAVaV0WXRgjpYZl05FhRj5ytReH1bqIAGs1vNJJpHHtxKZzJYbNQYR4iER3DsMMQYKlxPsSnAKp9RI54Q4wWAAMIrYR4ogOcTqarVITsUIAN1CIzGjh8EAIWxIfDYqB8pFwwiI-L5AuE9PY7ECEHKwhAAF86T0ZABBdB8QaC+kMJkMcpgeUAXSAA" height="700" width="100%" style="border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem;" allowfullscreen="" allow="autoplay" data-external="1"></iframe>
</div>
<bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
<template>Expand</template>
<button aria-expanded="false" aria-label="Expand card" class="bslib-full-screen-enter badge rounded-pill"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" style="height:1em;width:1em;fill:currentColor;" aria-hidden="true" role="img"><path d="M20 5C20 4.4 19.6 4 19 4H13C12.4 4 12 3.6 12 3C12 2.4 12.4 2 13 2H21C21.6 2 22 2.4 22 3V11C22 11.6 21.6 12 21 12C20.4 12 20 11.6 20 11V5ZM4 19C4 19.6 4.4 20 5 20H11C11.6 20 12 20.4 12 21C12 21.6 11.6 22 11 22H3C2.4 22 2 21.6 2 21V13C2 12.4 2.4 12 3 12C3.6 12 4 12.4 4 13V19Z"/></svg></button>
</bslib-tooltip>
<script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
</div>

## Example

This is a basic example which shows you how to solve a common problem:

``` r
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
```
