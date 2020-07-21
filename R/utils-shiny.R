# dropNulls
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


#' Create checkbox/switch input based on the selected type
#'
#' Used internally by \link{toggle_input_386} and \link{checkbox_input_386}
#'
#' @inheritParams shiny::checkboxInput
#' @param type Input type. This is to be able to distinguish between switch and checkbox,
#' which have slightly different design.
#'
#' @return An input tag.
#' @export
create_checkbox_tag <- function(inputId, label, value = FALSE, width = NULL,
                                type = c("switch", "checkbox")) {

  type <- match.arg(type)

  value <- restoreInput(id = inputId, default = value)
  input_tag <- tags$input(
    id = inputId,
    type = "checkbox",
    class = "custom-control-input"
  )

  if (!is.null(value) && value) {
    input_tag <- input_tag %>% tagAppendAttributes(checked = "checked")
  }

  input_wrapper <- tags$div(
    class = sprintf("custom-control custom-%s", type),
    style = if (!is.null(width)) {
      paste0("width: ", validateCssUnit(width), ";")
    }
  )

  input_wrapper %>% tagAppendChildren(
    input_tag,
    tags$label(class = "custom-control-label", `for` = inputId, label)
  )
}



shinyInputLabel <- function(inputId, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    `for` = inputId
  )
}

anyNamed <- function (x) {
  if (length(x) == 0)
    return(FALSE)
  nms <- names(x)
  if (is.null(nms))
    return(FALSE)
  any(nzchar(nms))
}

# This function takes in either a list or vector for `choices` (and
# `choiceNames` and `choiceValues` are passed in as NULL) OR it takes
# in a list or vector for both `choiceNames` and `choiceValues` (and
# `choices` is passed as NULL) and returns a list of two elements:
#    - `choiceNames` is a vector or list that holds the options names
#      (each element can be arbitrary UI, or simple text)
#    - `choiceValues` is a vector or list that holds the options values
#       (each element must be simple text)
normalizeChoicesArgs <- function(choices, choiceNames, choiceValues,
                                 mustExist = TRUE) {
  # if-else to check that either choices OR (choiceNames + choiceValues)
  # were correctly provided
  if (is.null(choices)) {
    if (is.null(choiceNames) || is.null(choiceValues)) {
      if (mustExist) {
        stop("Please specify a non-empty vector for `choices` (or, ",
             "alternatively, for both `choiceNames` AND `choiceValues`).")
      } else {
        if (is.null(choiceNames) && is.null(choiceValues)) {
          # this is useful when we call this function from `updateInputOptions()`
          # in which case, all three `choices`, `choiceNames` and `choiceValues`
          # may legitimately be NULL
          return(list(choiceNames = NULL, choiceValues = NULL))
        } else {
          stop("One of `choiceNames` or `choiceValues` was set to ",
               "NULL, but either both or none should be NULL.")
        }
      }
    }
    if (length(choiceNames) != length(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must have the same length.")
    }
    if (anyNamed(choiceNames) || anyNamed(choiceValues)) {
      stop("`choiceNames` and `choiceValues` must not be named.")
    }
  } else {
    if (!is.null(choiceNames) || !is.null(choiceValues)) {
      warning("Using `choices` argument; ignoring `choiceNames` and `choiceValues`.")
    }
    choices <- choicesWithNames(choices) # resolve names if not specified
    choiceNames <- names(choices)
    choiceValues <- unname(choices)
  }

  return(list(choiceNames = as.list(choiceNames),
              choiceValues = as.list(as.character(choiceValues))))
}


# generate options for radio buttons and checkbox groups (type = 'checkbox' or
# 'radio'). Removed the inline parameter from shiny.
generateOptions <- function (inputId, selected, type = "checkbox", choiceNames,
                             choiceValues, session = getDefaultReactiveDomain()) {

  options <- mapply(
    choiceValues,
    choiceNames,
    FUN = function(value, name) {
      idx <- paste0(inputId, "-", type, "-", which(choiceValues == value))

      inputTag <- tags$input(
        type = type,
        id = idx,
        name = inputId,
        value = value,
        class = "custom-control-input"
      )
      if (value %in% selected) inputTag$attribs$checked <- "checked"
      pd <- processDeps(name, session)
      tags$div(
        class = sprintf("custom-control custom-%s", type),
        inputTag,
        tags$label(
          class = "custom-control-label",
          `for` = idx,
          tags$span(pd$html, pd$deps)
        )
      )
    }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  div(class = "shiny-options-group", options)
}


asNamed <- function (x) {
  if (is.null(names(x))) {
    names(x) <- character(length(x))
  }
  x
}

# True when a choice list item represents a group of related inputs.
isGroup <- function(choice) {
  is.list(choice) ||
    !is.null(names(choice)) ||
    length(choice) > 1 ||
    length(choice) == 0
}

# True when choices is a list and contains at least one group of related inputs.
hasGroups <- function(choices) {
  is.list(choices) && any(vapply(choices, isGroup, logical(1)))
}

# Assigns empty names to x if it's unnamed, and then fills any empty names with
# the corresponding value coerced to a character(1).
setDefaultNames <- function(x) {
  x <- asNamed(x)
  emptyNames <- names(x) == ""
  names(x)[emptyNames] <- as.character(x)[emptyNames]
  x
}

# Makes a character vector out of x in a way that preserves names.
asCharacter <- function(x) {
  stats::setNames(as.character(x), names(x))
}

# Processes a "flat" set of choices, or a collection of choices not containing
# any named groups. choices should be a list without any list children, or an
# atomic vector. choices may be named or unnamed. Any empty names are replaced
# with the corresponding value coerced to a character.
processFlatChoices <- function(choices) {
  choices <- setDefaultNames(asCharacter(choices))
  as.list(choices)
}

# Processes a "nested" set of choices, or a collection of choices that contains
# one or more named groups of related choices and zero or more "flat" choices.
# choices should be a named list, and any choice group must have a non-empty
# name. Empty names of remaining "flat" choices are replaced with that choice's
# value coerced to a character.
processGroupedChoices <- function(choices) {
  # We assert choices is a list, since only a list may contain a group.
  stopifnot(is.list(choices))
  # The list might be unnamed by this point. We add default names of "" so that
  # names(choices) is not zero-length and mapply can work. Within mapply, we
  # error if any group's name is ""
  choices <- asNamed(choices)
  choices <- mapply(function(name, choice) {
    choiceIsGroup <- isGroup(choice)
    if (choiceIsGroup && name == "") {
      # If the choice is a group, and if its name is empty, produce an error. We
      # error here because the composite nature of the choice prevents us from
      # meaningfully automatically naming it. Note that while not documented,
      # groups are not necessarily lists (aka generic vectors) but can also be
      # any named atomic vector, or any atomic vector of length > 1.
      stop('All sub-lists in "choices" must be named.')
    } else if (choiceIsGroup) {
      # The choice is a group, but it is named. Process it using the same
      # function we use for "top level" choices.
      processFlatChoices(choice)
    } else {
      # The choice was not named and is not a group; it is a "leaf".
      as.character(choice)
    }
  }, names(choices), choices, SIMPLIFY = FALSE)
  # By this point, any leaves in the choices list might still have empty names,
  # so we're sure to automatically name them.
  setDefaultNames(choices)
}

# Takes a vector/list/factor, and adds names (same as the value) to any entries
# without names. Coerces all leaf nodes to `character`.
choicesWithNames <- function(choices) {
  if (hasGroups(choices)) {
    processGroupedChoices(choices)
  } else {
    processFlatChoices(choices)
  }
}



# Given a Shiny tag object, process singletons and dependencies. Returns a list
# with rendered HTML and dependency objects.
processDeps <- function(tags, session) {
  ui <- takeSingletons(tags, session$singletons, desingleton=FALSE)$ui
  ui <- surroundSingletons(ui)
  dependencies <- lapply(
    resolveDependencies(findDependencies(ui)),
    createWebDependency
  )
  names(dependencies) <- NULL

  list(
    html = doRenderTags(ui),
    deps = dependencies
  )
}


# Useful for update_radio_input_386. Removed the inline parameter from shiny.
updateInputOptions <-function (session, inputId, label = NULL, choices = NULL, selected = NULL,
          type = NULL, choiceNames = NULL, choiceValues = NULL)
{
  if (is.null(type))
    stop("Please specify the type ('checkbox' or 'radio')")
  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues, mustExist = FALSE)
  if (!is.null(selected))
    selected <- as.character(selected)
  options <- if (!is.null(args$choiceValues)) {
    format(tagList(generateOptions(session$ns(inputId), selected,
                                   type, args$choiceNames, args$choiceValues)))
  }
  message <- dropNulls(list(label = label, options = options,
                            value = selected))
  session$sendInputMessage(inputId, message)
}


randomInt <- function (min, max) {
  if (missing(max)) {
    max <- min
    min <- 0
  }
  if (min < 0 || max <= min)
    stop("Invalid min/max values")
  min + sample(max - min, 1) - 1
}


# A scope where we can put mutable global state
.globals <- new.env(parent = emptyenv())
.globals$ownSeed <- NULL

withPrivateSeed <-function (expr) {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    hasOrigSeed <- TRUE
    origSeed <- .GlobalEnv$.Random.seed
  }
  else {
    hasOrigSeed <- FALSE
  }
  if (is.null(.globals$ownSeed)) {
    if (hasOrigSeed) {
      rm(.Random.seed, envir = .GlobalEnv, inherits = FALSE)
    }
  }
  else {
    .GlobalEnv$.Random.seed <- .globals$ownSeed
  }
  on.exit({
    .globals$ownSeed <- .GlobalEnv$.Random.seed
    if (hasOrigSeed) {
      .GlobalEnv$.Random.seed <- origSeed
    } else {
      rm(.Random.seed, envir = .GlobalEnv, inherits = FALSE)
    }
    httpuv::getRNGState()
  })
  expr
}


p_randomInt <- function (...) {
  withPrivateSeed(randomInt(...))
}


markTabAsSelected <- function (x) {
  attr(x, "selected") <- TRUE
  x
}


`%OR%` <- function (x, y)
{
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else x
}


findAndMarkSelectedTab <- function (tabs, selected, foundSelected) {
  tabs <- lapply(tabs, function(div) {
    if (foundSelected || is.character(div)) {
    }
    else if (inherits(div, "shiny.navbarmenu")) {
      res <- findAndMarkSelectedTab(div$tabs, selected,
                                    foundSelected)
      div$tabs <- res$tabs
      foundSelected <<- res$foundSelected
    }
    else {
      if (is.null(selected)) {
        foundSelected <<- TRUE
        div <- markTabAsSelected(div)
      }
      else {
        tabValue <- div$attribs$`data-value` %OR% div$attribs$title
        if (identical(selected, tabValue)) {
          foundSelected <<- TRUE
          div <- markTabAsSelected(div)
        }
      }
    }
    return(div)
  })
  return(list(tabs = tabs, foundSelected = foundSelected))
}



anyNamed <- function (x)
{
  if (length(x) == 0)
    return(FALSE)
  nms <- names(x)
  if (is.null(nms))
    return(FALSE)
  any(nzchar(nms))
}



buildTabset <- function (tabs, ulClass, textFilter = NULL, id = NULL, selected = NULL,
          foundSelected = FALSE) {
  res <- findAndMarkSelectedTab(tabs, selected, foundSelected)
  tabs <- res$tabs
  foundSelected <- res$foundSelected
  if (!is.null(id))
    ulClass <- paste(ulClass, "shiny-tab-input")
  if (anyNamed(tabs)) {
    nms <- names(tabs)
    nms <- nms[nzchar(nms)]
    stop("Tabs should all be unnamed arguments, but some are named: ",
         paste(nms, collapse = ", "))
  }
  tabsetId <- p_randomInt(1000, 10000)
  tabs <- lapply(seq_len(length(tabs)), buildTabItem, tabsetId = tabsetId,
                 foundSelected = foundSelected, tabs = tabs, textFilter = textFilter)
  tabNavList <- tags$ul(class = ulClass, id = id, `data-tabsetid` = tabsetId,
                        lapply(tabs, "[[", 1))
  tabContent <- tags$div(class = "tab-content", `data-tabsetid` = tabsetId,
                         lapply(tabs, "[[", 2))
  list(navList = tabNavList, content = tabContent)
}


isTabSelected <- function (x) {
  isTRUE(attr(x, "selected", exact = TRUE))
}


containsSelectedTab <- function (tabs) {
  any(vapply(tabs, isTabSelected, logical(1)))
}



getIcon <- function (tab = NULL, iconClass = NULL) {
  if (!is.null(tab))
    iconClass <- tab$attribs$`data-icon-class`
  if (!is.null(iconClass)) {
    if (grepl("fa-", iconClass, fixed = TRUE)) {
      iconClass <- paste(iconClass, "fa-fw")
    }
    icon(name = NULL, class = iconClass)
  }
  else NULL
}


navbarMenuTextFilter <- function (text) {
  if (grepl("^\\-+$", text))
    tags$li(class = "divider")
  else tags$li(class = "dropdown-header", text)
}


buildTabItem <- function (index, tabsetId, foundSelected, tabs = NULL, divTag = NULL,
          textFilter = NULL) {
  divTag <- if (!is.null(divTag))
    divTag
  else tabs[[index]]
  if (is.character(divTag) && !is.null(textFilter)) {
    liTag <- textFilter(divTag)
    divTag <- NULL
  }
  else if (inherits(divTag, "shiny.navbarmenu")) {
    tabset <- buildTabset(divTag$tabs, "dropdown-menu", navbarMenuTextFilter,
                          foundSelected = foundSelected)
    containsSelected <- containsSelectedTab(divTag$tabs)
    liTag <- tags$li(class = paste0("dropdown", if (containsSelected)
      " active"), tags$a(href = "#", class = "dropdown-toggle",
                         `data-toggle` = "dropdown", `data-value` = divTag$menuName,
                         getIcon(iconClass = divTag$iconClass), divTag$title,
                         tags$b(class = "caret")), tabset$navList)
    divTag <- tabset$content$children
  }
  else {
    tabId <- paste("tab", tabsetId, index, sep = "-")
    liTag <- tags$li(tags$a(href = paste("#", tabId, sep = ""),
                            `data-toggle` = "tab", `data-value` = divTag$attribs$`data-value`,
                            getIcon(iconClass = divTag$attribs$`data-icon-class`),
                            divTag$attribs$title))
    if (isTabSelected(divTag)) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "tab-pane active"
    }
    divTag$attribs$id <- tabId
    divTag$attribs$title <- NULL
  }
  return(list(liTag = liTag, divTag = divTag))
}
