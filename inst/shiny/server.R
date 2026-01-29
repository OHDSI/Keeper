shinyServer(function(input, output, session) {
  person <- shiny::reactiveValues(index = 1)
  
  decisions <- shiny::reactiveValues(
    decisions = decisionsDataFrame
  )
  
  output$personId <- shiny::renderText({
    return(personIds[person$index])
  })
  
  observe({
    indexDay <- decisions$decisions[person$index, "indexDay"]
    if (!is.na(indexDay)) {
      decision <- decisions$decisions[person$index, "decision"]
      if (is.na(decision))
        decision <- character(0)
      updateRadioButtons(session, "decision", selected = decision)
      updateNumericInput(session, "indexDay", value = indexDay)
    }
  })
  
  shiny::observeEvent(input$nextButton, {
    if (person$index < length(personIds)) {
      person$index <- person$index + 1
    }
  })
  
  shiny::observeEvent(input$previousButton, {
    if (person$index > 1) {
      person$index <- person$index - 1
    }
  })
  
  output$profile <- shiny::renderUI({
    profile <- keeper |>
      filter(personId == personIds[person$index])
    uiElements <- list()
    for (i in seq_len(ncol(profile))) {
      parts <- trimws(strsplit(as.character(profile[1, i]), ";")[[1]])
      if (length(parts) == 1) {
        parts <- trimws(strsplit(as.character(profile[1, i]), "(?<=\\))\\s+", perl = TRUE)[[1]])
      }
      formatted_parts <- lapply(seq_along(parts), function(j) {
        if (j < length(parts)) {
          tagList(parts[j], br()) 
        } else {
          parts[j]
        }
      })
      name <- names(profile)[i]
      name <- gsub("([A-Z])", " \\1", name)
      name <- tolower(name)
      name <- gsub("([a-z])([0-9])", "\\1_\\2", name)
      name <- tolower(name)
      name <- gsub("\\b([a-z])", "\\U\\1", name, perl = TRUE)
      
      uiElements[[i]] <- tagList(
        h3(name),
        p(formatted_parts)
      )
    }
    return(do.call(tagList, uiElements))
  })
  
  shiny::observeEvent(input$decision, {
    decisions$decisions[person$index, "decision"] <- input$decision
    write_csv(decisions$decisions, decisionsFileName)
  })
  
  shiny::observeEvent(input$indexDay, {
    req(!is.na(input$indexDay)) 
    decisions$decisions[person$index, "indexDay"] <- input$indexDay
    write_csv(decisions$decisions, decisionsFileName)
  })
  
  observeEvent(input$theme_selector, {
    new_theme <- bslib::bs_theme(version = 5, bootswatch = input$theme_selector)
    session$setCurrentTheme(new_theme)
  })
  
})