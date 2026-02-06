shinyServer(function(input, output, session) {
  person <- shiny::reactiveValues(index = 1)
  
  decisions <- shiny::reactiveValues(
    decisions = decisionsDataFrame
  )
  
  output$personId <- shiny::renderText({
    if (hasPersonIds) {
      return(keeper$demographics$personId[person$index])
    } else {
      return(keeper$demographics$generatedId[person$index])
    }
  })
  
  output$database <- shiny::renderText(database)
  output$phenotype <- shiny::renderText(phenotype)
  output$user <- shiny::renderText(session$user)
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
    if (person$index < nPersons) {
      person$index <- person$index + 1
    }
  })
  
  shiny::observeEvent(input$previousButton, {
    if (person$index > 1) {
      person$index <- person$index - 1
    }
  })
  
  output$profile <- shiny::renderUI({
    generatedId <- generatedIds[person$index]
    uiElements <- list()
    for (section in names(keeper)) {
      rows <- keeper[[section]] |>
        filter(generatedId == !!generatedId)
      
      if (section == "demographics") {
        formattedParts <- tagList(
          sprintf("Age: %d", rows$age),
          br(),
          sprintf("Sex: %s", rows$sex),
          br(),
          sprintf("Observation period: day %d - day %d", rows$observationStartDay, rows$observationEndDay),
        )
        if (rows$race != "") {
          formattedParts <- append(formattedParts, list(br(), sprintf("Race: %s", rows$race)))
        }
        if (rows$ethnicity != "") {
          formattedParts <- append(formattedParts, list(br(), sprintf("Ethnicity: %s", rows$ethnicity)))
        }
      } else {
        if (nrow(rows) == 0) {
          formattedParts <- tagList("- None -")
        } else if (section == "visit") {
          if (rows$visitStartDay == rows$visitEndDay) {
            formattedParts <- tagList(rows$conceptName)
          } else {
            formattedParts <- tagList(sprintf("%s (%d days)", rows$conceptName, rows$visitEndDay - rows$visitStartDay))
          }
        } else {
          if (section == "presentation") {
            text <- rows |>
              mutate(
                label = paste0(.data$conceptName,
                               if_else(.data$metaData == "",
                                       "",
                                       sprintf(" (%s)", .data$metaData))
                ),
                style = case_when(
                  target == "Disease of interest" ~ "color: black",
                  target == "Alternative diagnoses" ~ "color: red",
                  TRUE ~ "color: gray"
                ),
                order = case_when(
                  target == "Disease of interest" ~ 1,
                  target == "Alternative diagnoses" ~ 0,
                  TRUE ~ -1
                ) 
              ) |>
              arrange(desc(order), .data$label) 
          } else if (section %in% c("symptoms", "priorDisease", "priorTreatmentProcedures", "diagnosticProcedures" , "postDisease" ,"postTreatmentProcedures")) {
            text <- rows |>
              group_by(.data$conceptName, .data$target) |>
              arrange(.data$startDay) |>
              summarise(days = paste(.data$startDay, collapse = ", "), .groups = "drop") |>
              mutate(label = sprintf("%s (day %s)", .data$conceptName, .data$days),
                     style = case_when(
                       target == "Disease of interest" ~ "color: black",
                       target == "Alternative diagnoses" ~ "color: red",
                       TRUE ~ "color: gray"
                     ),
                     order = case_when(
                       target == "Disease of interest" ~ 1,
                       target == "Alternative diagnoses" ~ 0,
                       TRUE ~ -1
                     ) 
              ) |>
              arrange(desc(order), .data$label) 
          } else if (section %in% c("priorDrugs", "postDrugs")) {
            text <- rows |>
              mutate(label = sprintf("%d for %d day%s", 
                                     .data$startDay, 
                                     .data$endDay - .data$startDay + 1,
                                     if_else(.data$endDay == .data$startDay, "", "s"))) |>
              group_by(.data$conceptName, .data$target) |>
              arrange(.data$startDay) |>
              summarise(days = paste(.data$label, collapse = ", "), .groups = "drop") |>
              mutate(label = sprintf("%s (day %s)", .data$conceptName, .data$days),
                     style = case_when(
                       target == "Disease of interest" ~ "color: black",
                       target == "Alternative diagnoses" ~ "color: red",
                       TRUE ~ "color: gray"
                     ),
                     order = case_when(
                       target == "Disease of interest" ~ 1,
                       target == "Alternative diagnoses" ~ 0,
                       TRUE ~ -1
                     ) 
              ) |>
              arrange(desc(order), .data$label) 
          } else if (section == "measurements") {
            text <- rows |>
              mutate(label = if_else(.data$measurementValue == "",
                                     as.character(.data$startDay),
                                     sprintf("%d with value %s", .data$startDay, .data$measurementValue))) |>
              group_by(.data$conceptName, .data$target) |>
              arrange(.data$startDay) |>
              summarise(days = paste(.data$label, collapse = ", "), .groups = "drop") |>
              mutate(label = sprintf("%s (day %s)", .data$conceptName, .data$days),
                     style = case_when(
                       target == "Disease of interest" ~ "color: black",
                       target == "Alternative diagnoses" ~ "color: red",
                       TRUE ~ "color: gray"
                     ),
                     order = case_when(
                       target == "Disease of interest" ~ 1,
                       target == "Alternative diagnoses" ~ 0,
                       TRUE ~ -1
                     ) 
              ) |>
              arrange(desc(order), .data$label) 
          } else if (section == "alternativeDiagnoses") {
            text <- rows |>
              group_by(.data$conceptName) |>
              arrange(.data$startDay) |>
              summarise(days = paste(.data$startDay, collapse = ", "), .groups = "drop") |>
              mutate(label = sprintf("%s (day %s)", .data$conceptName, .data$days),
                     style = "color: red") |>
              arrange(.data$label) 
          } else {
            stop("Unkown section: ", section)
          }
          formattedParts <- lapply(1:nrow(text), function(i) {
            div(text$label[i], style = text$style[i])
          })
          formattedParts <- tagList(formattedParts)
        }
      }
      
      
      name <- section
      name <- gsub("([A-Z])", " \\1", name)
      name <- tolower(name)
      name <- gsub("([a-z])([0-9])", "\\1_\\2", name)
      name <- tolower(name)
      name <- gsub("\\b([a-z])", "\\U\\1", name, perl = TRUE)
      
      uiElements[[length(uiElements) + 1]] <- tagList(
        h3(name),
        p(formattedParts)
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