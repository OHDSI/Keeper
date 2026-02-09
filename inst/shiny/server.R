generateLabel <- function(conceptName, startDay, endDay, extraData, keeperTable) {
  if (keeperTable == "presentation") {
    return(paste0(conceptName, if_else(extraData == "",  "", sprintf(" (%s)", extraData))))
  } else if (keeperTable == "visitContext") {
    return(paste0(conceptName, if_else(startDay == endDay, "", sprintf(" (%d days)", endDay - startDay))))
  } else if (keeperTable %in% c("priorDrugs", "postDrugs")) {
    return(sprintf("%s (day %s)", 
                   conceptName[1],
                   paste(sprintf("%d for %d day%s", 
                                 startDay, 
                                 endDay - startDay + 1,
                                 if_else(endDay == startDay, "", "s")),
                         collapse = ", ")))
  } else if (keeperTable == "measurements") {
    return(sprintf("%s (day %s)", 
                   conceptName[1],
                   paste(if_else(extraData == "",
                                 as.character(startDay),
                                 sprintf("%d with value %s", startDay, extraData)),
                         collapse = ", ")))     
  } else {
    return(sprintf("%s (day %s)", 
                   conceptName[1],
                   paste(startDay, collapse = ", ")))     
  }
}

prettifyName <- function(name){
  name <- gsub("([A-Z])", " \\1", name)
  name <- tolower(name)
  name <- gsub("([a-z])([0-9])", "\\1_\\2", name)
  name <- tolower(name)
  name <- gsub("\\b([a-z])", "\\U\\1", name, perl = TRUE)
  return(name)
}

shinyServer(function(input, output, session) {
  person <- shiny::reactiveValues(index = 1)
  
  generatedId <- shiny::reactive({
    return(generatedIds[person$index])
  })
  
  keeperSubset <- shiny::reactive({
    subset <- keeper |>
      filter(generatedId == generatedId())
    return(subset)
  })
  
  decisions <- shiny::reactiveValues(
    decisions = decisionsDataFrame
  )
  
  output$personId <- shiny::renderText({
    if (hasPersonIds) {
      personId <- keeperSubset() |> 
        filter(category == "personId") |>
        pull("conceptName")
      return(personId)
    } else {
      return(generatedId())
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
    subset <- keeperSubset()
    uiElements <- list()
    
    age <- subset |>
      filter(.data$category == "age") |>
      pull(conceptName)
    sex <- subset |>
      filter(.data$category == "sex") |>
      pull(conceptName)
    observationPeriod <- subset |>
      filter(.data$category == "observationPeriod") |>
      select("startDay", "endDay")
    race <- subset |>
      filter(.data$category == "race") |>
      pull(conceptName)
    ethnicity <- subset |>
      filter(.data$category == "ethnicity") |>
      pull(conceptName)
    formattedParts <- tagList(
      sprintf("Age: %s", age),
      br(),
      sprintf("Sex: %s", sex),
      br(),
      sprintf("Observation period: day %d - day %d", observationPeriod$startDay, observationPeriod$endDay)
    )
    if (race != "") {
      formattedParts <- append(formattedParts, list(br(), sprintf("Race: %s", race)))
    }
    if (ethnicity != "") {
      formattedParts <- append(formattedParts, list(br(), sprintf("Ethnicity: %s", ethnicity)))
    }
    uiElements[[length(uiElements) + 1]] <- tagList(
      h3("Demographics"),
      p(formattedParts)
    )
    
    keeperTables <- c("presentation",
                      "visitContext",
                      "symptoms",
                      "priorDisease",
                      "postDisease",
                      "priorDrugs",
                      "postDrugs",
                      "priorTreatmentProcedures",
                      "postTreatmentProcedures",
                      "alternativeDiagnoses",
                      "diagnosticProcedures",
                      "measurements",
                      "death")
    
    for (keeperTable in keeperTables) {
      table <- subset |>
        filter(category == keeperTable) |>
        mutate(
          extraGroup = if (keeperTable == "presentation") .data$extraData else "") |>
        group_by(.data$conceptName, .data$target, .data$extraGroup) |>
        arrange(.data$startDay) |>
        summarise(label = generateLabel(.data$conceptName, .data$startDay, .data$endDay, .data$extraData, keeperTable), .groups = "drop") |>
        mutate(          
          sortOrder = case_when(
            .data$target == "Disease of interest" ~ 1,
            .data$target == "Alternative diagnoses" ~ 0,
            TRUE ~ -1),
          style = case_when(
            target == "Disease of interest" ~ "color: black",
            target == "Alternative diagnoses" ~ "color: red",
            TRUE ~ "color: gray"
          )
        ) |>
        arrange(desc(.data$sortOrder), .data$label) 
      
      formattedParts <- lapply(1:nrow(table), function(i) {
        div(table$label[i], style = table$style[i])
      })
      formattedParts <- tagList(formattedParts)
      uiElements[[length(uiElements) + 1]] <- tagList(
        h3(prettifyName(keeperTable)),
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
