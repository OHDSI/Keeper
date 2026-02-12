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

getPopoverContent <- function(section, conceptSets = NULL, phenotype = NULL) {
  if (section == "demographics") {
    return(tags$p("Patient demographics, including the age at day 0."))
  } else if (section == "presentation") {
    return(tags$p("Any condition observed on day 0."))
  } else if (section == "visitContext") {
    return(tags$p("Any visit that occurred on day 0 or included day 0."))
  } else if (section == "symptoms") {
    text <- "Symptoms that occurred in the 30 days prior, excluding day 0. Symptoms can be conditions or observations."
    categories <- "symptoms"
  } else if (section == "priorDisease") {
    text <- "Conditions related to either the disease of interest or alternative diagnoses recorded any time prior, excluding day 0."
    categories <- c("doi", "symptoms")
  } else if (section == "postDisease") {
    text <- "Conditions related to either the disease of interest or alternative diagnoses recorded any time after, excluding day 0."
    categories <- c("doi", "symptoms")
  } else if (section == "priorDrugs") {
    text <- "Drugs related to either the disease of interest or alternative diagnoses recorded any time prior, excluding day 0."
    categories <- "drugs"
  } else if (section == "postDrugs") {
    text <- "Drugs related to either the disease of interest or alternative diagnoses recorded any time after, excluding day 0."
    categories <- "drugs"
  } else if (section == "priorTreatmentProcedures") {
    text <- "Treatment procedures related to either the disease of interest or alternative diagnoses recorded any time prior, excluding day 0."
    categories <- "treatmentProcedures"
  } else if (section == "postTreatmentProcedures") {
    text <- "Treatment procedures related to either the disease of interest or alternative diagnoses recorded any time after, excluding day 0."
    categories <- "treatmentProcedures"
  } else if (section == "alternativeDiagnoses") {
    text <- "Alternative diagnoses (conditions) recorded in the 90 days prior to 90 days after."
    categories <- "alternativeDiagnosis"
  } else if (section == "diagnosticProcedures") {
    text <- "Diagnostic procedures either for the disease of interest of alternative diagnoses recorded in the 30 days before to 30 days after."
    categories <- "diagnosticProcedures"
  } else if (section == "measurements") {
    text <- "Measurements related to either the disease of interest or alternative diagnoses recorded in the 30 days before to 30 days after."
    categories <- "measurements"
  } else if (section == "death") {
    return(tags$p("Death recorded any time after, including day 0."))
  } else {
    return("Unknown section")
  }
  concepts <- conceptSets |>
    filter(conceptSetName %in% categories, phenotype == !!phenotype) |>
    mutate(conceptName = if_else(nchar(conceptName) > 50, paste0(substr(conceptName, 1, 47), "..."), conceptName)) |>
    arrange(conceptName)
  conceptsDoi <- concepts |> 
    filter(target == "Disease of interest") |>
    pull(conceptName)
  if (length(conceptsDoi) == 0) {
    conceptsDoi = "<none>"
  }
  conceptsAd <- concepts |>
    filter(target == "Alternative diagnoses") |> 
    pull(conceptName)
  if (length(conceptsAd) == 0) {
    conceptsDoi = "<none>"
  }
  content <- list(
    tags$p(text),
    "Restricted to the following concepts (or their descendants):",
    div(class = "scroll-box",
        tags$table(
          tags$tr(tags$th("Disease of interest"), tags$th("Alternative diagnoses")),
          tags$tr(tags$td(tags$ul(lapply(conceptsDoi, tags$li)), style = "vertical-align: top"), 
                  tags$td(tags$ul(lapply(conceptsAd, tags$li)), style = "vertical-align: top")),
          style = "width: 100%"
        )
    )
  )
  return(content)
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
  
  dataList <- getDataList(session)
  
  decisions <- reactiveValues(decisionsDataFrame = dataList$decisions$decisionsDataFrame)
  
  profile <- shiny::reactiveValues(index = 1)
  
  keeperSubset <- shiny::reactive({
    key <- isolate(decisions$decisionsDataFrame)[profile$index, ] |>
      select(databaseId, phenotype, generatedId)
    subset <- dataList$keeper |>
      inner_join(key, by = join_by(databaseId, phenotype, generatedId))
    return(subset)
  })
  
  output$personId <- shiny::renderText({
    if (dataList$hasPersonIds) {
      personId <- keeperSubset() |> 
        filter(category == "personId") |>
        pull("conceptName")
    } else {
      personId <-  keeperSubset() |> 
        head(1) |>
        pull(generatedId)
    }
    return(personId)
  })
  
  output$database <- shiny::renderText({
    keeperSubset() |> 
      head(1) |>
      pull(databaseId)
  })
  
  output$phenotype <- shiny::renderText({
    keeperSubset() |> 
      head(1) |>
      pull(phenotype)
  })
  
  output$adjudicator <- shiny::renderText(dataList$decisions$adjudicator)
  
  output$maxLabel <- shiny::renderText(sprintf("/ %d", dataList$nProfiles))
  
  observe({
    indexDay <- decisions$decisionsDataFrame[profile$index, "indexDay"]
    if (!is.na(indexDay)) {
      decision <- decisions$decisionsDataFrame[profile$index, "decision"]
      if (is.na(decision))
        decision <- character(0)
      
      updateRadioButtons(session, "decision", selected = decision)
      updateNumericInput(session, "indexDay", value = indexDay)
    }
  })
  
  shiny::observeEvent(input$nextButton, {
    if (profile$index < dataList$nProfiles) {
      profile$index <- profile$index + 1
      updateTextInput(session, "profileIndex", value = profile$index)
    }
  })
  
  shiny::observeEvent(input$previousButton, {
    if (profile$index > 1) {
      profile$index <- profile$index - 1
      updateTextInput(session, "profileIndex", value = profile$index)
    }
  })
  
  shiny::observeEvent(input$profileIndex, {
    if (!is.na(as.integer(input$profileIndex)) &&
        as.numeric(input$profileIndex) >= 1 &&
        as.numeric(input$profileIndex) <= dataList$nProfiles &&
        as.numeric(input$profileIndex) != isolate(profile$index)) {
      profile$index <- as.numeric(input$profileIndex)
    }
  }, ignoreInit = TRUE)
  
  output$profile <- shiny::renderUI({
    subset <- keeperSubset()
    if (nrow(subset) == 0) {
      return("No data")
    }
    
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
      h3("Demographics",
         popover(
           icon("circle-info", style="font-size: 17px; color: #336b92"),
           getPopoverContent("demographics"),
           title = "Demographics"
         )
      ),
      p(formattedParts),
      
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
            .data$target == "Disease of interest" ~ 2,
            .data$target == "Both" ~ 1,
            .data$target == "Alternative diagnoses" ~ 0,
            TRUE ~ -1),
          style = case_when(
            target == "Disease of interest" ~ "color: #000000",
            target == "Both" ~ "color: #11A08A",
            target == "Alternative diagnoses" ~ "color: #EB6622",
            TRUE ~ "color: #999999"
          )
        ) |>
        arrange(desc(.data$sortOrder), .data$label) 
      
      formattedParts <- lapply(1:nrow(table), function(i) {
        div(table$label[i], style = table$style[i])
      })
      formattedParts <- tagList(formattedParts)
      
      uiElements[[length(uiElements) + 1]] <- tagList(
        h3(prettifyName(keeperTable),

           popover(
             icon("circle-info", style="font-size: 17px; color: #336b92"),
             getPopoverContent(section = keeperTable, 
                               conceptSets = dataList$conceptSets,    
                               phenotype = keeperSubset() |> 
                                 head(1) |>
                                 pull(phenotype)),
             title = prettifyName(keeperTable)
           )
        ),
        p(formattedParts)
      )
    }
    
    return(do.call(tagList, uiElements))
  })
  
  shiny::observeEvent(input$decision, {
    decisions$decisionsDataFrame[profile$index, "decision"] <- input$decision
    if (dataList$decisions$type == "file") {
      write_csv(decisions$decisions, dataList$decisionsFileName)
    } else if (dataList$decisions$type == "database") {
      writeLines(sprintf("Updating database, setting decision to %s", input$decision))
      key <- keeperSubset() |> 
        head(1) |>
        select(databaseId, phenotype, generatedId)
      sql <- "UPDATE @database_schema.adjudications
        SET decision = '@decision'
        WHERE database_id = '@database_id'
          AND phenotype = '@phenotype'
          AND generated_id = @generated_id
          AND adjudicator = '@adjudicator';"
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connectionPool,
        sql = sql,
        database_schema = databaseSchema,
        database_id = key$databaseId,
        phenotype = key$phenotype,
        generated_id = key$generatedId,
        adjudicator = dataList$adjudicator,
        decision = input$decision,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
    }
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(input$indexDay, {
    req(!is.na(input$indexDay))
    decisions$decisionsDataFrame[profile$index, "indexDay"] <- input$indexDay
    if (dataList$decisions$type == "file") {
      write_csv(decisions$decisions, dataList$decisionsFileName)
    } else if (dataList$decisions$type == "database") {
      writeLines(sprintf("Updating database, setting index_day to %s", input$indexDay))
      key <- keeperSubset() |> 
        head(1) |>
        select(databaseId, phenotype, generatedId)
      sql <- "UPDATE @database_schema.adjudications
        SET index_day = @index_day
        WHERE database_id = '@database_id'
          AND phenotype = '@phenotype'
          AND generated_id = @generated_id
          AND adjudicator = '@adjudicator';"
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connectionPool,
        sql = sql,
        database_schema = databaseSchema,
        database_id = key$databaseId,
        phenotype = key$phenotype,
        generated_id = key$generatedId,
        adjudicator = dataList$adjudicator,
        index_day = input$indexDay,
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
    }
  }, ignoreInit = TRUE)
})
