source("functionsForShiny.R")
source("PlotKeeper.R")

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
                      "priorDrugs",
                      "priorTreatmentProcedures",
                      "measurements",
                      "alternativeDiagnoses",
                      "diagnosticProcedures",
                      "postDisease",
                      "postDrugs",
                      "postTreatmentProcedures",
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
            target == "Disease of interest" ~ "color: #11A08A",
            target == "Both" ~ "color: #000000",
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
  
  output$demographics <- renderUI({
    subset <- keeperSubset()
    if (nrow(subset) == 0) {
      return("No data")
    }
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
    cells <- list(
      tags$td(tags$b("Age"), tags$br(), age, align = "center"),
      tags$td(tags$b("Sex"), tags$br(), sex, align = "center"),
      tags$td(tags$b("Observation period"), tags$br(), sprintf("day %d - day %d", observationPeriod$startDay, observationPeriod$endDay), align = "center")
    )
    if (race != "") {
      cells <- append(cells, tags$td(tags$b("Race"), tags$br(), race, align = "center"))
    }
    if (ethnicity != "") {
      cells <- append(cells, tags$td(tags$b("Ethnicity"), tags$br(), ethnicity, align = "center"))
    } 
    
    table <- tags$table(tags$tr(cells), 
                        style = "border-collapse: separate; border-spacing: 20px; margin-right: auto; margin-top: -10px; margin-bottom: -20px;")

    return(table)
  })
  
  output$timeline <- renderPlotly({
    subset <- keeperSubset()
    return(plotTimeline(subset))
  })
  
  shiny::observeEvent(input$decision, {
    decisions$decisionsDataFrame[profile$index, "decision"] <- input$decision
    if (dataList$decisions$type == "file") {
      write_csv(decisions$decisionsDataFrame, dataList$decisions$fileName)
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
      write_csv(decisions$decisionsDataFrame, dataList$decisions$fileName)
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
