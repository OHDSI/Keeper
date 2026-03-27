generateLabel <- function(conceptName, startDay, endDay, extraData, keeperTable) {
  if (keeperTable == "presentation") {
    return(paste0(conceptName, if_else(extraData == "", "", sprintf(" (%s)", extraData))))
  } else if (keeperTable == "visits") {
    return(sprintf("%s%s (%s)",
                   conceptName[1],
                   if_else(extraData[1] == "", "", sprintf(" - %s", extraData[1])),
                   paste(if_else(startDay == endDay,
                                 sprintf("day %s", startDay),
                                 sprintf("days %s to %s", startDay, endDay)),
                         collapse = ", ")))
  } else if (keeperTable %in% c("priorDrugs", "postDrugs")) {
    return(sprintf("%s (%s)", 
                   conceptName[1],
                   paste(sprintf("day %d for %d day%s", 
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
  } else if (section == "visits") {
    return(tags$p("Any visit that occurred in the 30 days prior to 30 days after."))
  } else if (section == "symptoms") {
    text <- "Symptoms that occurred in the 30 days prior, excluding day 0. Symptoms can be conditions or observations."
    categories <- "symptoms"
  } else if (section == "priorDisease") {
    text <- "Conditions related to either the disease of interest or alternative diagnoses recorded any time prior, excluding day 0."
    categories <- c("doi", "complications")
  } else if (section == "postDisease") {
    text <- "Conditions related to either the disease of interest or alternative diagnoses recorded any time after, excluding day 0."
    categories <- c("doi", "complications")
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
    filter(target == "Disease of interest" & conceptSetName != "alternativeDiagnosis") |>
    pull(conceptName)
  if (length(conceptsDoi) == 0) {
    conceptsDoi = "<none>"
  }
  conceptsAd <- concepts |>
    filter(target == "Alternative diagnoses" | conceptSetName == "alternativeDiagnosis") |> 
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
