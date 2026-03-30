# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# source_data = group

convertKeeperTableToKeeper <- function(source_data) {
  if (!"generatedId" %in% colnames(source_data)) {
    source_data$generatedId <- source_data$personId
  }
  if (!"sex" %in% colnames(source_data)) {
    source_data$sex <- source_data$gender
  }
  if (!"race" %in% colnames(source_data)) {
    source_data$race <- ""
  }
  if (!"ethnicity" %in% colnames(source_data)) {
    source_data$ethnicity <- ""
  }
  if ("visitContext" %in% colnames(source_data)) {
    source_data$visits <- source_data$visitContext
  }
  if ("afterDisease" %in% colnames(source_data)) {
    source_data$postDisease <- source_data$afterDisease
  }
  if ("afterDrugs" %in% colnames(source_data)) {
    source_data$postDrugs <- source_data$afterDrugs
  }
  if ("afterTreatmentProcedures" %in% colnames(source_data)) {
    source_data$postTreatmentProcedures <- source_data$afterTreatmentProcedures
  }
  if (!"alternativeDiagnoses" %in% colnames(source_data)) {
    source_data$alternativeDiagnoses <- source_data$alternativeDiagnosis
  }
  
  # 2. Process Demographic and Base Columns
  # Extract Age
  df_age <- source_data %>%
    select(generatedId, conceptName = age) %>%
    mutate(
      startDay = 0, endDay = 0,
      conceptId = NA_integer_,
      conceptName = as.character(conceptName),
      category = "age", target = "Disease of interest", extraData = NA_character_
    )
  
  # Extract Sex
  df_sex <- source_data %>%
    select(generatedId, conceptName = sex) %>%
    mutate(
      startDay = 0, endDay = 0,
      conceptId = case_when(
        conceptName == "MALE" ~ 8507,
        conceptName == "FEMALE" ~ 8532,
        TRUE ~ 0
      ),
      category = "sex", target = "Disease of interest", extraData = NA_character_
    )
  
  # Extract Observation Period
  df_obs <- source_data %>%
    select(generatedId, observationPeriod) %>%
    mutate(
      startDay = as.numeric(str_extract(observationPeriod, "^-?\\d+")),
      endDay = as.numeric(str_extract(gsub(" days$", "", observationPeriod), "\\d+$")),
      conceptId = NA_integer_,
      conceptName = "Observation period",
      category = "observationPeriod", target = "Disease of interest", extraData = NA_character_
    ) %>%
    select(-observationPeriod)
  
  # Extract Race
  df_race <- source_data %>%
    select(generatedId, conceptName = race) %>%
    mutate(
      startDay = 0, endDay = 0,
      conceptId = ifelse(!is.na(conceptName) & conceptName == "White", 8527, 0),
      category = "race", target = "Disease of interest", extraData = NA_character_
    )
  
  # Extract Ethnicity
  df_eth <- source_data %>%
    select(generatedId, conceptName = ethnicity) %>%
    mutate(
      startDay = 0, endDay = 0,
      conceptId = ifelse(!is.na(conceptName) & conceptName == "Not Hispanic or Latino", 38003564, 0),
      category = "ethnicity", target = "Disease of interest", extraData = NA_character_
    )
  
  # Extract Visit
  df_visit <- source_data %>%
    select(generatedId, conceptName = visits) %>%
    mutate(
      startDay = 0, 
      endDay = if_else(grepl("days", conceptName), as.numeric(gsub(" days", "", str_extract(conceptName, "[0-9]+ days"))), 0),
      conceptId = NA_real_,
      conceptName = gsub(" \\([0-9]+ days\\)", "", gsub("->", " followed by ", conceptName)),
      category = "visits", 
      target = "Disease of interest",
      extraData = ""
    )
  
  # 3. Process Event Columns (presentation, visits, priorDrugs, etc.)
  event_cols <- c("presentation", "symptoms", "priorDisease", "postDisease", 
                  "priorDrugs", "postDrugs", "priorTreatmentProcedures", 
                  "postTreatmentProcedures", "alternativeDiagnoses", "diagnosticProcedures")
  
  for (col in event_cols) {
    source_data[[col]] <- as.character(source_data[[col]])
  }
  
  df_events <- source_data %>%
    select(generatedId, any_of(event_cols)) %>%
    pivot_longer(
      cols = -generatedId,
      names_to = "category",
      values_to = "raw_value",
      values_drop_na = TRUE
    ) %>%
    # Split multiple events separated by semicolons
    separate_rows(raw_value, sep = "; ") %>%
    # Extract the concept name and the data inside parentheses
    mutate(
      conceptName = str_trim(str_replace(raw_value, "\\(.*\\);?$", "")),
      parenthesis_data = str_extract(raw_value, "(?<=\\().*(?=\\))")
    ) %>%
    mutate(
      # By default, days are NA for descriptive extraData
      startDay = NA_real_,
      endDay = NA_real_,
      extraData = NA_character_,
      conceptId = NA_integer_,
      target = "Disease of interest"
    )
  
  # Attempt to parse 'startDay' and 'endDay' from parenthesis if it contains "day"
  # Otherwise, treat the content inside parenthesis as `extraData`
  df_events_2 <- list()
  for (i in seq_len(nrow(df_events))) {
    row <- df_events[i, ]
    if (is.na(row$parenthesis_data)) {
      startDays <- 0
      endDays <- 0
      extraData <- row$parenthesis_data      
    } else if (str_detect(row$parenthesis_data, "for")) {
      startDays <- as.numeric(str_extract(row$parenthesis_data, "-?\\d+"))
      endDays <- startDays + as.numeric(gsub("for ", "", str_extract(row$parenthesis_data, "for \\d+")))
      extraData <- ""
    } else if (str_detect(row$parenthesis_data, "day")) {
      startDays <- as.numeric(str_extract_all(row$parenthesis_data, "-?\\d+", simplify = TRUE))
      endDays <- startDays
      extraData <- ""
    } else {
      startDays <- 0
      endDays <- 0
      extraData <- row$parenthesis_data
    }
    newRow <- tibble(
      generatedId = row$generatedId,
      startDay = startDays,
      endDay = endDays,
      conceptId = row$conceptId,
      conceptName = row$conceptName,
      category = row$category,
      target = row$target,
      extraData = extraData
    )
    df_events_2[[i]] <- newRow
  }
  df_events_2 <- bind_rows(df_events_2)
  
  df_concept_prevalence <- source_data %>%
    select(generatedId) %>%
    mutate(
      startDay = 0, endDay = 0,
      conceptId = NA_integer_,
      conceptName = "0.01",
      category = "cohortPrevalence",
      target = "Disease of interest", 
      extraData = NA_character_
    )
  
  # 4. Combine all the processed datasets together
  target_data <- bind_rows(df_age, df_sex, df_obs, df_race, df_eth, df_visit, df_events_2, df_concept_prevalence) %>%
    # Arrange to match final layout schema
    select(generatedId, startDay, endDay, conceptId, conceptName, category, target, extraData) %>%
    # Sort by ID (optional, to keep records of the same patient together)
    arrange(generatedId, category)
  
  # 5. Export to target CSV
  return(target_data)
}
