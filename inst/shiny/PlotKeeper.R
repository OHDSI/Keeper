plotKeeper <- function() {
  library(ggplot2)
  library(dplyr)
  library(plotly)
  
  keeper <- readRDS("/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/KeeperMm - Copy.rds")
  
  prettifyName <- function(name){
    name <- gsub("([A-Z])", " \\1", name)
    name <- tolower(name)
    name <- gsub("([a-z])([0-9])", "\\1_\\2", name)
    name <- tolower(name)
    name <- gsub("\\b([a-z])", "\\U\\1", name, perl = TRUE)
    return(name)
  }
  
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
  
  subset <- keeper |>
    filter(generatedId == 2)
  
  
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
  
  vizGroups <- tibble(
    visualGroup = c("Presentation",
                    "Visit Context",
                    "Disease",
                    "Symptoms",
                    "Alternative Diagnoses",
                    "Diagnostic Procedures",
                    "Measurements",
                    "Drugs",
                    "Treatment Procedures",
                    "Death"),
    minDay = c(0, 
               0,
               -9999,
               -30,
               -90,
               -30,
               -30,
               -9999,
               -9999,
               0),
    maxDay = c(0, 
               0,
               9999,
               -1,
               90,
               30,
               30,
               9999,
               9999,
               9999),
    sortOrder = 10:1
  )
  
  vizData <- subset |>
    filter(category %in% keeperTables)  |>
    mutate(visualGroup = gsub("Prior |Post ", "", prettifyName(category)))
  
  groupSizes <- vizData |>
    group_by(visualGroup) |>
    summarise(nData = n_distinct(conceptName))
  
  vizGroups <- vizGroups |>
    left_join(groupSizes, by = join_by(visualGroup)) |>
    arrange(sortOrder) |>
    mutate(nData = if_else(is.na(nData), 0, nData)) |>
    mutate(height = pmax(40, nData * 8)) |>
    mutate(xmin = pmin(-5, pmax(minDay, -90)),
           xmax = pmax(5, pmin(maxDay, 90)),
           ymin = cumsum(height) - height,
           ymax = cumsum(height),
           spacing = height / (nData + 1))
  
  yGroups <- vizData |>
    mutate(
    sortOrder = case_when(
      .data$target == "Disease of interest" ~ 1,
      .data$target == "Alternative diagnoses" ~ 0,
      TRUE ~ -1)
    ) |>
    group_by(visualGroup, conceptName, sortOrder) |>
    summarise(.groups = "drop") |>
    group_by(visualGroup) |>
    arrange(sortOrder, conceptName) |>
    mutate(yOrder = row_number()) |>
    ungroup() |>
    select(visualGroup, conceptName, yOrder)
  
  vizData$text <- sapply(seq_len(nrow(vizData)), function(i) generateLabel(vizData$conceptName[i],
                                                                           vizData$startDay[i], 
                                                                           vizData$endDay[i], 
                                                                           vizData$extraData[i], 
                                                                           vizData$category[i]))
  
  vizData <- vizData |>
    inner_join(yGroups, by = join_by(visualGroup, conceptName)) |>
    inner_join(vizGroups, by = join_by(visualGroup)) |>
    mutate(x = if_else(startDay > 90,
                       100,
                       if_else(startDay < -90, 
                               -100, 
                               startDay)),
           xend = if_else(startDay < -90,
                          -100,
                          if_else(endDay > 90, 100, endDay)),
           y = ymin + yOrder * spacing)
    
  breaks <- c(-100, -90, -60, -30, 0, 30, 60, 90, 100)
  labels <- c("<-90","-90", "-60", "-30", "0", "30", "60", "90", ">90")
  plot <- ggplot(vizGroups, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    geom_rect(fill = "white") +
    geom_hline(aes(yintercept = ymin), color = "gray") +
    geom_vline(xintercept = breaks, color = "gray") +
    geom_text(aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = visualGroup)) +
    geom_point(aes(x = x, y = y, color = target, text = text), data = vizData) + 
    geom_segment(aes(x = x, xend = xend, y = y, yend = y, color = target), data = vizData) +
    scale_x_continuous("Day", breaks = breaks, labels = labels) +
    theme(
      panel.background = element_rect(fill = "lightgray"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  ggplotly(plot, tooltip = "text") |>
    layout(hoverlabel=list(bgcolor = "#ffffff"))
} 