library(ggplot2)
library(dplyr)
library(plotly)

# keeper <- readRDS("/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/KeeperAf.rds")
# generatedIds <- unique(keeper$generatedId)
# subset <- keeper |>
#   filter(generatedId == generatedIds[3])


generateLabelForPlot <- function(conceptName, startDay, endDay, extraData, keeperTable) {
  conceptName <- if_else(nchar(conceptName) > 63, paste0(substr(conceptName, 1, 60), "..."), conceptName)
  if (startDay[1] < -90 | startDay[1] > 90) {
    return(paste(generateLabel(conceptName, startDay, endDay, extraData, keeperTable), collapse = "\n"))
  }
  
  if (keeperTable == "presentation") {
    labels <- paste0(conceptName, if_else(extraData == "",  "", sprintf(" (%s)", extraData)))
  } else if (keeperTable == "visitContext") {
    labels <- paste0(conceptName, if_else(startDay == endDay, "", sprintf(" (%d days)", endDay - startDay)))
  } else if (keeperTable %in% c("priorDrugs", "postDrugs")) {
    labels <- sprintf("%s (%s)", 
                      conceptName,
                      paste(sprintf("for %d day%s", 
                                    endDay - startDay + 1,
                                    if_else(endDay == startDay, "", "s")),
                            collapse = ", "))
  } else if (keeperTable == "measurements") {
    labels <- sprintf("%s%s", 
                      conceptName,
                      if_else(extraData == "",
                              "",
                              sprintf(" (with value %s)", extraData)))
  } else {
    labels <- conceptName[1]
  }
  label <- sprintf("<b>Day %s</b>\n%s", startDay[1], 
                   paste(labels, collapse = "\n"))
  return(label)
}


plotTimeline <- function(subset) {
  
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
  ) |>
    mutate(xmin = pmin(-5, pmax(minDay, -100)),
           xmax = pmax(5, pmin(maxDay, 100)),
           ymin = sortOrder - 1,
           ymax = sortOrder)
  
  vizData <- subset |>
    filter(category %in% keeperTables)  |>
    distinct() |>
    mutate(visualGroup = gsub("Prior |Post ", "", prettifyName(category)),
           startDay = if_else(is.na(startDay), 0, startDay)) |>
    mutate(x = if_else(startDay > 90,
                       100,
                       if_else(startDay < -90, 
                               -100, 
                               startDay)),
           xend = if_else(startDay < -90,
                          -100,
                          if_else(endDay > 90, 100, endDay))) |>
    group_by(visualGroup, target, x) |>
    arrange(conceptName, startDay) |>
    summarise(nData = n_distinct(conceptName), 
              text = generateLabelForPlot(conceptName,
                                          startDay, 
                                          endDay, 
                                          extraData, 
                                          category[1]),
              xend = max(xend),
              .groups = "drop") |>
    inner_join(vizGroups, by = join_by(visualGroup)) |>
    mutate(y = case_when(
      .data$target == "Disease of interest" ~ sortOrder - 0.2,
      .data$target == "Both" ~ sortOrder - 0.4,
      .data$target == "Alternative diagnoses" ~ sortOrder - 0.6,
      TRUE ~ sortOrder - 0.8))
  
  breaks <- c(-100, -90, -60, -30, 0, 30, 60, 90, 100)
  labels <- c("<-90","-90", "-60", "-30", "0", "30", "60", "90", ">90")
  plot <- ggplot(vizGroups, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    geom_rect(fill = "white") +
    geom_hline(aes(yintercept = ymin), color = "gray") +
    geom_vline(xintercept = breaks, color = "gray") +
    suppressWarnings(geom_point(aes(x = x, y = y, text = text, color = target), data = vizData)) + 
    geom_segment(aes(x = x, xend = xend, y = y, yend = y, color = target), data = vizData) +
    scale_color_manual(values = c("Disease of interest" = "#11A08A",
                                  "Both" = "#000000",
                                  "Alternative diagnoses" = "#EB6622",
                                  "Other" = "#999999")) +
    scale_x_continuous("Day", breaks = breaks, labels = labels) +
    scale_y_continuous(breaks = (vizGroups$ymin + vizGroups$ymax)/2, labels = vizGroups$visualGroup) +
    theme(
      panel.background = element_rect(fill = "lightgray"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_line(color = "white"),
      axis.text = element_text(size = 10),
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.position = "none"
    )
  
  plotly <- ggplotly(plot, tooltip = "text") |>
    layout(hoverlabel = list(bgcolor = "#ffffff", 
                             align = "left",
                             font = list(size = 14)),
           # legend = list(
           #   title = list(text = ""), # Keeps the title blank
           #   orientation = "h",       # Makes the legend horizontal
           #   xanchor = "center",      # Centers the legend box horizontally
           #   x = 0.5,                 # Places it in the middle of the x-axis
           #   yanchor = "top",         # Anchors the legend to the top of its bounding box
           #   y = -0.1                 # Pushes it down below the x-axis
           # ),
           xaxis = list(fixedrange = TRUE,       # Disables zooming/panning on the x-axis
                        showspikes = TRUE,       # Enables the hover line
                        spikemode = "across",    # Draws the line across the entire plot area
                        spikedash = "dash",      # Makes the line dashed
                        spikecolor = "black",    # Sets the color of the line
                        spikethickness = 1),     # Sets the thickness of the line), 
           yaxis = list(fixedrange = TRUE)) |>
    style(hoverinfo = "none", traces = 1:3) |>
    config(displayModeBar = FALSE)
  return(plotly)
}

