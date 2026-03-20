library(dplyr)
library(readr)
library(shiny)
library(bslib)
library(pool)
library(plotly)



# .shinyArgs <- list(
#   keeper = readRDS("e:/temp/KeeperMm.rds"),
#   conceptSets = readr::read_csv("e:/temp/mmConceptSets.csv"),
#   decisionsFileName = "e:/temp/Decisions.csv"
# )


if (Sys.getenv("KEEPER_SERVER") != "") {
  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "data")

  writeLines("Opening connection pool")
  connectionPool <- pool::dbPool(
    drv = DatabaseConnector::DatabaseConnectorDriver(),
    dbms = "postgresql",
    server = Sys.getenv("KEEPER_SERVER"),
    user = Sys.getenv("KEEPER_USER"),
    password = Sys.getenv("KEEPER_PASSWORD")
  )
  databaseSchema = Sys.getenv("KEEPER_DATABASE_SCHEMA")
  onStop(function() {
    if (DBI::dbIsValid(connectionPool)) {
      writeLines("Closing database pool")
      pool::poolClose(connectionPool)
    }
  })
}

loadDecisionsFromFile <- function(fileName, keeper) {
  if (file.exists(fileName)) {
    message("Loading existing decisions file")
    decisionsDataFrame <- read_csv(fileName, show_col_types = FALSE)
    decisionsDataFrame <- as.data.frame(decisionsDataFrame)
  } else {
    decisionsDataFrame <- keeper |>
      distinct(generatedId, databaseId, phenotype) |>
      mutate(decision = NA,
             indexDay = 0)
    write_csv(decisionsDataFrame, fileName)
  }
  return(decisionsDataFrame)
}

addDatabaseIdPhenotypeIfNeeded <- function(keeper) {
  if (!"databaseId" %in% colnames(keeper)) {
    keeper <- keeper |>
      inner_join(keeper |>
                   filter(category == "cdmSourceAbbreviation") |>
                   select(generatedId, databaseId = conceptName),
                 by = join_by(generatedId)) |>
      inner_join(keeper |>
                   filter(category == "phenotype") |>
                   select(generatedId, phenotype = conceptName),
                 by = join_by(generatedId))
  }
  return(keeper)
}

getDataList <- function(session) {
  if (exists(".shinyArgs", envir = .GlobalEnv)) {
    writeLines("Using user-provided Keeper data")
    args <- get(".shinyArgs", envir = .GlobalEnv)
    keeper <- args$keeper
    keeper <- addDatabaseIdPhenotypeIfNeeded(keeper)
    conceptSets <- args$conceptSets
    decisionsFileName <- args$decisionsFileName
    decisions <- list(
      type = "file",
      fileName = decisionsFileName,
      decisionsDataFrame = loadDecisionsFromFile(decisionsFileName, keeper)
    )
  } else if (Sys.getenv("KEEPER_SERVER") != "") {
    writeLines("Loading Keeper data from database server")
    
    if (is.null(session$user)) {
      writeLines("Could not detect user. Setting to default")
      adjudicator <- "TEST_USER"
    } else {
      adjudicator <- toupper(session$user)
    }
    sql <- "
      SELECT keeper.* 
      FROM @database_schema.adjudications 
      INNER JOIN @database_schema.keeper
        ON adjudications.database_id = keeper.database_id
          AND adjudications.phenotype = keeper.phenotype
          AND adjudications.generated_id = keeper.generated_id
      WHERE adjudicator = '@adjudicator';
    "
    keeper <- DatabaseConnector::renderTranslateQuerySql(
      connection = connectionPool,
      sql = sql,
      database_schema = databaseSchema,
      adjudicator = adjudicator,
      snakeCaseToCamelCase = TRUE
    ) |>
      as_tibble()
    sql <- "
      SELECT * 
    FROM @database_schema.adjudications 
    WHERE adjudicator = '@adjudicator'
    ORDER BY sort_order;"
    decisionsDataFrame <- DatabaseConnector::renderTranslateQuerySql(
      connection = connectionPool,
      sql = sql,
      database_schema = databaseSchema,
      adjudicator = adjudicator,
      snakeCaseToCamelCase = TRUE
    ) |>
      as.data.frame()
    if (nrow(decisionsDataFrame) == 0) {
      decisionsDataFrame <- tibble(
        generatedId = -1,
        databaseId = "NA",
        phenotype = "NA",
        decision = NA,
        indexDay = 0
      )
    }
    decisions <- list(
      type = "database",
      adjudicator = adjudicator,
      decisionsDataFrame = decisionsDataFrame
    )
    
    sql <- "SELECT * FROM @database_schema.concept_sets;"
    conceptSets <- DatabaseConnector::renderTranslateQuerySql(
      connection = connectionPool,
      sql = sql,
      database_schema = databaseSchema,
      snakeCaseToCamelCase = TRUE
    ) |>
      as_tibble()

  } else {
    writeLines("Loading Keeper data from data folder")
    # keeper <- readRDS("/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/KeeperMm - Copy.rds")
    # decisionsFileName <- "/Users/schuemie/Library/CloudStorage/OneDrive-JNJ/QuickShare/Decisions.csv"
    keeper <- readRDS("data/Keeper.rds")
    keeper <- addDatabaseIdPhenotypeIfNeeded(keeper)
    conceptSets <- readRDS("data/ConceptSets.rds")
    decisionsFileName <- "data/Decisions.csv"
    decisions <- list(
      type = "file",
      fileName = decisionsFileName,
      decisionsDataFrame = loadDecisionsFromFile(decisionsFileName, keeper)
    )
  }
  
  return(list(keeper = keeper,
              decisions = decisions,
              conceptSets = conceptSets,
              nProfiles = nrow(decisions$decisionsDataFrame),
              hasPersonIds = "personId" %in% keeper$category))
}
