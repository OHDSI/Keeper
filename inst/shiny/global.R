library(dplyr)
library(readr)
library(shiny)
library(bslib)
library(pool)
library(plotly)


# .shinyArgs <- list(
#   keeper = readRDS("../shuffledKeeper.rds"),
#   conceptSets = readr::read_csv("../t1dmConceptSets.csv"),
#   decisionsFileName = "e:/temp/Decisions.csv"
# )
# unlink("e:/temp/Decisions.csv")

if (exists(".shinyArgs", envir = .GlobalEnv)) {
  mode <- "local"
  adjudicators <- NULL
} else if (Sys.getenv("KEEPER_SERVER") != "") {
  mode <- "server"
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
  
  sql = "
      SELECT DISTINCT adjudicator 
      FROM @database_schema.adjudications 
      ORDER BY adjudicator;
    "
  adjudicators <- DatabaseConnector::renderTranslateQuerySql(
    connection = connectionPool,
    sql = sql,
    database_schema = databaseSchema
  ) |>
    pull(adjudicator)
} else {
  stop("No arguments provided, and no server details found")
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
             certainty = NA,
             indexDay = 0) |>
      as.data.frame()
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

getDataList <- function(adjudicator = NULL) {
  if (mode == "local") {
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
  } else if (mode == "server") {
    writeLines(paste("Loading KEEPER data from database server for user", adjudicator))
    
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
      ORDER BY sort_order;
    "
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
        certainty = NA,
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
  }
  
  return(list(keeper = keeper,
              decisions = decisions,
              conceptSets = conceptSets,
              nProfiles = nrow(decisions$decisionsDataFrame),
              hasPersonIds = "personId" %in% keeper$category))
}
