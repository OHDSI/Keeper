# Run Keeper on a real database 

library(Keeper)

connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_mdcr.cdm_merative_mdcr_v3618"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable  <- "keeper_test"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

# Generate cohort --------------------------------------------------------------
library(PhenotypeLibrary)
library(CohortGenerator)
outcomeCohort <- getPlCohortDefinitionSet(77) # GI bleed

cohortTableNames <- getCohortTableNames(cohortTable = cohortTable)
createCohortTables(connectionDetails = connectionDetails,
                   cohortDatabaseSchema = cohortDatabaseSchema,
                   cohortTableNames = cohortTableNames)
generateCohortSet(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTableNames = cohortTableNames,
                  cohortDefinitionSet = outcomeCohort)

# Check number of subjects per cohort:
connection <- DatabaseConnector::connect(connectionDetails)
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id"
DatabaseConnector::renderTranslateQuerySql(connection, sql,  cohortDatabaseSchema = cohortDatabaseSchema, cohortTable = cohortTable)
DatabaseConnector::disconnect(connection)

# Run Keeper -------------------------------------------------------------------
keeper <- createKeeper(
  connectionDetails = connectionDetails,
  databaseId = "MDCR",
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  cohortDefinitionId = 77,
  cohortName = "GI Bleed",
  # tempEmulationSchema = "scratch.scratch_mschuemi",
  sampleSize = 100,
  assignNewId = TRUE,
  useAncestor = TRUE,
  doi = c(4202064, 192671, 2108878, 2108900, 2002608),
  symptoms = c(4103703, 443530, 4245614, 28779),
  comorbidities = c(81893, 201606, 313217, 318800, 432585, 4027663, 4180790, 4212540,
                    40481531, 42535737, 46271022), 
  drugs = c(904453, 906780, 923645, 929887, 948078, 953076, 961047, 985247, 992956, 
            997276, 1102917, 1113648, 1115008, 1118045, 1118084, 1124300, 1126128, 
            1136980, 1146810, 1150345, 1153928, 1177480, 1178663, 1185922, 1195492, 
            1236607, 1303425, 1313200, 1353766, 1507835, 1522957, 1721543, 1746940, 
            1777806, 19044727, 19119253, 36863425), 
  diagnosticProcedures = c(4087381, 4143985, 4294382, 42872565, 45888171, 46257627), 
  measurements	= c(3000905, 3000963, 3003458, 3012471, 3016251, 3018677, 3020416, 
                   3022217, 3023314, 3024929, 3034426), 
  alternativeDiagnosis = c(24966, 76725, 195562, 316457, 318800, 4096682), 
  treatmentProcedures = c(0), 
  complications =  c(132797, 196152, 439777, 4192647)      
)

