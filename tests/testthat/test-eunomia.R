library(Keeper)
library(testthat)
library(Eunomia)

connectionDetails <- getEunomiaConnectionDetails()
createCohorts(connectionDetails)

test_that("Run Keeper on Eunomia", {
  keeper <- createKeeper(
    connectionDetails = connectionDetails,
    databaseId = "Synpuf",
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    cohortDefinitionId = 3,
    cohortName = "GI Bleed",
    sampleSize = 100,
    assignNewId = TRUE,
    useAncestor = TRUE,
    doi = c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 375545, 29555009, 4209145, 4034964, 380834, 4299544, 4226354, 4159742, 43530690, 433736,
            320128, 4170226, 40443308, 441267, 4163735, 192963, 85828009),
    symptoms = c(4232487, 4229881),
    comorbidities = c(432867, 436670),
    drugs = c(1730370, 21604490, 21601682, 21601855, 21601462, 21600280, 21602728, 1366773, 21602689, 21603923, 21603746),
    diagnosticProcedures = c(40756884, 4143852, 2746768, 2746766),
    measurements	= c(3034962, 3000483, 3034962, 3000483, 3004501, 3033408, 3005131, 3024629, 3031266, 3037110, 3009261, 3022548, 3019210, 3025232, 3033819,
                     3000845, 3002666, 3004077, 3026300, 3014737, 3027198, 3025398, 3010300, 3020399, 3007332, 3025673, 3027457, 3010084, 3004410, 3005673),
    alternativeDiagnosis = c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 375545, 29555009, 4209145, 4034964, 380834, 4299544, 4226354, 4159742, 43530690, 433736,
                             320128, 4170226, 40443308, 441267, 4163735, 192963, 85828009),
    treatmentProcedures = c(0),
    complications =  c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 375545, 29555009, 4209145, 4034964,
                       380834, 4299544, 4226354, 4159742, 43530690, 433736, 320128, 4170226, 40443308, 441267, 4163735, 192963, 85828009)      
  )
  expect_s3_class(keeper, "data.frame")
  settings <- createPromptSettings(provideExamples = FALSE)
  systempPrompt <- createSystemPrompt(settings, "GI bleed")  
  prompt <- createPrompt(settings, "GI bleed", keeper[1, ])
  expect_type(systempPrompt, "character")
  expect_type(prompt, "character")
})


