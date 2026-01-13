library(Keeper)
library(dplyr)
library(tidyr)
library(ellmer)

# Settings -------------------------------------------------------------------------------------------------------------
# Nemotron 3 Nano runing on local LM Studio with original full prompt
client <- chat_openai_compatible(
  base_url = "http://localhost:1234/v1",
  credentials = function() "lm-studio",
  model = "nvidia/nemotron-3-nano"
)
promptSettings <- createPromptSettings()
cacheFolder = "cache"
resultsFile <- "extras/KeeperEvaluation/MetricsNemotron3NanoOldPrompt.csv"

# Load development set -------------------------------------------------------------------------------------------------
keeperFile <- "../keeperllmeval/KEEPER_results_all_redux.xlsx"
keeper <- openxlsx::read.xlsx(keeperFile) |>
  tibble() |>
  SqlRender::snakeCaseToCamelCaseNames() |>
  rename(cohortId = "cohortid", 
         cohortName = "cohortname",
         goldStandard = "adjudicated.case.status.(yes.=.true.positive,.no.=.false.positive,.i.don't.know.=.uncertain)")

# PersonID is interpreted as numeric by openxlsx, but number is too large, leading to loss of precision. 
# We therefore use a sequential number instead:
keeper <- keeper |>
  select(-"personid") |>
  mutate(personId = as.character(row_number())) 

# Run KEEPER LLM review ------------------------------------------------------------------------------------------------
groups <- keeper |>
  group_by(cohortName) |>
  group_split()

allResults <- list()
# group = groups[[1]]
for (group in groups) {
  message("Evaluating ", group$cohortName[1])
  result <- reviewCases(keeper = group,
                        settings = promptSettings,
                        diseaseName = group$cohortName[1],
                        client = client,
                        cacheFolder = cacheFolder)
  allResults[[length(allResults) + 1]] <- result |>
    mutate(diseaseName = group$cohortName[1])
}

allResults <- bind_rows(allResults)

# Compare to gold standard ---------------------------------------------------------------------------------------------
computeCohensKappa <- function(agreement) {
  return((agreement - 0.5) / 0.5)
}
overall <- allResults |>
  rename(system = "isCase") |>
  inner_join(keeper |>
               select("personId", "cohortName", "goldStandard"),
             by = join_by(personId)) |>
  mutate(goldStandard = tolower(goldStandard)) |>
  filter(!is.na(system), !grepl("know", goldStandard)) |>
  mutate(system = if_else(grepl("know", system), "no", system)) |>
  group_by(goldStandard, system) |>
  summarise(count = n(), .groups = "drop") |>
  mutate(type = case_when(
    goldStandard == "yes" & system == "yes" ~ "TP",
    goldStandard == "yes" & system == "no" ~ "FN",
    goldStandard == "no" & system == "no" ~ "TN",
    goldStandard == "no" & system == "yes" ~ "FP"
  )) |>
  select("count", "type") |>
  pivot_wider(names_from = type, values_from = count) |>
  mutate(
    sens = TP / (TP + FN),
    spec = TN / (TN + FP),
    ppv = TP / (TP + FP),
    npv = TN / (TN + FN),
    agree = (TP + TN) / (TP + FP + TN + FN),
    kappa = computeCohensKappa((TP + TN) / (TP + FP + TN + FN))
  )
readr::write_csv(overall, resultsFile)
overall

