library(Keeper)
library(dplyr)
library(tidyr)
library(ellmer)
library(openxlsx)

# Settings -------------------------------------------------------------------------------------------------------------
# Nemotron 3 Nano running on local LM Studio with original full prompt
client <- chat_openai_compatible(
  base_url = "http://localhost:1234/v1",
  credentials = function() "lm-studio",
  model = "nvidia/nemotron-3-nano"
)
promptSettings <- createPromptSettings(timingReminder = FALSE)
cacheFolder <- "cache"
resultsFile <- "extras/KeeperEvaluation/MetricsNemotron3NanoOldPrompt.xlsx"

# Nemotron 3 Nano running on local LM Studio with additional timing reminder, removing narrative request.
client <- chat_openai_compatible(
  base_url = "http://localhost:1234/v1",
  credentials = function() "lm-studio",
  model = "nvidia/nemotron-3-nano"
)
promptSettings <- createPromptSettings(writeNarrative = FALSE)
cacheFolder <- "cacheTiming"
resultsFile <- "extras/KeeperEvaluation/MetricsNemotron3NanoTiming.xlsx"

# GPT-o3 running on Azure with original full prompt
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
promptSettings <- createPromptSettings(timingReminder = FALSE)
cacheFolder <- "cache"
resultsFile <- "extras/KeeperEvaluation/MetricsO3OldPrompt.xlsx"

# GPT-o3 running on Azure with additional timing reminder, removing narrative request.
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
promptSettings <- createPromptSettings(writeNarrative = FALSE)
cacheFolder <- "cacheTiming"
resultsFile <- "extras/KeeperEvaluation/MetricsO3Timing.xlsx"

# GPT-o3 running on Azure with additional timing reminder.
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
promptSettings <- createPromptSettings()
cacheFolder <- "cacheTimingNarrative"
resultsFile <- "extras/KeeperEvaluation/MetricsO3TimingNarrative.xlsx"

# GPT-o3 running on Azure without narrative request
client <- chat_azure_openai(
  endpoint = gsub("/openai/deployments.*", "", keyring::key_get("genai_o3_endpoint")),
  api_version = "2024-12-01-preview",
  model = "o3",
  credentials = function() keyring::key_get("genai_api_gpt4_key")
)
promptSettings <- createPromptSettings(timingReminder = FALSE, writeNarrative = FALSE)
cacheFolder <- "cacheNoNarrative"
resultsFile <- "extras/KeeperEvaluation/MetricsO3NoNarrative.xlsx"

# Load development set -------------------------------------------------------------------------------------------------
keeperFile <- "../keeperllmeval/KEEPER_results_all_redux.xlsx"
keeper <- read.xlsx(keeperFile) |>
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
# group = groups[[5]]
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
perPersonId <- allResults |>
  rename(system = "isCase") |>
  inner_join(keeper |>
               select("personId", "goldStandard"),
             by = join_by(personId)) |>
  mutate(goldStandard = tolower(goldStandard)) |>
  filter(!is.na(system), !grepl("know", goldStandard)) |>
  mutate(system = if_else(grepl("know", system), "no", system)) |>
  mutate(type = case_when(
    goldStandard == "yes" & system == "yes" ~ "TP",
    goldStandard == "yes" & system == "no" ~ "FN",
    goldStandard == "no" & system == "no" ~ "TN",
    goldStandard == "no" & system == "yes" ~ "FP"
  )) |>
  select("personId", "diseaseName", "system", "goldStandard", "type")

overall <- perPersonId |>
  group_by(type) |>
  summarise(count = n(), .groups = "drop") |>
  pivot_wider(names_from = type, values_from = count) |>
  mutate(
    sens = TP / (TP + FN),
    spec = TN / (TN + FP),
    ppv = TP / (TP + FP),
    npv = TN / (TN + FN),
    agree = (TP + TN) / (TP + FP + TN + FN),
    kappa = computeCohensKappa((TP + TN) / (TP + FP + TN + FN))
  )

# Save to Excel file:
workbook <- createWorkbook()
addWorksheet(workbook, sheetName = "Overall")   
addWorksheet(workbook, sheetName = "Per person ID")  
writeData(workbook, sheet = "Overall", x = overall) 
writeData(workbook, sheet = "Per person ID", x = perPersonId) 
freezePane(workbook, sheet = "Per person ID", firstActiveRow = 2)
num2dec <- createStyle(numFmt = "0.00")  
addStyle(workbook,
         sheet = "Overall",
         style = num2dec,
         rows = c(2),
         cols = 5:10, 
         gridExpand = TRUE)  
saveWorkbook(workbook, file = resultsFile, overwrite = TRUE)

overall

