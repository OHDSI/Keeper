# Keeper

**KEEPER** = **K**nowledge-**E**nhanced **E**lectronic **P**rofile **R**eview.

An R package for reviewing patient profiles for phenotype (cohort definition) validation in OMOP CDM observational health data. Part of the [HADES](https://ohdsi.github.io/Hades/) project. Maintained by OHDSI (Anna Ostropolets, Martijn Schuemie). Apache License 2.0.

- Package: `Keeper` v2.2.0
- Website: https://ohdsi.github.io/Keeper/
- Repo: https://github.com/OHDSI/Keeper

## What it does

The package supports the full workflow of validating a cohort definition against a disease of interest:

1. **Concept set generation** — `generateKeeperConceptSets()` uses an LLM plus the OHDSI vocabulary (HeCate API) and Phoebe to auto-populate the input concept sets (disease of interest, symptoms, treatments, alternative diagnoses, hypernym, etc.) for a phenotype.
2. **Profile generation** — `generateKeeper()` extracts patient-level data for a cohort (random sample or user-specified list) and formats it into structured KEEPER profiles (demographics, presentation, visits, symptoms, prior/post disease, prior/post drugs, treatment procedures, alternative diagnoses, diagnostic procedures, measurements, death). `convertKeeperToTable()` reshapes long profiles into a wide table for manual review.
3. **Review** — profiles can be reviewed by humans via an interactive Shiny app (`launchReviewerApp()`) or by large language models (`reviewCases()`, via the `ellmer` package). LLM review returns `isCase`, `certainty`, `justification`, `indexDay`, plus metadata (`model`, `keeperVersion`, `cohortPrevalence`).
4. **Reference cohort & operating characteristics** — `createSensitiveAndSpecificCohorts()` builds a highly-sensitive reference cohort; `uploadReferenceCohort()` uploads LLM reviews as a reference cohort + metadata tables; `computeCohortOperatingCharacteristics()` computes sensitivity, specificity, PPV, NPV, AUC, and Cohen's kappa (with confidence bounds) of a cohort definition against that reference, optionally stratified by LLM certainty.

## Directory structure

```
Keeper/
├── DESCRIPTION, NAMESPACE, NEWS.md, README.md, _pkgdown.yml, .Rbuildignore
├── R/
│   ├── KEEPER.R              (package doc, imports)
│   ├── GenerateConceptSets.R (LLM + HeCate/Phoebe concept set generation)
│   ├── GenerateKeeper.R      (generateKeeper, convertKeeperToTable)
│   ├── LlmLoop.R             (reviewCases, LLM review loop + file-based caching)
│   ├── PromptCraft.R         (createPromptSettings, prompt construction)
│   ├── QueryLlm.R            (queryLlm wrapper with retries)
│   ├── ResponseParser.R      (parse LLM responses)
│   ├── SensitiveCohort.R     (sensitive cohort, reference cohort, operating characteristics)
│   └── Shiny.R               (launchReviewerApp)
├── man/                      (roxygen-generated .Rd files)
├── man-roxygen/              (roxygen templates: Connection, CdmDatabaseSchema, TempEmulationSchema, CohortTable)
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── setup.R           (multi-DB test server config)
│       ├── test-databases.R  (integration: run Keeper on real DBs)
│       ├── test-eunomia.R    (Eunomia in-memory CDM tests)
│       ├── test-generate-concept-sets.R
│       ├── test-llm-loop.R
│       └── test-sensitive-cohort.R
├── vignettes/
│   ├── GeneratingKeeper.Rmd
│   └── UsingKeeperWithLlms.Rmd
├── inst/
│   ├── sql/                  (Keeper.sql, CreateKeeperCohort.sql, CreateSensitiveCohort.sql, CreateSensCohortRatios.sql, ComputeCohortConfusion.sql)
│   ├── shiny/                (global.R, server.R, ui.R, functionsForShiny.R, PlotKeeper.R, www/)
│   ├── ConceptSetGenerationPrompts.yaml
│   ├── KeeperPrompt.txt, KeeperLegacyPrompt.txt
│   ├── gibConceptSets.csv, t1dmConceptSets.csv
│   ├── shuffledKeeper.rds, llmReviews.rds, metrics.rds   (sample data, accessed via system.file())
│   └── doc/                  (built vignette PDFs)
├── docs/                     (pkgdown site)
├── extras/                   (package manual PDF)
├── .github/workflows/        (R-CMD-check CI)
└── cache*/                   (local LLM response caches — dev artifacts, gitignored)
```

There is **no `data/` directory** — sample data ships in `inst/` (`.rds`, `.csv`) and is accessed via `system.file()`.

## Dependencies

- **Depends:** `DatabaseConnector (>= 7.0.0)`, `R (>= 4.1.0)`
- **Imports:** `checkmate`, `dplyr`, `SqlRender`, `english`, `stringr`, `jsonlite`, `httr`, `yaml`
- **Suggests:** `rmarkdown`, `testthat`, `knitr`, `withr`, `Eunomia`, `shiny`, `ellmer`, `bslib`, `shinyjs`, `readr`, `plotly`, `pool`, `R6`

## Architecture & conventions

- **Database access** goes through `DatabaseConnector` (a `connectionDetails` or an open `connection` object) and `SqlRender` for SQL rendering/translation. SQL lives in `inst/sql/` and is loaded with `SqlRender::loadRenderTranslateSql(sqlFilename = ..., packageName = "Keeper", ...)`.
- **Input validation** uses `checkmate` with an `assertCollection` pattern: build an `errorMessages` collection, run all `assert*` calls with `add = errorMessages`, then `checkmate::reportAssertions(errorMessages)`.
- **Connection handling** is consistent: accept `connectionDetails = NULL` and `connection = NULL`, error if both are NULL, connect/disconnect with `on.exit()` if a `connectionDetails` was given.
- **Temp tables** use the `#` prefix and rely on `tempEmulationSchema` (default `getOption("sqlRenderTempEmulationSchema")`); call `DatabaseConnector::assertTempEmulationSchemaSet()` early.
- **LLM review** (`reviewCases()`) uses a file-based cache so interrupted runs can resume; the active session's `cacheFolder`/`rdsPath` variables point at one of the `cache*/` directories.
- **Roxygen** with `markdown = TRUE`; `man-roxygen/` holds shared parameter templates. Regenerate docs with `roxygen2::roxygenise()`.

## Testing

- `testthat`-based. `tests/testthat/setup.R` configures a multi-database test server.
- `test-eunomia.R` runs against the in-memory Eunomia CDM (no external DB needed); `test-databases.R` is a heavier integration test against real databases.
- LLM-dependent tests (`test-llm-loop.R`, `test-generate-concept-sets.R`) rely on cached responses in the `cache*/` directories rather than live API calls.

## Build & tooling

- Developed in RStudio (`KEEPER.Rproj`).
- CI: `.github/workflows/` runs R-CMD-check.
- Docs: pkgdown site in `docs/` (`_pkgdown.yml`); package manual in `extras/`.
- `deploy.sh` and `compare_versions` are helper scripts at the repo root.

## Notes for contributors

- `createSensitiveCohort()` is **deprecated** — it delegates to `createSensitiveAndSpecificCohorts()`. Use the latter in new code.
- The interface is still evolving ("Ready for testing. Interface may still change in future versions." per README).
- The `cache*/` directories are local development artifacts and are gitignored — do not commit them.
