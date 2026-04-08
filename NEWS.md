Keeper 2.1.0
============

Changes:

1. Restricting concept classes for recommended drugs to ingredient or higher.

2. LLM review now clearly marks when the date of onset is unclear.

3. The observation period start and end days are now also stored in the output of `reviewCases()` when `removePii = FALSE`.

4. Added `washoutPeriod`, `type`, and '`stratifyByCertainty` arguments to the `computeCohortOperatingCharacteristics()` function.


Bug fixes:

1. Added missing 'phenotype' field to the metadata table in `uploadReferenceCohort()`.


Keeper 2.0.0
============

This is a complete re-implementation of Keeper. 

Changes:

1. Changed `createKeeper()` to `generateKeeper()`, and made the following changes:

    - The concept set input arguments have been grouped in a single `keeperConceptSets` argument. 
    
    - The output has changed from a human-readable table to a structured intermediary format.
    
    - Removed the 'comorbidities' category from both the input and the output, as this was ill-defined. 
    
    - Replaced `visitContext`, which was a visit spanning day 0, with `visit`, a list of visits in the 30 days prior to 30 days after.
    
    - The `databaseId` argument has been removed. Instead, the database name is extracted from the `cdm_source` table.
    
    - Computes the prevalence of the input cohort in the overall population and stores it in the output table for later computations.

2. Added the `convertKeeperToTable()` to convert the output of `generateKeeper()` to the original human-readable table format.

3. Added the `reviewCases()` function that uses large language models (LLMs) to review Keeper profiles. It Uses the [ellmer](https://ellmer.tidyverse.org/index.html) package to standardize communication with LLMs.

4. Added the `generateKeeperConceptSets()` function that uses LLMs, the OHDSI Vocabulary vector store, and Phoebe to suggest concept sets to be used as input for `generateKeeper()`.

5. Added a Shiny app for human review of Keeper profiles. This can be launched using the `launchReviewerApp()` function. Experts can also deploy the Shiny app on a Shiny server with PostgreSQL backend.

6. Added the `createSensitiveCohort()` function. This creates a cohort based on the Keeper input concept sets which should include everyone in the database who potentially has the phenotype. This cohort can be used to estimate sensitivity (in addition to positive predictive value) of a phenotype algorithm.

7. Added the `uploadReferenceCohort()` and `computeCohortOperatingCharacteristics()` to use a large reference cohort (created by annotating a large sample of a sensitive cohort using LLMs) to evaluate phenotype definitions.


Bugfixes:

1. Avoiding incorrect person IDs due to integer overflow by using strings instead.


Keeper 0.2.1
============

Bugfixes:

1. Fixes error related to temp tables on platforms that require temp table emulation (e.g. DataBricks).


Keeper 0.2.0
============

Changes:

1. Added vignette on selection of concept sets.


Keeper 0.1.0
============

Changes:

1. Change name from 'Keeper' to 'Keeper', and `createKeeper()` to `createKeeper()` for consistency with HADES.

2. Removed `vocabularyDatabaseSchema` argument of `createKeeper()`, which wasn't used, and the vocabulary should be in the CDM schema anyway.

3. Drop dependency on `stringr`.

4. Removed `exportFolder` argument from `createKeeper()`. The function now returns the Keeper output.

5. Keeper output column names now use camelCase in line with Hades recommendations.

6. Adding functions for converting Keeper output into prompts for large language models (LLMs), and for parsing the response of an LLM.
