Knowledge-Enhanced Electronic Profile Review (KEEPER)
=====================================================

[![Build Status](https://github.com/OHDSI/Keeper/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/Keeper/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/Keeper/coverage.svg?branch=main)](https://app.codecov.io/github/OHDSI/Keeper?branch=mai)


Introduction
============
An R package for reviewing patient profiles for phenotype validation. 


Features
========
- Extracts patient level data for a) a random sample of patients in a cohort or b) patients in a user-specified list and formats it according to the KEEPER principles. 
- Allows additional de-identification through replacing OMOP personId with a new random id.

Examples
========
```r
keeper <- createKeeper(
  connectionDetails = connectionDetails,
  databaseId = "Synpuf",
  cdmDatabaseSchema = "dbo",
  cohortDatabaseSchema = "results",
  cohortTable = "cohort",
  cohortDefinitionId = 1234,
  cohortName = "DM type I",
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
```


Technology
============

Keeper is an R package.


Installation
============

1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including Java.

2. Install `Keeper` from GitHub:

    ```r
    install.packages("remotes")
    remotes::install_github("ohdsi/Keeper")
    ```
How to use
==========
Note: should move this to vignette.

- instatiated cohort with patients of interest in COHORT table or in another table that has the same fields as COHORT;
- doi: string for disease of interest (ex.: diabetes type I). Hereon, assume a string of concept_ids;
- symptoms: symptoms of disease of interest or alternative/competing diagnoses (those that you want to see to be able to distinguish your doi from another close disease, ex.: polyuria, weight gain or loss, vision disturbances);
- comorbidities: relevant diseases that co-occur with doi or alternative/competing diagnoses (ex.: obesity, metabolic syndrom, pancreatic disorders, pregnancy);
- drugs: drugs, relevant to the disease of interest or those that can be used to treat alternative/competing diagnoses (ex.: insulin, oral glucose lowering drugs);
- diagnosticProcedures: relevant diagnostic procedures (ex.: ultrasound of pancreas);
- measurements: relevant lab tests (ex.: islet cell ab, HbA1C, glucose measurment in blood, insulin ab);
- alternativeDiagnosis: alternative/competing diagnoses (ex.: diabetes type 2, cystic fibrosis, gestational diabetes, renal failure, pancreonecrosis)
- treatmentProcedures: relevant treatment procedures (ex.: operative procedures on pancreas);
- complications: relevant complications (ex.: retinopathy, CKD).

*note: if no suitable concept_ids exists for an input string, input c(0)


Use useAncestor = TRUE to switch from verbatim string of concept_ids vs ancestors. In latter case, the app will take you concept_ids and include them along with their descendants.

Use sampleSize to specify desired numebr of patients to be selected.

Use assignNewId = TRUE to replace person_id with a new sequence.


Output contains the following information per patient:

- demographics (age, gender);
- visit_context: information about visits overlapping with the index date (day 0) formatted as the type of visit and its duration;
- observation_period: information about overlapping OBSERVATION_PERIOD formatted as days prior - days after the index date;
- presentation: all records in CONDITION_OCCURRENCE on day 0 with corrresponding type and status;
- comorbidities: records in CONDITION_ERA and OBSERVATION that were selected as co-comoribidites and risk factors within all time prior excluding day 0. The list does not inlcude symptoms, disease of interest and complications;
- symptoms: records in CONDITION_ERA that were selected as symptoms 30 days prior excluding day 0. The list does not include disease of interest and complications. If you want to see symptoms outside of this window, please place them in complications;
- prior_disease: records in CONDITION_ERA that were selected as disease of interest or complications all time prior excluding day 0;
- prior_drugs: records in DRUG_ERA that were selected as drugs of interest all time prior excluding day 0 formatted as day of era start and length of drug era;
- prior_treatment_procedures: records in PROCEDURE_OCCURRENCE that were selected as treatments of interest within all time prior excluding day 0;
- diagnostic_procedures: records in PROCEDURE_OCCURRENCE that were selected as diagnostic procedures within all time prior excluding day 0;
- measurements: records in MEASUREMENT that were selected as measurements (lab tests) of interest within 30 days before and 30 days after day 0 formatted as value and unit (if exisits) and assessment compared to the reference range provided in MEASUREMENT table (normal, abnormal high and abnormal low);
- alternative_diagnosis: records in CONDITION_ERA that were selected as alternative (competing) diagnosis withi 90 days before and 90 days after day 0. The list does not inlcude disease of interest;
- after_disease: same as prior_disease but after day 0;
- after_drugs: same as prior_drugs but after day 0;
- after_treatment_procedures: same as prior_treatment_procedures but after day 0;
- death: death record any time after day 0.


Technology
==========
KEEPER is an R package.


System Requirements
===================
Requires R (version 3.6.0 or higher). 


User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/Keeper/).

PDF versions of the documentation are also available:

* Package manual: [Keeper manual](https://raw.githubusercontent.com/OHDSI/DatabaseConnector/main/extras/Keeper.pdf) 


Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/Keeper/issues">GitHub issue tracker</a> for all bugs/issues/enhancements


Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.


License
=======

Keeper is licensed under Apache License 2.0. 


Development
===========

Keeper is being developed in R Studio.


### Development status

Under development.


Acknowledgements
================

- None

