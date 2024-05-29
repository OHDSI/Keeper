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
  doi = c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 375545, 29555009, 
          4209145, 4034964, 380834, 4299544, 4226354, 4159742, 43530690, 433736, 320128, 
          4170226, 40443308, 441267, 4163735, 192963, 85828009),
  symptoms = c(4232487, 4229881),
  comorbidities = c(432867, 436670),
  drugs = c(1730370, 21604490, 21601682, 21601855, 21601462, 21600280, 21602728, 1366773, 
            21602689, 21603923, 21603746),
  diagnosticProcedures = c(40756884, 4143852, 2746768, 2746766),
  measurements	= c(3034962, 3000483, 3034962, 3000483, 3004501, 3033408, 3005131, 3024629, 
                    3031266, 3037110, 3009261, 3022548, 3019210, 3025232, 3033819, 3000845, 
                    3002666, 3004077, 3026300, 3014737, 3027198, 3025398, 3010300, 3020399, 
                    3007332, 3025673, 3027457, 3010084, 3004410, 3005673),
  alternativeDiagnosis = c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 
                           375545, 29555009, 4209145, 4034964, 380834, 4299544, 4226354, 
                           4159742, 43530690, 433736,320128, 4170226, 40443308, 441267, 
                           4163735, 192963, 85828009),
  treatmentProcedures = c(0),
  complications =  c(201820,442793,443238,4016045,4065354,45757392, 4051114, 433968, 375545, 
                     29555009, 4209145, 4034964, 380834, 4299544, 4226354, 4159742, 43530690, 
                     433736, 320128, 4170226, 40443308, 441267, 4163735, 192963, 85828009)                             
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

* Vignette: [Using Keeper with Large Language Models](https://raw.githubusercontent.com/OHDSI/Keeper/main/inst/doc/UsingKeeperWithLlms.pdf)
* Package manual: [Keeper manual](https://raw.githubusercontent.com/OHDSI/Keeper/main/extras/Keeper.pdf) 


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

