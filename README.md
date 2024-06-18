Knowledge-Enhanced Electronic Profile Review (KEEPER)
=====================================================

[![Build Status](https://github.com/OHDSI/Keeper/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/Keeper/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/Keeper/coverage.svg?branch=main)](https://app.codecov.io/github/OHDSI/Keeper?branch=mai)

KEEPER is part of [HADES](https://ohdsi.github.io/Hades/).

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
  doi = c(435216, 201254),
  symptoms = c(79936, 432454, 4232487, 4229881, 254761),
  comorbidities = c(141253, 432867, 436670, 433736, 255848),
  drugs = c(21600712, 21602728, 21603531),
  diagnosticProcedures = c(0),
  measurements	= c(3004410,3005131,3005673,3010084,3033819,4149519,4229110, 4020120),
  alternativeDiagnosis = c(192963,201826,441267,40443308),
  treatmentProcedures = c(4242748),
  complications =  c(201820,375545,380834,433968,442793,4016045,4209145,4299544)                             
)
```


Technology
==========

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
* Vignette: [Setting parameters for Keeper](https://raw.githubusercontent.com/OHDSI/Keeper/main/inst/doc/SettingKeeperParameters.pdf)
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

Ready for testing. Interface may still change in future versions.


Acknowledgements
================

- None
