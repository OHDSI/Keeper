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

- Supports review of patient profiles by humans through an interactive Shiny app.

- Supports review of patient profiles by large language models.


Examples
========

```r
keeperConceptSets <- generateKeeperConceptSets(
  phenotype = "Gastrointestinal bleeding",
  client = ellmer::chat_openai_compatible(),
  vocabConnectionDetails = connectionDetails,
  vocabDatabaseSchema = "cdm"
)

keeper <- generateKeeper(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "cdm",
  cohortDatabaseSchema = "results",
  cohortTable = "cohort",
  cohortDefinitionId = 1234,
  sampleSize = 100,
  removePii = TRUE,
  phenotypeName = "Gastrointestinal bleeding",
  keeperConceptSets = keeperConceptSets
)

keeperTable <- convertKeeperToTable(keeper)
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
Requires R (version 4.1.0 or higher). 


User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/Keeper/).

PDF versions of the documentation are also available:

* Vignette: [Generating KEEPER profiles for review](https://raw.githubusercontent.com/OHDSI/Keeper/main/inst/doc/GeneratingKeeper.pdf)
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

Ready for testing. Interface may still change in future versions.


Acknowledgements
================

- None
