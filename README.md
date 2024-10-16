Tutorial Demo 2024
========================

<img src="https://img.shields.io/badge/Study%20Status-Design%20Finalized-brightgreen.svg" alt="Study Status: Design Finalized">

- Analytics use case(s): **CohortMethod/SCCS/Characterization/Patient-Level Prediction**
- Study type: **Demo**
- Tags: **OHDSI Symposium 2024**
- Study lead: **Jenna Reps**
- Study lead forums tag: **-**
- Study start date: **-**
- Study end date: **-**
- Protocol: [**Protocol**](StudyProtocol.pdf)
- Publications: **-**
- Results explorer: **-**

A demonstration of how to use HADES open source tools for network studies performing charcterization, estimation and prediction.

# Code To Run

First open R studio and activate the renv in this repository to create a new folder with an R environment that is compatible with the study:

```r
# activate the renv in the githiub repository 


```
Now make sure to open the environment folder to activate the environment and copy the code below into R studio, fill in your database connection details and then run the code:

``` r
# Specify the location you want to save the results to:
studyRootFolder <- '/Users/jreps/Documents/TutorialDemo2024'

# Add your database connection information:
database <- 'mdcd'
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = keyring::key_get('dbms', 'all'),
  server = keyring::key_get('server', database),
  user = keyring::key_get('user', 'all'),
  password = keyring::key_get('pw', 'all'),
  port = keyring::key_get('port', 'all')#,
)
workDatabaseSchema <- keyring::key_get('cohortDatabaseSchema', 'all')
cdmDatabaseSchema <- keyring::key_get('cdmDatabaseSchema',  database)


# No need to edit below this line
analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  fileName = '<todo add>'
)

workFolder <- file.path(studyRootFolder, "work_folder")
resultsFolder <- file.path(studyRootFolder, "results_folder")
if (!dir.exists(studyRootFolder)) {
  dir.create(studyRootFolder, recursive = TRUE)
}

executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = workDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTableNames = CohortGenerator::getCohortTableNames(
    cohortTable = "tutorial_demo_2024"
    ),
  workFolder = workFolder,
  resultsFolder = resultsFolder
)

Strategus::execute(
  connectionDetails = connectionDetails,
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings
)

```
