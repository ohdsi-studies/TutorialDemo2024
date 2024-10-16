studyLocation <- '/Users/jreps/Documents/GitHub/TutorialDemo2024'

library(dplyr)
library(Strategus)

# Exposures-outcomes
outcomeOfInterestIds <- c(18713)
exposureOfInterestIds <- c(18716,18715)
indicationOfInterest <- c(14704)

baseUrl <- '<baseUrlAtlas>'
ROhdsiWebApi::authorizeWebApi(
  baseUrl = baseUrl,
  authMethod = 'windows',
  webApiUsername = keyring::key_get('webapi', 'username'),
  webApiPassword = keyring::key_get('webapi', 'password')
)

cohorts <- c(outcomeOfInterestIds,exposureOfInterestIds,indicationOfInterest)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohorts,
  generateStats = T
)

  firstTarget <- list(
    CohortGenerator::createLimitSubset(
      name = '',
      priorTime = 365,
      limitTo = "firstEver"
    ),
    CohortGenerator::createCohortSubset(
      name = ' ',
      cohortIds = indicationOfInterest,
      startWindow = CohortGenerator::createSubsetCohortWindow(
        startDay = -9999,
        endDay = 0,
        targetAnchor = 'cohortStart'
        ),
      cohortCombinationOperator = 'any',
      negate = F,
      endWindow = CohortGenerator::createSubsetCohortWindow(
        startDay = -9999,
        endDay = 9999,
        targetAnchor = 'cohortStart'
      )
      )
  )
  subsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "first occurrence with hypertension",
    definitionId = 1,
    subsetOperators = firstTarget
  )
  cohortDefinitionSet <- cohortDefinitionSet |>
    CohortGenerator::addCohortSubsetDefinition(
      cohortSubsetDefintion = subsetDef,
      targetCohortIds = exposureOfInterestIds
        )


  negativeControlOutcomeIds <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = 8358,
    baseUrl = baseUrl
  )

  negativeControlOutcomeIds <- unlist(
    lapply(negativeControlOutcomeIds$expression[[1]],
           function(x) x$concept$CONCEPT_ID)
  )

  ncoCohortSet <- data.frame(
    cohortId = 1000 + (1:length(negativeControlOutcomeIds)),
    cohortName = paste('neg control ', 1:length(negativeControlOutcomeIds)),
    outcomeConceptId = negativeControlOutcomeIds
  )

  # Module Settings ----------------------

  # Cohort Diagnostics -----------------
  cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
  cdModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
    runInclusionStatistics = TRUE,
    runIncludedSourceConcepts = TRUE,
    runOrphanConcepts = TRUE,
    runTimeSeries = FALSE,
    runVisitContext = TRUE,
    runBreakdownIndexEvents = TRUE,
    runIncidenceRate = TRUE,
    runCohortRelationship = TRUE,
    runTemporalCohortCharacterization = TRUE
  )

  # Cohort Generator -----------------
  cgModuleSettingsCreator <- CohortGeneratorModule$new()

  # Create the settings & validate them
  cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
  cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

  ncoCohortSharedResourceSpecifications <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSet, "first", TRUE)
  cgModuleSettingsCreator$validateNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSharedResourceSpecifications)

  cgModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications()

  # Characterization -------------------------------
  cModuleSettingsCreator <- CharacterizationModule$new()
  cModuleSpecifications <- cModuleSettingsCreator$createModuleSpecifications(
    targetIds = c(exposureOfInterestIds,indicationOfInterest),
    outcomeIds = outcomeOfInterestIds
  )

  # Cohort Incidence -----------------
  ciModuleSettingsCreator <- CohortIncidenceModule$new()
  targets <- list(
    CohortIncidence::createCohortRef(id = 18716, name = "Lisinopril"),
    CohortIncidence::createCohortRef(id = 18715, name = "Hydrochlorothiazide"),
    CohortIncidence::createCohortRef(id = 18716*1000+1, name = "Lisinopril with hypertension"),
    CohortIncidence::createCohortRef(id = 18715*1000+1, name = "Hydrochlorothiazide with hypertension"),
    CohortIncidence::createCohortRef(id = indicationOfInterest , name = "Hypertension")
    )
  outcomes <- list(CohortIncidence::createOutcomeDef(
    id = 1,
    name = "cardiac arrythmias",
    cohortId = 18713,
    cleanWindow = 9999)
    )

  tars <- list(
    # while exposed
    CohortIncidence::createTimeAtRiskDef(id = 1, startWith = "start", endWith = "end"),
    # fixed 1-year
    CohortIncidence::createTimeAtRiskDef(id = 2, startWith = "start", endWith = "start", endOffset = 365)
  )
  analysis1 <- CohortIncidence::createIncidenceAnalysis(
    targets = c(1, 2,3,4,5),
    outcomes = c(1),
    tars = c(1, 2)
  )

  irDesign <- CohortIncidence::createIncidenceDesign(
    targetDefs = targets,
    outcomeDefs = outcomes,
    tars = tars,
    analysisList = list(analysis1),
    strataSettings = CohortIncidence::createStrataSettings(
      byYear = TRUE,
      byGender = TRUE
    )
  )

  ciModuleSpecifications <- ciModuleSettingsCreator$createModuleSpecifications(
    irDesign = irDesign$toList()
  )

  # Cohort Method ----------------------
  cmModuleSettingsCreator <- CohortMethodModule$new()
  negativeControlOutcomes <- lapply(
    X = ncoCohortSet$cohortId,
    FUN = CohortMethod::createOutcome,
    outcomeOfInterest = FALSE,
    trueEffectSize = 1,
    priorOutcomeLookback = 30
  )

  outcomesOfInterest <- lapply(
    X = 18713,
    FUN = CohortMethod::createOutcome,
    outcomeOfInterest = TRUE
  )

  outcomes <- append(
    negativeControlOutcomes,
    outcomesOfInterest
  )

  tcos1 <- CohortMethod::createTargetComparatorOutcomes(
    targetId = 18716*1000+1,
    comparatorId = 18715*1000+1,
    outcomes = outcomes,
    excludedCovariateConceptIds = c(1308216,974166)
  )

  targetComparatorOutcomesList <- list(tcos1)

  covarSettings <- FeatureExtraction::createDefaultCovariateSettings(
    addDescendantsToExclude = TRUE
    )

  getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
    washoutPeriod = 183,
    firstExposureOnly = TRUE,
    removeDuplicateSubjects = "remove all",
    maxCohortSize = 100000,
    covariateSettings = covarSettings
  )

  # while exposed
  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    minDaysAtRisk = 1,
    riskWindowStart = 0,
    startAnchor = "cohort start",
    riskWindowEnd = 0,
    endAnchor = "cohort end"
  )

  matchOnPsArgs <- CohortMethod::createMatchOnPsArgs()
  fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
    modelType = "cox"
    )
  createPsArgs <- CohortMethod::createCreatePsArgs(
    stopOnError = FALSE,
    control = Cyclops::createControl(cvRepetitions = 1)
  )
  computeSharedCovBalArgs <- CohortMethod::createComputeCovariateBalanceArgs()
  computeCovBalArgs <- CohortMethod::createComputeCovariateBalanceArgs(
    covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
  )

  cmAnalysis1 <- CohortMethod::createCmAnalysis(
    analysisId = 1,
    description = "No matching, simple outcome model",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )

  cmAnalysis2 <- CohortMethod::createCmAnalysis(
    analysisId = 2,
    description = "Matching on ps and covariates, simple outcomeModel",
    getDbCohortMethodDataArgs = getDbCmDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    createPsArgs = createPsArgs,
    matchOnPsArgs = matchOnPsArgs,
    computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
    computeCovariateBalanceArgs = computeCovBalArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )

  cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

  analysesToExclude <- NULL

  cmModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
    cmAnalysisList = cmAnalysisList,
    targetComparatorOutcomesList = targetComparatorOutcomesList,
    analysesToExclude = analysesToExclude
  )

  # EvidenceSythesis ------------------
  esModuleSettingsCreator <- EvidenceSynthesisModule$new()
  evidenceSynthesisSourceCm <- esModuleSettingsCreator$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "adaptive grid"
  )
  metaAnalysisCm <- esModuleSettingsCreator$createBayesianMetaAnalysis(
    evidenceSynthesisAnalysisId = 1,
    alpha = 0.05,
    evidenceSynthesisDescription = "Bayesian random-effects alpha 0.05 - adaptive grid",
    evidenceSynthesisSource = evidenceSynthesisSourceCm
  )
  evidenceSynthesisSourceSccs <- esModuleSettingsCreator$createEvidenceSynthesisSource(
    sourceMethod = "SelfControlledCaseSeries",
    likelihoodApproximation = "adaptive grid"
  )
  metaAnalysisSccs <- esModuleSettingsCreator$createBayesianMetaAnalysis(
    evidenceSynthesisAnalysisId = 2,
    alpha = 0.05,
    evidenceSynthesisDescription = "Bayesian random-effects alpha 0.05 - adaptive grid",
    evidenceSynthesisSource = evidenceSynthesisSourceSccs
  )
  evidenceSynthesisAnalysisList <- list(metaAnalysisCm, metaAnalysisSccs)
  evidenceSynthesisAnalysisSpecifications <- esModuleSettingsCreator$createModuleSpecifications(
    evidenceSynthesisAnalysisList
  )

  # PatientLevelPrediction -------------------------------
  plpModuleSettingsCreator <- PatientLevelPredictionModule$new()
  makeModelDesignSettings <- function(targetId, outcomeId, popSettings, covarSettings) {
    invisible(PatientLevelPrediction::createModelDesign(
      targetId = targetId,
      outcomeId = outcomeId,
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
        sampleSize = 1000000
        ),
      populationSettings = popSettings,
      covariateSettings = covariateSettings,
      preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
      modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
      splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
      runCovariateSummary = T
    ))
  }

  # 1 year time at risk
  plpPopulationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
    startAnchor = "cohort start",
    riskWindowStart = 1,
    endAnchor = "cohort start",
    riskWindowEnd = 365,
    minTimeAtRisk = 1
  )

  plpCovarSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = T,
    useDemographicsAge = T,
    useConditionGroupEraLongTerm = T,
    useDrugGroupEraLongTerm = T
  )



  plpTargets <- c(18713*1000+1, indicationOfInterest)

  modelDesignList <- list()
  for (i in 1:length(plpTargets)) {
    for (j in 1:length(outcomeOfInterestIds)) {
      modelDesignList <- append(
        modelDesignList,
        list(
          makeModelDesignSettings(
            targetId = plpTargets[i],
            outcomeId = outcomeOfInterestIds[j],
            popSettings = plpPopulationSettings,
            covarSettings = plpCovarSettings
          )
        )
      )
    }
  }

  plpModuleSpecifications <- plpModuleSettingsCreator$createModuleSpecifications(
    modelDesignList = modelDesignList
  )

  # SelfControlledCaseSeries -------------------------------
  sccsModuleSettingsCreator <- SelfControlledCaseSeriesModule$new()

  exposuresOutcomeList <- list()
  for (exposureOfInterestId in exposureOfInterestIds) {
    for (outcomeOfInterestId in outcomeOfInterestIds) {
      exposuresOutcomeList[[length(exposuresOutcomeList) + 1]] <- SelfControlledCaseSeries::createExposuresOutcome(
        outcomeId = outcomeOfInterestId,
        exposures = list(SelfControlledCaseSeries::createExposure(exposureId = exposureOfInterestId)),
        nestingCohortId = indicationOfInterest
      )
    }
    for (negativeControlOutcomeId in negativeControlOutcomeIds) {
      exposuresOutcomeList[[length(exposuresOutcomeList) + 1]] <- SelfControlledCaseSeries::createExposuresOutcome(
        outcomeId = negativeControlOutcomeId,
        exposures = list(SelfControlledCaseSeries::createExposure(exposureId = exposureOfInterestId, trueEffectSize = 1))
      )
    }
  }

  getDbSccsDataArgs <- SelfControlledCaseSeries::createGetDbSccsDataArgs(
    studyStartDate = "",
    studyEndDate = "",
    maxCasesPerOutcome = 1e6,
    useNestingCohort = TRUE,
    nestingCohortId = indicationOfInterest,
    deleteCovariatesSmallCount = 0
  )

  createStudyPopulation6AndOlderArgs <- SelfControlledCaseSeries::createCreateStudyPopulationArgs(
    naivePeriod = 365
  )

  covarPreExp <- SelfControlledCaseSeries::createEraCovariateSettings(
    label = "Pre-exposure",
    includeEraIds = "exposureId",
    start = -30,
    end = -1,
    endAnchor = "era start"
  )

  covarExposureOfInt <- SelfControlledCaseSeries::createEraCovariateSettings(
    label = "Main",
    includeEraIds = "exposureId",
    start = 0,
    startAnchor = "era start",
    end = 0,
    endAnchor = "era end",
    profileLikelihood = TRUE,
    exposureOfInterest = TRUE
  )

  calendarTimeSettings <- SelfControlledCaseSeries::createCalendarTimeCovariateSettings(
    calendarTimeKnots = 5,
    allowRegularization = TRUE,
    computeConfidenceIntervals = FALSE
  )

  seasonalitySettings <- SelfControlledCaseSeries::createSeasonalityCovariateSettings(
    seasonKnots = 5,
    allowRegularization = TRUE,
    computeConfidenceIntervals = FALSE
  )

  createSccsIntervalDataArgs <- SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
    eraCovariateSettings = list(covarPreExp, covarExposureOfInt),
    seasonalityCovariateSettings = seasonalitySettings,
    calendarTimeCovariateSettings = calendarTimeSettings,
    minCasesForTimeCovariates = 100000
  )

  fitSccsModelArgs <- SelfControlledCaseSeries::createFitSccsModelArgs(
    control = Cyclops::createControl(
      cvType = "auto",
      selectorType = "byPid",
      startingVariance = 0.1,
      seed = 1,
      resetCoefficients = TRUE,
      noiseLevel = "quiet"
    )
  )

  sccsAnalysis1 <- SelfControlledCaseSeries::createSccsAnalysis(
    analysisId = 1,
    description = "SCCS age 18-",
    getDbSccsDataArgs = getDbSccsDataArgs,
    createStudyPopulationArgs = createStudyPopulation6AndOlderArgs,
    createIntervalDataArgs = createSccsIntervalDataArgs,
    fitSccsModelArgs = fitSccsModelArgs
  )

  sccsAnalysisList <- list(sccsAnalysis1)

  sccsModuleSpecifications <- sccsModuleSettingsCreator$createModuleSpecifications(
    sccsAnalysisList = sccsAnalysisList,
    exposuresOutcomeList = exposuresOutcomeList,
    combineDataFetchAcrossOutcomes = FALSE
  )


  # Create analysis specifications ---------------
  analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
    addSharedResources(cohortSharedResourcesSpecifications) |>
    addSharedResources(ncoCohortSharedResourceSpecifications) |>
    addCharacterizationModuleSpecifications(cModuleSpecifications) |>
    addCohortDiagnosticsModuleSpecifications(cdModuleSpecifications) |>
    addCohortGeneratorModuleSpecifications(cgModuleSpecifications) |>
    addCohortIncidenceModuleSpecifications(ciModuleSpecifications) |>
    addCohortMethodeModuleSpecifications(cmModuleSpecifications) |>
    addEvidenceSynthesisModuleSpecifications(evidenceSynthesisAnalysisSpecifications) |>
    addSelfControlledCaseSeriesModuleSpecifications(sccsModuleSpecifications) |>
    addPatientLevelPredictionModuleSpecifications(plpModuleSpecifications)


  ParallelLogger::saveSettingsToJson(
    object = analysisSpecifications,
    fileName = file.path(studyLocation, 'analysisSpecifications.json')
      )
