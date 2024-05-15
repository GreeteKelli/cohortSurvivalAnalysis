################################################################################
# The following code if from Cohort2Trajectory repository with minimal changes
#
# Name: Cohort2Trajectory
# Author: Haug, M.
# Version: 1.1.3
# Source: https://github.com/HealthInformaticsUT/Cohort2Trajectory
################################################################################

################################################################################
#
# Run the trajectory creator
#
################################################################################

#' This function creates patient treatment trajectories
#'
#' @param connection Connection to database
#' @param dbms The type of DBMS running on the server. Valid values are:'oracle','postgresql','redshift','sql server','pdw','netezza','bigquery','sqlite', 'sqlite extended','spark'
#' @param cdmSchema Schema which contains the OHDSI Common Data Model.
#' @param cdmVocabSchema Schema which contains the OHDSI Common Data Modelvocabulary tables.
#' @param cdmTmpSchema Schema for temporary tables, will be deleted.
#' @param cdmResultsSchema Schema which has the information about thecohorts created in Atlas
#' @param stateCohortLabels Vector of the customized labels of the statecohorts
#' @param stateCohortPriorityOrder Vector of the customized labels of thestate cohorts in priority order
#' @param stateCohortMandatory Vector of the customized labels of thestate cohorts which are mandatory in trajectory
#' @param stateCohortAbsorbing Vector of the customized labels of thestate cohorts which are absorbing
#' @param stateSelectionType The type of state selection (1 - Firstoccurring, 2 - Max overlap, 3 - Priority)
#' @param oocFix The method to use for replacing "OUT OF COHORT" stateswith more relevant states
#' @param trajectoryType The type of the trajectory (0 - Discrete time, 1- Continuous time)
#' @param lengthOfStay The length of stay (days) in one state (Effect onlyin discrete case)
#' @param outOfCohortAllowed boolean whether the patient trajectory cansurpass the target cohort's observation-period
#' @param useCDM The package can also be run without the OMOP CDM
#' @param pathToData When using without OMOP CDM specify the path to datafile
#' @param allowedStatesList A list object which indicates accessiblestates from said state
#' @param mergeStates Boolean, if you want to merge states when theyoverlap
#' @param mergeThreshold Value from 0 to 1. If mergeStates is TRUE thestates will be label-merged given they overlap more than the specifiedthreshold. Can be given as vector, then multiple iterations are runned,
#' @param runGeneration TRUE/FALSE if FALSE no data tranfromation will berun, only the imported dataset will be cleaned
#' @example man/examples/Cohort2Trajectory.R
#'
#' @export
Cohort2Trajectory_modified <- function(dbms = "postgresql",
                                       connection = NULL,
                                       cdmSchema = "ohdsi_cdm",
                                       cdmVocabSchema = "ohdsi_vocab",
                                       cdmTmpSchema = "ohdsi_temp",
                                       cdmResultsSchema = "ohdsi_results",
                                       studyName = "Cohort2Trajectory",
                                       baseUrl =
                                         "http://localhost:8080/WebAPI",
                                       stateCohortLabels = NULL,
                                       stateCohortPriorityOrder = NULL,
                                       stateCohortMandatory = NULL,
                                       stateCohortAbsorbing = NULL,
                                       stateSelectionType = NULL,
                                       oocFix = "None",
                                       trajectoryType = NULL,
                                       lengthOfStay = NULL,
                                       outOfCohortAllowed = NULL,
                                       runSavedStudy = FALSE,
                                       pathToResults = getwd(),
                                       useCDM = TRUE,
                                       pathToData =
                                         './tmp/datasets/importedData.csv',
                                       allowedStatesList =
                                         createStateList(stateCohortLabels),
                                       mergeStates = FALSE,
                                       mergeThreshold = 0.5,
                                       runGeneration = TRUE) {
  ###############################################################################
  #
  # Creating mandatory directories if they do not exist
  #
  ###############################################################################

  createMandatorySubDirs(pathToResults)


  ##############################################################################
  #
  # Creating global variables
  #
  ##############################################################################

  dbms <<- dbms
  conn <<- connection
  cdmSchema <<- cdmSchema
  cdmTmpSchema <<- cdmTmpSchema
  cdmResultsSchema <<- cdmResultsSchema


  ##############################################################################

  data <- NULL

  # running already defined study
  if (runSavedStudy) {
    cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
    # loading settings

    # TODO NO settings just alternating name of the disease
    settings <- loadSettings(studyName)
    # stateNamesJSON <- c("0", studyName, "absorbingState")
    stateNamesJSON <- c(studyName)
    insertedJSONs <- c(settings$insertedJSONs)
    for (i in 1:length(stateNamesJSON)) {
      cohortJson <- insertedJSONs[i]
      cohortName <- stateNamesJSON[i]
      # creating cohorts

      cohortExpression <-
        CirceR::cohortExpressionFromJson(cohortJson)
      cohortSql <-
        CirceR::buildCohortQuery(cohortExpression,
                                 options =
                                   CirceR::createGenerateOptions(generateStats = FALSE))
      cohortsToCreate <-
        rbind(
          cohortsToCreate,
          data.frame(
            cohortId = i,
            cohortName = cohortName,
            sql = cohortSql,
            stringsAsFactors = FALSE
          )
        )
    }

    ############################################################################
    #
    # Generate the saved states in database
    #
    ############################################################################

    # Create the cohort tables to hold the cohort generation results
    cohortTableNames <-
      CohortGenerator::getCohortTableNames(cohortTable = studyName)

    # Drop. codesets if exists
    dropRelation(
      connection = conn,
      dbms = dbms,
      schema = '',
      relationName = 'codesets'
    )

    CohortGenerator::createCohortTables(
      connection = connection,
      cohortDatabaseSchema = cdmTmpSchema,
      cohortTableNames = cohortTableNames
    )

    # Generate the cohorts
    generateCohortSet(
      connection = connection,
      cdmDatabaseSchema = cdmSchema,
      cdmVocabSchema = cdmVocabSchema,
      cohortDatabaseSchema = cdmTmpSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = cohortsToCreate
    )

    if (studyName == "absorbingState") {
      return(TRUE)
    }

    sql <-
      loadRenderTranslateSql(
        dbms = dbms,
        "SELECT * FROM @cdmTmpSchema.@studyName UNION SELECT 0 as
cohort_definition_id, subject_id, cohort_start_date, cohort_end_date FROM
@cdmTmpSchema.@studyName UNION SELECT 99 as cohort_definition_id,
subject_id, cohort_start_date, cohort_end_date FROM @cdmTmpSchema.absorbingState
ORDER BY subject_id",
        cdmTmpSchema = cdmTmpSchema,
        studyName = studyName
      )


    ############################################################################
    #
    # Set the study parameters
    #
    ############################################################################

    stateCohortLabels <-
      c(settings$savedTrajectoryStates, "absorbingState")
    stateCohortPriorityOrder <-
      c('absorbingState', settings$savedPriorityOrder)
    stateCohortMandatory <- settings$savedMandatoryStates
    stateCohortAbsorbing <- settings$savedAbsorbingStates
    stateSelectionType <- settings$savedStateSelectionType
    oocFix <- settings$outOfCohortFix
    trajectoryType <-
      if (settings$savedTrajectoryType == "Discrete") {
        0
      }
    else {
      1
    }
    lengthOfStay <- settings$savedLengthOfStay
    outOfCohortAllowed <- settings$savedOutOfCohortAllowed

    data <- DatabaseConnector::querySql(connection, sql)
    # data <- dplyr::arrange(data, SUBJECT_ID, COHORT_START_DATE)
    # Apply state names
    data$COHORT_DEFINITION_ID <- plyr::mapvalues(
      x = data$COHORT_DEFINITION_ID,
      from = c(1:length(stateNamesJSON), 99),
      to = c(stateNamesJSON, "absorbingState"),
      warn_missing = FALSE
    )
    data <- dplyr::select(data,
                          SUBJECT_ID,
                          COHORT_DEFINITION_ID,
                          COHORT_START_DATE,
                          COHORT_END_DATE)

  }
  else if (useCDM) {
    ParallelLogger::logInfo("Importing data ...")
    data <- getCohortData(
      connection,
      dbms,
      resultsSchema = cdmResultsSchema,
      cdmSchema = cdmSchema,
      selectedTarget = atlasTargetCohort,
      selectedStates = atlasStateCohorts,
      baseUrl,
      pathToResults = pathToResults
    )
    # Change state labels
    data$COHORT_DEFINITION_ID <- plyr::mapvalues(
      x = data$COHORT_DEFINITION_ID,
      from = c("0", as.character(atlasStateCohorts)),
      to = c("0", stateCohortLabels),
      warn_missing = FALSE
    )

    if (!is.null(baseUrl)) {
      for (i in 1:length(stateCohortLabels)) {
        file.rename(
          paste(
            pathToResults,
            "/inst/JSON/",
            atlasStateCohorts[i],
            ".json",
            sep = ""
          ),
          paste(
            pathToResults,
            "/inst/JSON/",
            stateCohortLabels[i],
            ".json",
            sep = ""
          )
        )
        file.rename(
          paste(
            pathToResults,
            "/inst/SQL/",
            atlasStateCohorts[i],
            ".sql",
            sep = ""
          ),
          paste(
            pathToResults,
            "/inst/SQL/",
            stateCohortLabels[i],
            ".sql",
            sep = ""
          )
        )
      }
    }

    ParallelLogger::logInfo("Data import completed!")


  }
  else {
    ParallelLogger::logInfo("Importing data ...")
    data = readr::read_csv(pathToData)
    ParallelLogger::logInfo("Read complete!")
  }

  data <-
    dplyr::arrange(data,
                   SUBJECT_ID,
                   COHORT_START_DATE,
                   COHORT_END_DATE,
                   COHORT_DEFINITION_ID)

  if (nrow(data) == 0) {
    return(
      ParallelLogger::logInfo("There were no patients imported! Check your
target cohort!")
    )
  }

  # save_object(
  #   path =  paste(
  #     pathToResults,
  #     "/tmp/datasets/",
  #     studyName,
  #     "importedData.csv",
  #     sep = ""
  #   ),
  #   object = data
  # )

  ParallelLogger::logInfo("Cleaning data ...")

  data <- cleanCohortData(
    cohortData = data,
    mandatoryStates = stateCohortMandatory,
    outOfCohortAllowed = as.logical(outOfCohortAllowed),
    mergeStates = mergeStates,
    mergeThreshold = mergeThreshold
  )

  # As we may have new state labels (if mergeStates = TRUE) we now will modify some settings:
  if (mergeStates) {
    stateCohortPriorityOrder <-
      ordered_combinations(stateCohortPriorityOrder, n =
                             length(mergeThreshold) + 1)

    allowedStatesList_updated <-
      lapply(names(allowedStatesList), function(state_name) {
        c(allowedStatesList[[state_name]],
          stateCohortPriorityOrder[grepl(state_name, stateCohortPriorityOrder)])
      })
    names(allowedStatesList_updated) <- names(allowedStatesList)
    for (state_name in stateCohortPriorityOrder) {
      allowedStatesList_updated[[state_name]] <-
        allowedStatesList_updated[[strsplit(state_name, split =
                                              "\\+")[[1]][1]]]
    }

    allowedStatesList <- allowedStatesList_updated

    stateCohortAbsorbing <-
      unique(unlist(lapply(stateCohortAbsorbing, function(state_name) {
        stateCohortPriorityOrder[grepl(state_name,
                                       stateCohortPriorityOrder)]
      })))
  }

  ParallelLogger::logInfo("Data cleaning completed!")
  if (nrow(data) == 0) {
    ParallelLogger::logInfo("No patients left after cleaning the data!")
    return(NULL)
  }

  # save_object(
  #   path =  paste(
  #     pathToResults,
  #     "/tmp/datasets/",
  #     studyName,
  #     "CleanedImportedData.csv",
  #     sep = ""
  #   ),
  #   object = data
  # )

  if (!runGeneration) {
    return(ParallelLogger::logInfo("Completed with only cleaning the
trajectories!"))
  }
  else if (length(unique(data$COHORT_DEFINITION_ID)) < 2) {
    return(
      ParallelLogger::logInfo(
        "No state data left after cleaning the imported data! Exiting ..."
      )
    )
  }

  ParallelLogger::logInfo("Generating trajectories ...")
  result <- data.frame()
  unique_subject_ids <- unique(data$SUBJECT_ID)
  batch_size <- 1000
  batches <-
    split(unique_subject_ids, ceiling(seq_along(unique_subject_ids) /
                                        batch_size))



  # Create an empty dataframe to store the combined results
  i = 0
  if (nrow(dplyr::filter(data, COHORT_DEFINITION_ID != 0)) == 0) {
    ParallelLogger::logInfo("No trajectories generated as cohorts' do not
increment any trajectory worthy data!")
    return(NULL)
  }
  if (as.numeric(trajectoryType) == 0) {
    for (batch in batches) {
      i = i + 1
      ParallelLogger::logInfo(paste(paste("Creating batch ", i, "!!!", sep
                                          = "")))
      # Filter the data based on the current batch of SUBJECT_ID values
      batch_data <- subset(data, SUBJECT_ID %in% batch)

      # Call your function with the filtered data
      result <- rbind(
        result,
        Cohort2Trajectory::getTrajectoriesDiscrete(
          connection = connection,
          dbms = dbms,
          cohortData = batch_data,
          stateDuration = lengthOfStay,
          pathToResults = pathToResults,
          oocFix = oocFix,
          stateSelection = stateSelectionType,
          statePriorityVector = stateCohortPriorityOrder,
          absorbingStates = stateCohortAbsorbing,
          studyName = studyName,
          addPersonalData = useCDM,
          allowedStatesList = allowedStatesList
        )
      )

      if (nrow(result) == 0) {
        ParallelLogger::logInfo(
          "No trajectories generated as cohorts' do not increment any
trajectory worthy data!"
        )
        return(NULL)
      }
      save_object(result,
                  path = paste(
                    pathToResults,
                    paste(
                      "/tmp/datasets/",
                      studyName,
                      "patientDataDiscrete.csv",
                      sep = ""
                    ),
                    sep = ""
                  ))

    }

    ParallelLogger::logInfo(paste(
      "Saved trajectory dataframe: ",
      pathToResults,
      paste(
        "/tmp/datasets/",
        studyName,
        "patientDataDiscrete.csv",
        sep = ""
      ),
      sep = ""
    ))

  }
  else if (as.numeric(trajectoryType) == 1) {
    for (batch in batches) {
      i = i + 1
      ParallelLogger::logInfo(paste(paste("Creating batch ", i, "!!!", sep
                                          = "")))
      # Filter the data based on the current batch of SUBJECT_ID values
      batch_data <- subset(data, SUBJECT_ID %in% batch)
      # Call your function with the filtered data
      result <- rbind(
        result,
        Cohort2Trajectory::getTrajectoriesContinuous(
          connection = connection,
          patientData =  batch_data,
          pathToResults = pathToResults,
          stateSelection = stateSelectionType,
          statePriorityVector = stateCohortPriorityOrder,
          absorbingStates = stateCohortAbsorbing,
          studyName = studyName,
          addPersonalData = useCDM,
          allowedStatesList = allowedStatesList
        )
      )


      if (nrow(result) == 0) {
        ParallelLogger::logInfo(
          "No trajectories generated as cohorts' do not increment any
trajectory worthy data!"
        )
        return(NULL)
      }
      result = dplyr::slice(dplyr::group_by(result, SUBJECT_ID, STATE_LABEL), 1)
      save_object(result,
                  path = paste(
                    pathToResults,
                    paste(
                      "/tmp/datasets/",
                      studyName,
                      "patientDataContinuous.csv",
                      sep = ""
                    ),
                    sep = ""
                  ))

    }
    ParallelLogger::logInfo(paste(
      "Saved trajectory dataframe: ",
      pathToResults,
      paste(
        "/tmp/datasets/",
        studyName,
        "patientDataContinuous.csv",
        sep = ""
      ),
      sep = ""
    ))
  }

  ParallelLogger::logInfo("Trajectory generation completed!")

  if (useCDM) {
    ParallelLogger::logInfo("Saving trajectories to the specified temp
schema ...")

    dropRelation(
      connection = connection,
      dbms = dbms,
      schema = cdmTmpSchema,
      relationName = paste(studyName, "patient_trajectories", sep = "_")
    )

    DatabaseConnector::insertTable(
      connection = connection,
      tableName = paste(studyName, "patient_trajectories", sep = "_"),
      databaseSchema = cdmTmpSchema,
      data = result
    )

    ParallelLogger::logInfo("Trajectories saved to the specified temp
schema!")

  }


  ############################################################################
  #
  # Saving study settings as new row
  #

  ############################################################################

  if (!runSavedStudy) {
    savedTrajectoryType <- if (trajectoryType == 0) {
      "Discrete"
    }
    else {
      "Continuous"
    }
    savedTrajectoryStates <-
      as.vector(sanitize_filenames(stateCohortLabels))
    savedPriorityOrder <-
      as.vector(sanitize_filenames(stateCohortPriorityOrder))
    savedStateSelectionType <-
      as.vector(sanitize_filenames(stateSelectionType))
    savedAbsorbingStates <-
      as.vector(sanitize_filenames(stateCohortAbsorbing))
    savedMandatoryStates <-
      as.vector(sanitize_filenames(stateCohortMandatory))
    savedLengthOfStay <- lengthOfStay
    savedOutOfCohortAllowed <- as.logical(outOfCohortAllowed)
    savedOutOfCohortFix <- oocFix
    # defining a row
    newSettings <- data.frame(
      studyName,
      savedTrajectoryType,
      paste(savedTrajectoryStates, collapse = ","),
      paste(savedPriorityOrder, collapse = ","),
      as.integer(savedStateSelectionType),
      paste(savedAbsorbingStates, collapse = ","),
      paste(savedMandatoryStates, collapse = ","),
      savedLengthOfStay,
      savedOutOfCohortAllowed,
      savedOutOfCohortFix
    )

    settings <-
      read.csv(paste(
        pathToResults,
        "/inst/Settings/trajectorySettings.csv",
        sep = ""
      ))
    if (studyName %in% settings$studyName) {
      studyIndex <- which(settings$studyName == studyName)
      settings[studyIndex, ] <- newSettings
    } else {
      colnames(newSettings) <- colnames(settings)
      settings <- rbind(settings, newSettings)
    }

    write.csv(
      settings,
      paste(
        pathToResults,
        "/inst/Settings/trajectorySettings.csv",
        sep = ""
      ),
      row.names = FALSE
    )
    ParallelLogger::logInfo(paste(
      "Saved settings to: ",
      paste(
        pathToResults,
        "/inst/Settings/trajectorySettings.csv",
        sep = ""
      ),
      sep = ""
    ))
  }
  return(ParallelLogger::logInfo("Trajectories generated!"))
}



###############################################################################

#' This function eliminates patients which do not fulfill the inclusion criteria
#'
#' @param cohortData Imported cohort data
#' @param mandatoryStates States which have to be present in the trajectory, otherwise dropped#'
#' @param mergeStates Boolean, if you want to merge states when they overlap
#' @param mergeThreshold Value from 0 to 1. If mergeStates is TRUE the states will be label-merged given they overlap more than the specified threshold. Can be given as vector, then multiple iterations are runned,
#' @return A dataframe with selected patients. Columns: cohort_definition_id, subject_id, cohort_start_date, cohort_end_date
#' @keywords internal
cleanCohortData <- function(cohortData,
                            mandatoryStates,
                            outOfCohortAllowed = FALSE,
                            mergeStates = FALSE,
                            mergeThreshold = 0.5) {
  data_tmp <- cohortData


  ##############################################################################
  #
  # Preserving only patients which are present in the target cohort
  #

  ##############################################################################

  # Preserving only patients present in target cohort
  patientsEligible <-
    unique(dplyr::filter(data_tmp, COHORT_DEFINITION_ID ==
                           "0")$SUBJECT_ID)

  data_tmp <-
    dplyr::filter(data_tmp, SUBJECT_ID %in% patientsEligible)

  ##############################################################################
  #
  # Cleaning data from observations before and after target cohort
  #

  ##############################################################################
  # Removing subjects information which have been included before
  # target cohort start date (maybe should just switch the start date
  # then for the same as target cohort)

  # Selecting the first occurring target cohort row
  data_target <- dplyr::slice(dplyr::arrange(
    dplyr::group_by(
      dplyr::filter(data_tmp, COHORT_DEFINITION_ID == "0"),
      SUBJECT_ID
    ),
    COHORT_START_DATE
  ), 1L)
  # Selecting information about the states
  data_states <-
    dplyr::filter(data_tmp, COHORT_DEFINITION_ID != "0")

  if (mergeStates) {
    ParallelLogger::logInfo("Merging labels according to the specified
treshold!")

    data_states <-
      combineCohorts(data_states,
                     mergeThreshold,
                     unique(data_states$SUBJECT_ID))

    ParallelLogger::logInfo("Label merging completed!")
  }
  data_tmp <- rbind(data_target, data_states)
  data_target <-
    dplyr::select(data_target, SUBJECT_ID, COHORT_START_DATE,
                  COHORT_END_DATE)
  colnames(data_target) <-
    c("SUBJECT_ID", "REFERENCE_START_DATE", "REFERENCE_END_DATE")
  data_tmp <- merge(data_tmp, data_target, by = "SUBJECT_ID")
  # If the state start date is after inclusion criteria end date then let's filter it out
  if (!outOfCohortAllowed) {
    data_tmp <-
      dplyr::filter(data_tmp,!(REFERENCE_END_DATE < COHORT_START_DATE))
  }
  # If the state end date is before inclusion criteria start date then let's filter it out
  data_tmp <-
    dplyr::filter(data_tmp,!(REFERENCE_START_DATE > COHORT_END_DATE))

  data_tmp$COHORT_END_DATE <- as.Date(data_tmp$COHORT_END_DATE)

  data_tmp$REFERENCE_END_DATE <-
    as.Date(data_tmp$REFERENCE_END_DATE)
  # If the state start date is inside the interval but the end date is outside the interval
  # then cut the endpoint to reference end point
  if (!outOfCohortAllowed) {
    data_tmp <- dplyr::mutate(
      data_tmp,
      COHORT_END_DATE = dplyr::if_else(
        REFERENCE_END_DATE < COHORT_END_DATE,
        as.Date(REFERENCE_END_DATE),
        as.Date(COHORT_END_DATE)
      )
    )
  }
  # If state start date is before inclusion criteria start date then let's change it to the target cohort start date
  data_tmp <- dplyr::mutate(
    data_tmp,
    COHORT_START_DATE =
      dplyr::if_else(
        REFERENCE_START_DATE <= COHORT_START_DATE,
        COHORT_START_DATE,
        REFERENCE_START_DATE
      )
  )
  # Lets prioritize the cohorts giving the target cohort priority 0 others 1
  # We use priority feature to always order target cohort first per  patient in the dataframe
  # this positioning is needed for calculating the feature TIME_IN_COHORT
  data_tmp <- dplyr::mutate(data_tmp,
                            PRIORITY = dplyr::if_else(COHORT_DEFINITION_ID
                                                      == "0", 0, 1))
  data_tmp <-
    dplyr::arrange(data_tmp, SUBJECT_ID, PRIORITY, COHORT_START_DATE)


  ##############################################################################
  #
  # Preserving only patients which have the mandatory state(s)
  #

  ##############################################################################

  # Mandatory states
  if (!(length(mandatoryStates) == 0 |
        "No mandatory state" %in% mandatoryStates)) {
    for (state in mandatoryStates) {
      tmpPatientsEligible = unique(dplyr::filter(data_tmp,
                                                 COHORT_DEFINITION_ID == state)$SUBJECT_ID)
      patientsEligible = intersect(patientsEligible, tmpPatientsEligible)

    }
  }
  data_tmp <-
    dplyr::filter(data_tmp, SUBJECT_ID %in% patientsEligible)


  ##############################################################################
  #
  # Adding feature "TIME IN COHORT"
  #

  ##############################################################################

  # Order by patientId, start & end date
  #data_merged = data_merged[order(data_merged[, 1], data_merged[, 3],data_merged[, 4]),]
  data_tmp <-
    dplyr::mutate(data_tmp, TIME_IN_COHORT = round(as.numeric(
      difftime(
        as.Date(COHORT_START_DATE),
        as.Date(REFERENCE_START_DATE),
        units = "days"
      ) /
        365.25
    ), 3))

  data_tmp <- dplyr::select(
    data_tmp,
    SUBJECT_ID,
    COHORT_DEFINITION_ID,
    COHORT_START_DATE,
    COHORT_END_DATE,
    TIME_IN_COHORT
  )
  return(data_tmp)
}
