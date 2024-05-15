#' Cohort Visual Analysis
#'
#' This function contains the main workflow for this R package.
#' Cohorts are read in and database queries are made.
#'
#' @param readQueries (boolean), deafult TRUE; json files are read from URL
#' @param cohortURL (String) default value "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/cohorts/"
#' @param cohortsURL (String) default value "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/Cohorts.csv"
#' @param days (String) default 365
#' @param absorbingState (c()) default c("c688.json"); absorbing state cohort for cohort2Trajectory, must be in /inst/JSON
#' @param targetCohorts c(""), default NULL; selection of cohorts that are collected from URL and used as the starting event for trajectory. Vector element must be structured as "c<Id>.json"
#' @param dbms database type
#' @param conn database connection
#' @param cdmSchema Schema which contains the OHDSI Common Data Model
#' @param cdmVocabSchema Schema which contains the OHDSI Common Data Model vocabulary tables
#' @param cdmTmpSchema Schema for temporary tables, will be deleted # should be ohdsi_temp
#' @param cdmResultsSchema Schema which will contain the final results
#' @param pathToResults
#'
#' @return
#' @export
#'
#' @examples
#' cohortVisualAnalysis(readQueries = TRUE,cohortURL = "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/cohorts/",cohortsURL = "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/Cohorts.csv", days = 365, absorbingState = c("c688.json"),targetCohorts = NULL, dbms = "dbms", conn = conn, cdmSchema = "cdmSchema", cdmVocabSchema = "cdmVocabSchema", cdmTmpSchema = "cdmTmpSchema", cdmResultsSchema = "cdmResultsSchema")
#'
#'
#'
cohortVisualAnalysis <- function(readQueries = TRUE,
                                 cohortURL = "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/cohorts/",
                                 cohortsURL = "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/Cohorts.csv",
                                 days = 365,
                                 absorbingState = c("c688.json"),
                                 targetCohorts = NULL,

                                 dbms,
                                 conn,
                                 cdmSchema,
                                 cdmVocabSchema,
                                 cdmTmpSchema,
                                 cdmResultsSchema,
                                 pathToResults = getwd()) {
  pathToResults <<- pathToResults
  createMandatorySubDirs(pathToResults)


  ################################################################################
  # read JSON files
  ################################################################################
  if (readQueries == TRUE) {
    # incase targetCohorts defined, adjust to suit cohortIds
    cohortIds <- NULL
    if (!is.null(targetCohorts)) {
      targetCohorts <- append(targetCohorts, absorbingState)
      cohortIds = substr(targetCohorts, 2, nchar(targetCohorts))
    }

    readPhenotypeLibraryToJson(
      cohortsURL = cohortsURL,
      cohortURL = cohortURL,
      inclusionRulesDays = days,
      setInclusionRules = TRUE,
      cohortIds = cohortIds,
      pathToResults = pathToResults
    )

  }

  ################################################################################
  # Creating absorbing state
  ################################################################################

  #file.remove(pathToResults, "/inst/JSON/absorbingState.json")
  file.rename(
    paste0(pathToResults, "/inst/JSON/", absorbingState),
    paste0(pathToResults, "/inst/JSON/absorbingState.json")
  )
  studyName <- "absorbingState"
  stateCohortLabels = c(studyName)

  # Creates a list allowing all transitions from each state
  allowedStatesList = createStateList(stateCohortLabels)

  Cohort2Trajectory_modified(
    dbms = dbms,
    connection = conn,
    cdmSchema = cdmSchema,
    cdmVocabSchema = cdmVocabSchema,
    cdmTmpSchema = cdmTmpSchema,
    cdmResultsSchema = cdmResultsSchema,
    studyName = studyName,
    runSavedStudy = TRUE,
    pathToResults = pathToResults,
    allowedStatesList = allowedStatesList,
    mergeStates = FALSE,
  )

  Cohort2Trajectory::dropRelation(
    connection = conn,
    dbms = dbms,
    schema = cdmTmpSchema,
    relationName = "codesets"
  )

  ################################################################################
  # db queries with target cohorts
  ################################################################################

  # set jsonLabels to a suitable value either all or those cohorts specified
  if (!is.null(targetCohorts)) {
    jsonLabels <- targetCohorts
  } else {
    jsonLabels <- list.files(
      path = paste(pathToResults, "/inst/JSON", sep = ""),
      pattern = NULL,
      all.files = FALSE,
      full.names = FALSE
    )
  }
  # cleaning jsonLabels incase absorbing state is present
  absorbingCohortId <- substr(absorbingState, 1, nchar(absorbingState) - 5)
  jsonLabels <-
    setdiff(substr(jsonLabels, 1, nchar(jsonLabels) - 5),
            c(studyName, absorbingCohortId))

  for (targetName in jsonLabels) {
    stateCohortLabels = c(targetName, paste0(studyName,".json"))
    # Creates a list allowing all transitions from each state
    allowedStatesList = createStateList(stateCohortLabels)

    Cohort2Trajectory_modified(
      dbms = dbms,
      connection = conn,
      cdmSchema = cdmSchema,
      cdmVocabSchema = cdmVocabSchema,
      cdmTmpSchema = cdmTmpSchema,
      cdmResultsSchema = cdmResultsSchema,
      studyName = targetName,
      runSavedStudy = TRUE,
      pathToResults = pathToResults,
      allowedStatesList = allowedStatesList,
      mergeStates = FALSE,
    )
  }

}
