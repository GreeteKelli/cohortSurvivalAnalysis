#' Read phenotype library to json
#'
#'This functions reads x.json cohorts from OHDSI/PhenotypeLibrary github
#'repository. The JSON-files are adjusted to suit ATLAS queries.
#'
#' @param cohortsURL (String) link to raw webpage containing csv with cohorts and information
#' @param cohortURL (String) partial link to raw webpage that contains a json file for a specific cohort
#' @param inclusionRulesDays (String) parameter for ATLAS JSON query StartWindow start days
#' @param setInclusionRules (boolean) deafault value TRUE; option to not use inclusion rules
#' @param cohortIds (c()) default value NULL; adding vector with cohortIds will read only those cohorts
#' @param pathToResults (String) path to working directory
#'
#' @return
#' @export
#'
#' @examples
#' readPhenotypeLibraryToJson(inclusionRulesDays = "730", cohortIds = c(688,1054,785))
#'
readPhenotypeLibraryToJson <- function(cohortsURL = "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/Cohorts.csv",
                                       cohortURL = "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/cohorts/",
                                       inclusionRulesDays = 365,
                                       setInclusionRules = TRUE,
                                       cohortIds = NULL,
                                       pathToResults) {
  pathToResults <<- pathToResults

  # inclusion rules
  inclusionRules <-
    readInclusionRules(paste0(pathToResults, "/R/inclusionrules.json"),
                       inclusionRulesDays)

  ParallelLogger::logInfo(paste("Reading JSON files from", cohortsURL))
  ParallelLogger::logInfo("This may take a while ...")

  # read diseases cohort, filter if needed
  diseases <- read.csv(cohortsURL)

  #create name library
  names <-
    data.frame(id = diseases$cohortId,
               cohort = diseases$cohortNameFormatted)

  if (!is.null(cohortIds)) {
    cohortIds <- substr(cohortIds, 1, nchar(cohortIds) - 5)
    diseases = diseases[diseases$cohortId %in% cohortIds, ]
  }

  # partial link to RAW files
  linkToRawJson <- cohortURL

  for (i in rownames(diseases)) {
    # initiate and format cohort name
    cohortName = stringr::str_to_title(diseases[i, "cohortNameFormatted"])
    index = diseases[i, "cohortId"]

    # incase of faulty row
    if (is.na(cohortName)){
      next
    }

    # read file
    textFromRawJson <-
      readr::read_file(paste0(linkToRawJson, index, ".json"))
    # replace line breaks and tab symbols
    JSONfile = stringr::str_replace_all(textFromRawJson, "\t|\n", "")
    # modify file to use as query in ATLAS
    if (setInclusionRules == TRUE) {
      JSONfile = modifyJSON(JSONfile, inclusionRules, cohortName)
    }

    # save modified JSON file
    target_name = paste0(pathToResults, "/inst/JSON/c", index, ".json")
    write(JSONfile, target_name)
  }

  write.csv(names, paste0(pathToResults, "/tmp/names.csv"))

  ParallelLogger::logInfo("DONE!")
  ParallelLogger::logInfo("JSON files are available in /inst/JSON")
  ParallelLogger::logInfo(
    "names.csv containing names and ids of cohorts is available in ./tmp/datasets/names.csv"
  )
}


#' Modify JSON
#'
#' Adds inclusion rules and other necessary changes in list format.
#' Subfunction of readPhenotypeLibraryToJson
#' @param textFormatted (String) JSON query from PhenotypeLibrary
#' @param inclusionRules (list) specified inclusion rules available in
#' @param cohortName (String)
#'
#' @return returns ATLAS query in JSON format
#' @export
#'
#' @examples
modifyJSON <- function(textFormatted,
                       inclusionRules,
                       cohortName) {
  inclusionRules <- rjson::fromJSON(file = "R/inclusionrules.json")

  # read JSON text as list
  JSONlist <- rjson::fromJSON(json_str = textFormatted)

  # change inclusion rules according to disease
  #inclusionRules[[1]][[1]][1] = paste('No', cohortName)
  inclusionRules$InclusionRules[[1]]$name = paste('No', cohortName)

  # add altered inclusion rules to JSON list
  JSONlist$InclusionRules = inclusionRules[[1]]

  # file changes
  JSONlist$ExpressionLimit$Type = "First"
  JSONlist$PrimaryCriteria$CriteriaList[[1]]$ConditionOccurrence = JSONlist$PrimaryCriteria$CriteriaList[[1]]$ConditionOccurrence[-2]
  JSONlist$CensorWindow = {}
  JSONlist$EndStrategy = {}
  JSONlist$PrimaryCriteria$PrimaryCriteriaLimit$Type = "First"

  # altered JSON list to text
  JSONfile = rjson::toJSON(JSONlist)

  return(JSONfile)
}



#' Read inclusion rules
#'
#' Reads in inclusion rules file and modifies start window time of the query if needed
#' subfunction of readPhenotypeLibraryToJson
#'
#' @param filename (String) path to inclusion rules filename
#' @param startWindowTime (String) sets start window time inclusion rule value
#'
#' @return inclusion rules
#' @export
#'
#' @examples
readInclusionRules <- function(filename, startWindowTime) {
  inclusionRules <- rjson::fromJSON(file = "R/inclusionrules.json")
  inclusionRules[[1]]$expression$CriteriaList[[1]]$StartWindow$Start$Days <-
    startWindowTime

  ParallelLogger::logInfo(paste(
    "Set inclusion rules with ",
    startWindowTime,
    " days as startWindowTime"
  ))
  return(inclusionRules)
}
