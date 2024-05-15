################################################################################
# The following code relies heavily on Cohort2Trajectory package
#
# Name: Cohort2Trajectory
# Author: Haug, M.
# Version: 1.1.3
# Source: https://github.com/HealthInformaticsUT/Cohort2Trajectory
################################################################################

devtools::load_all()

################################################################################
# Database credentials
################################################################################

pathToDriver <<-  #
dbms <<- ""    #
user <<- '' #
pw <<-  #
server <<- #
port <<-  #

# Schema which contains the OHDSI Common Data Model
cdmSchema <<- "" #

# Schema which contains the OHDSI Common Data Model vocabulary tables.
cdmVocabSchema <<- "" #

# Schema for temporary tables, will be deleted # should be ohdsi_temp
cdmTmpSchema <<- "" #

# Schema which will contain the final results
cdmResultsSchema <<- "" #

# Name of the OHDSI Common Data Model
cdmName <<- "" #

################################################################################
# Initiate the database connection
################################################################################

connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    server = server,
    user = user,
    password = pw,
    port = port,
    pathToDriver = pathToDriver
  )

conn <- DatabaseConnector::connect(connectionDetails)

################################################################################
# Run cohortVisualAnalysis workflow
################################################################################

# This operation is very time consuming
# it is recommended to use showCohorts parameter
cohortVisualAnalysis(
  readQueries = FALSE,
  cohortURL = "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/cohorts/",
  cohortsURL = "https://raw.githubusercontent.com/OHDSI/PhenotypeLibrary/main/inst/Cohorts.csv",
  days = 365,
  absorbingState = , #TODO "c<x>.json"
  targetCohorts = , #TODO c("c<x>.json")

  dbms = dbms,
  conn = conn,
  cdmSchema = cdmSchema,
  cdmVocabSchema = cdmVocabSchema,
  cdmTmpSchema = cdmTmpSchema,
  cdmResultsSchema = cdmResultsSchema
)

################################################################################
# Manage datasets location
################################################################################

# WARNING: this will remove all previous data in /tmp/datasets_<cdmName>
# rename the directory manually if needed

unlink(paste0(pathToResults, "/tmp/datasets_", cdmName), recursive = TRUE)
dir.create(paste0(pathToResults, "/tmp/datasets_", cdmName))
file.rename(
  paste0(pathToResults, "/tmp/datasets"),
  paste0(pathToResults, "/tmp/datasets_", cdmName)
)

################################################################################
# Run GUI
################################################################################

shiny::runApp("./Shiny")

