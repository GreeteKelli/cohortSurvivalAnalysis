#####
#
# Server logic of the Shiny app
#
############

server <- function(input, output, session) {
  library(dplyr)
  # available databases
  databaseDirs <-
    list.dirs("../tmp", full.names = FALSE, recursive = FALSE)

  # update database selection
  shiny::updateRadioButtons(session, "dbMain", choices = databaseDirs)

  # update compare to selection
  shiny::updateRadioButtons(session, "dbCompare", choices = databaseDirs)

  # update event selection based on available datasets in selected dbs
  diseaseNamesdir <- read.csv("../tmp/names.csv")

  observe({
    req(input$dbMain)
    cohortFiles <- list.files(paste0("../tmp/", input$dbMain))
    cohortFiles <-
      setdiff(substr(cohortFiles, 2, nchar(cohortFiles) - 25), c("c688"))
    diseaseNamesdir <-
      dplyr::filter(diseaseNamesdir, id %in% cohortFiles)
    shiny::updateSelectInput(session, "disease", choices = diseaseNamesdir$cohort)
  })

  # reads selected event data from main db /datasets
  data <- shiny::reactive({
    req(input$disease)
    req(input$dbMain)
    fileName <-
      diseaseNamesdir %>% filter(cohort == input$disease) %>% select(id) #%>% top_n(n=1)#
    diseaseData <-
      read.csv(paste0(
        "../tmp/",
        input$dbMain,
        "/c",
        fileName,
        "patientDataContinuous.csv"
      ))
  })

  # reads selected event data from compare db /datasets
  data2 <- shiny::reactive({
    req(input$disease)
    req(input$dbCompare)
    fileName <-
      diseaseNamesdir %>% filter(cohort == input$disease) %>% select(id) #%>% top_n(n=1)
    diseaseData <-
      read.csv(
        paste0(
          "../tmp/",
          input$dbCompare,
          "/c",
          fileName,
          "patientDataContinuous.csv"
        )
      )
  })

  ##
  # following code contains all output plots
  ##

  # Survival analysis tab panel
  output$plot <- shiny::renderPlot({
    plot_KaplanMeier(
      data(),
      input$ageInputShowAll,
      input$genderShowAll,
      input$sliderAge[1],
      input$sliderAge[2]
    )

  })

  output$plot_cumulativeHazard <- shiny::renderPlot({
    plot_cumulativeHazard(
      data(),
      input$ageInputShowAll,
      input$genderShowAll,
      input$sliderAge[1],
      input$sliderAge[2]
    )
  })

  # Data analysis tab panel
  output$plot_gender <- shiny::renderPlot({
    plot_gender(data(),
                input$ageInputShowAll,
                input$sliderAge[1],
                input$sliderAge[2])
  })

  output$patientCount <- shiny::renderUI({
    formattedData <-
      dataformat(data(),
                 input$ageInputShowAll,
                 input$sliderAge[1],
                 input$sliderAge[2])
    data_info <-
      formattedData %>% group_by(EVENT) %>% summarise(count = n()) %>% arrange(EVENT)
    str <- paste0("", data_info[1, ]$count)
    total <- paste("Total number of patients", sum(data_info$count))
    noEvent <- paste("No event", data_info[1, ]$count)
    event <- paste("Event", data_info[2, ]$count)

    HTML(paste(total, noEvent, event, sep = "<br/>"))

  })

  output$plot_ageHist <- shiny::renderPlot({
    plot_ageDistribution(data())
  })

  ## Comparison tab panel
  output$KM_compare <- shiny::renderPlot({
    plot_KaplanMeierCombine(
      data(),
      input$ageInputShowAll,
      input$genderShowAll,
      input$sliderAge[1],
      input$sliderAge[2],
      data2()
    )

  })

  output$plot_gender2 <- shiny::renderPlot({
    plot_gender(data2(),
                input$ageInputShowAll,
                input$sliderAge[1],
                input$sliderAge[2])

  })

  output$patientCount2 <- shiny::renderUI({
    formattedData <-
      dataformat(data2(),
                 input$ageInputShowAll,
                 input$sliderAge[1],
                 input$sliderAge[2])
    data_info <-
      formattedData %>% group_by(EVENT) %>% summarise(count = n()) %>% arrange(EVENT)
    str <- paste0("", data_info[1, ]$count)
    total <- paste("Total number of patients", sum(data_info$count))
    noEvent <- paste("No event", data_info[1, ]$count)
    event <- paste("Event", data_info[2, ]$count)

    HTML(paste(total, noEvent, event, sep = "<br/>"))

  })

  output$plot_ageHist2 <- shiny::renderPlot({
    plot_ageDistribution(data2())

  })

}
