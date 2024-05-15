#####
#
# UI of the Shiny app
#
############

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = span(img(
    src = "UT_logo.png", width = 250
  ))),
  shinydashboard::dashboardSidebar(
    shiny::radioButtons(
      inputId = "dbMain",
      label = "Select main database",
      choices = c("")
    ),
    shiny::radioButtons(
      inputId = "dbCompare",
      label = "Compare to ",
      choices = c("")
    ),
    shiny::selectInput("disease", "Select disease", c()),
    shiny::checkboxInput("genderShowAll", "Differentiate data by gender", value =
                           FALSE),
    shiny::checkboxInput("ageInputShowAll", "Display data with all ages", value =
                           TRUE),
    shiny::sliderInput(
      "sliderAge",
      "Select age values",
      min = 0,
      max = 100,
      value = c(25, 55)
    ),
    width = 270
  ),
  shinydashboard::dashboardBody(
    shiny::includeCSS("www/customTheme.css"),
    shiny::tabsetPanel(
      id = "tabset",
      shiny::tabPanel(
        "Survival analysis",
        shinydashboard::box(plotOutput("plot"), title = "Kaplan-Meier curve"),
        shinydashboard::box(plotOutput("plot_cumulativeHazard"), title =
                              "Cumulative hazard plot")
      ),
      shiny::tabPanel(
        "Data analytics",
        column(
          width = 5,
          shinydashboard::box(
            htmlOutput("patientCount"),
            title = "Data for selected cohort",
            width = NULL
          ),
          shinydashboard::box(
            plotOutput("plot_gender"),
            title = "Gender distribution",
            width = NULL
          )

        ),
        column(
          width = 7,
          shinydashboard::box(
            plotOutput("plot_ageHist"),
            title = "Age distribution",
            width = NULL
          )
        )

      ),
      shiny::tabPanel(
        "Comparison",
        shinydashboard::box(plotOutput("KM_compare"), title =
                              "Kaplan-Meier curves"),
        column(
          width = 6,
          shinydashboard::box(
            htmlOutput("patientCount2"),
            title = "Event count in comparison db dataset",
            width = NULL
          ),
          shinydashboard::box(
            plotOutput("plot_gender2"),
            title = "Gender distribution in comparison db dataset",
            width = NULL
          ),
          shinydashboard::box(
            plotOutput("plot_ageHist2"),
            title = "Age distribution in comparison db dataset",
            width = NULL
          )
        )
      ),
      shiny::tabPanel(
        "Help",
        h2("R-package for survival analysis"),
        p(
          "This package has been developed as a student project, which is part of a bachelor's thesis."
        ),
        br(),
        h3("User guide"),
        p(
          "The parameters on the left allow you to select the data to be visualized with some filtering options available."
        ),
        p(
          "'Select main database' and 'select compare to' choose directories where to expect OMOP CDM datasets"
        ),
        p(
          "The 'Select disease' dropdown menu contains all cohorts found in /tmp/datasets_<db_name>"
        ),
        p(
          "Differentiate by gender: This option separates data based on the gender categories present in the dataset."
        ),
        p(
          "Display data with all ages: This option is enabled by default. Unchecking the checkbox will result in the usage of ages selected on the slider below."
        ),
        br(),
        p(
          "Data visualization may take some time. If in doubt, you can verify whether or not a disease was chosen (it should be visible under 'Select disease')."
        )
      )
    )
  )
)
