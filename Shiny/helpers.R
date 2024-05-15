############
#
# Visualisation function for Shiny app
#
############

#' Data format
#'
#' Formats data to suit visualisation. Used with cohortSurvivalAnalysis Shiny application
#'
#' @param data OMOP CDM dataset; values from cohortSurvivalAnalaysis package are suitable
#' @param showAllAges logical value that either shows data for patients of all ages or filters according to ageMin and ageMax
#' @param ageMin minimum age set for data filtering, only used if showAllAges == FALSE
#' @param ageMax maximum age set for data filtering, only used if showAllAges == FALSE
#'
#' @return
#' @export
#'
#' @examples
dataformat <- function(data, showAllAges, ageMin, ageMax) {
  library(dplyr)
  data <- data %>%
    select(SUBJECT_ID,
           GENDER_CONCEPT_ID,
           AGE,
           STATE_LABEL,
           TIME_IN_COHORT) %>%
    group_by(SUBJECT_ID) %>%
    mutate(AGE = floor(min(AGE))) %>%
    tidyr::spread(key = STATE_LABEL, value = TIME_IN_COHORT) %>%
    mutate(absorbingState = ifelse("absorbingState" %in% colnames(.), absorbingState, NA)) %>%
    mutate(EVENT = ifelse(is.na(absorbingState), 0, 1)) %>%
    rename(GENDER = GENDER_CONCEPT_ID, TIME = EXIT) %>%
    filter(ifelse(showAllAges == FALSE, AGE >= ageMin &
                    AGE <= ageMax, AGE > 0)) %>%
    mutate(GENDER = ifelse(
      GENDER == 8532,
      "female",
      ifelse(GENDER == 8507, "male", "unknown")
    )) %>%
    select(AGE, GENDER, TIME, EVENT)

  return(data)
}


#' Plot Kaplan Meier
#'
#' Plots KM curve for survival analysis tabpanel. Used with cohortSurvivalAnalysis Shiny application
#' Parameter info is available under dataformat function.
#'
#' @param data
#' @param showAllAges
#' @param showGenders
#' @param ageMin
#' @param ageMax
#'
#' @return
#' @export
#'
#' @examples
plot_KaplanMeier <-
  function(data,
           showAllAges,
           showGenders,
           ageMin,
           ageMax) {
    data <- dataformat(data, showAllAges, ageMin, ageMax)

    if (showGenders == TRUE) {
      fit <-
        survival::survfit(survival::Surv(time = data$TIME, event = data$EVENT) ~
                            data$GENDER,
                          data = data)
    } else {
      fit <-
        survival::survfit(survival::Surv(time = data$TIME, event = data$EVENT) ~
                            1,
                          data = data)
    }

    survminer::ggsurvplot(
      fit,
      data = data,
      pval = TRUE,
      pval.method = TRUE,
      conf.int = TRUE
    )

  }


#' Kaplan-Meier plot with data from separate dbs. Used with cohortSurvivalAnalysis Shiny application
#' Parameter info is available under dataformat function.
#'
#'
#' @param data
#' @param showAllAges
#' @param showGenders
#' @param ageMin
#' @param ageMax
#' @param data2 other OMOP CDM dataset that data is compared to
#'
#' @return
#' @export
#'
#' @examples
plot_KaplanMeierCombine <-
  function(data,
           showAllAges,
           showGenders,
           ageMin,
           ageMax,
           data2) {
    data <- dataformat(data, showAllAges, ageMin, ageMax)
    data2 <- dataformat(data2, showAllAges, ageMin, ageMax)
    if (showGenders == TRUE) {
      fit <-
        survival::survfit(survival::Surv(time = data$TIME, event = data$EVENT) ~
                            data$GENDER,
                          data = data)
      fit2 <-
        survival::survfit(survival::Surv(time = data2$TIME, event = data2$EVENT) ~
                            data2$GENDER,
                          data = data2)
      survminer::ggsurvplot_combine(
        list(fit, fit2),
        list(data, data2),
        risk.table = FALSE,
        pval = TRUE,
        pval.method = TRUE,
        conf.int = TRUE,
        legend.labs = c("main db1", "main db2")#, "compare db1", "compare db2"),
        #palette = c("blue", "red")#, "green", "pink")
      )
    } else {
      fit <-
        survival::survfit(survival::Surv(time = data$TIME, event = data$EVENT) ~
                            1,
                          data = data)
      fit2 <-
        survival::survfit(survival::Surv(time = data2$TIME, event = data2$EVENT) ~
                            1,
                          data = data2)
      survminer::ggsurvplot_combine(
        list(fit, fit2),
        list(data, data2),
        risk.table = FALSE,
        pval = TRUE,
        pval.method = TRUE,
        conf.int = TRUE,
        legend.labs = c("main db", "compare db"),
        palette = c("blue", "red")
      )
    }


  }

#' Cumulative hazard plot
#'
#' Used with cohortSurvivalAnalysis Shiny application
#' Parameter info is available under dataformat function.
#'
#' @param data
#' @param showAllAges
#' @param showGenders
#' @param ageMin
#' @param ageMax
#'
#' @return
#' @export
#'
#' @examples
plot_cumulativeHazard <-
  function(data,
           showAllAges,
           showGenders,
           ageMin,
           ageMax) {
    data <- dataformat(data, showAllAges, ageMin, ageMax)
    #fit <- survival::survfit(survival::Surv(time=data$TIME, event=data$EVENT)~1, data=data)

    if (showGenders == TRUE) {
      fit <-
        survival::survfit(survival::Surv(time = data$TIME, event = data$EVENT) ~
                            data$GENDER,
                          data = data)
    } else {
      fit <-
        survival::survfit(survival::Surv(time = data$TIME, event = data$EVENT) ~
                            1,
                          data = data)
    }

    survminer::ggsurvplot(
      fit,
      data = data,
      ylab = "Hazard rate",
      pval = TRUE,
      pval.method = TRUE,
      fun = function(y)
        - log(y)
    )
  }

#' Plot gender
#'
#' Shows the gender distribution in filtered data with a donut type plot. Used with cohortSurvivalAnalysis Shiny application
#' Parameter info is available under dataformat function.
#'
#' @param data
#' @param showAllAges
#' @param ageMin
#' @param ageMax
#'
#' @return
#' @export
#'
#' @examples
plot_gender <- function(data, showAllAges, ageMin, ageMax) {
  data <- dataformat(data, showAllAges, ageMin, ageMax)
  data$GENDER <- as.character(data$GENDER)
  plot_data <-
    data %>% group_by(GENDER) %>% summarise(count = n()) %>%
    mutate(fraction = count / sum(count)) %>%
    mutate(ymax = cumsum(fraction)) %>%
    mutate(ymin = c(0, head(ymax, n = -1))) %>%
    mutate(labelPosition = (ymax + ymin) / 2) %>%
    mutate(label = paste(GENDER, "\n value", count))

  ggplot2::ggplot(plot_data,
                  ggplot2::aes(
                    ymax = ymax,
                    ymin = ymin,
                    xmax = 4,
                    xmin = 3,
                    fill = GENDER
                  )) +
    ggplot2::geom_rect() +
    ggplot2::geom_label(x = 3.5,
                        ggplot2::aes(y = labelPosition, label = label),
                        size = 3) +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(c(2, 4)) +
    ggplot2::scale_fill_brewer("BuGn") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(1, 1, 1, 1, unit = "pt"))


}



#' Age distribution
#'
#' Shows age distribution on selected data. Used with cohortSurvivalAnalysis Shiny application
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_ageDistribution <- function(data) {
  data <- dataformat(data, TRUE, 0, 0)
  data = data %>% group_by(AGE) %>% summarise(freq = n())
  stats <- summary(data$AGE)

  ggplot2::ggplot(data, ggplot2::aes(
    x = AGE,
    y = freq,
    color = "lightblue"
  )) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = stats[[2]], color = "red")) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = stats[[3]], color = "red")) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = stats[[5]], color = "red")) +
    ggplot2::labs(x = "age", y = "frequency") +
    ggplot2::scale_color_manual(values = c("#ADD8E6", "#CC6666")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

}
