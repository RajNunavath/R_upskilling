install.packages(c(
  "teal",
  "teal.data",
  "teal.modules.general",
  "admiral.test",
  "dplyr",
  "tidyr"
))

install.packages(
  "admiral.test",
  repos = c("https://pharmaverse.r-universe.dev", getOption("repos"))
)
library(admiral.test)
library(dplyr)

adsl <- adsl

demog_table <- function(data) {
  
  total_n <- nrow(data)
  
  cat_summary <- function(var, label) {
    data %>%
      count(.data[[var]]) %>%
      mutate(
        Variable = label,
        Level = .data[[var]],
        N = n,
        Percent = round(100 * n / total_n, 1)
      ) %>%
      select(Variable, Level, N, Percent)
  }
  
  cont_summary <- function(var, label) {
    data %>%
      summarise(
        Mean = round(mean(.data[[var]], na.rm = TRUE), 1),
        SD = round(sd(.data[[var]], na.rm = TRUE), 1),
        Median = round(median(.data[[var]], na.rm = TRUE), 1),
        Min = round(min(.data[[var]], na.rm = TRUE), 1),
        Max = round(max(.data[[var]], na.rm = TRUE), 1)
      ) %>%
      pivot_longer(everything(),
                   names_to = "Level",
                   values_to = "Value") %>%
      mutate(
        Variable = label,
        N = NA,
        Percent = NA
      ) %>%
      select(Variable, Level, N, Percent, Value)
  }
  
  bind_rows(
    cat_summary("SEX", "Sex"),
    cat_summary("RACE", "Race"),
    cat_summary("ETHNIC", "Ethnicity"),
    cont_summary("AGE", "Age"),
    # cont_summary("HEIGHTBL", "Height (cm)"),
    # cont_summary("WEIGHTBL", "Weight (kg)"),
    # cont_summary("BMIBL", "BMI")
  )
  
}
ADSL_DEMOG <- demog_table(adsl)
library(teal)
library(teal.data)

data <- teal_data(
  ADSL = adsl,
  ADSL_DEMOG = ADSL_DEMOG
)


library(teal.modules.general)

modules <- modules(
  tm_data_table(
    label = "Demographics (Table 1)",
    datanames = "ADSL_DEMOG"
  )
)

app <- init(
  data = data,
  modules = modules
)

shiny::shinyApp(app$ui, app$server)

