#--------------------------------------------------
# Load Packages
#--------------------------------------------------
rm(list = ls())

library(tidyverse)
library(r2rtf)
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(pharmaverseadam)
library(lubridate)
library(stringr)
library(teal)
library(teal.data)
library(teal.modules.general)
library(shiny)

#--------------------------------------------------
# Demographics Table Function
#--------------------------------------------------
create_demog_table <- function(adsl_data) {
  
  # Filter out Screen Failure
  adsl <- adsl_data %>% filter(TRT01P != "Screen Failure")
  
  # Create Total
  asl <- adsl %>%
    arrange(TRT01P) %>%
    mutate(TRT01P = "Total")
  
  tot_ <- bind_rows(asl, adsl)
  
  # Get VS data
  vs1 <- convert_blanks_to_na(vs)
  vss <- vs %>%
    group_by(USUBJID, VSTESTCD) %>%
    summarise(VSORRES = mean(as.numeric(VSORRES), na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = VSTESTCD, values_from = VSORRES)
  
  # Merge with ADSL
  ad_vs <- left_join(tot_, vss, by = "USUBJID")
  
  # Big N
  biggn <- ad_vs %>%
    filter(!TRT01P == "Screen Failure") %>%
    group_by(TRT01P) %>%
    count(TRT01P) %>%
    mutate(N1 = n) %>%
    select(-n)
  
  # Continuous variable function
  new_function <- function(var, var1, ord_) {
    summary_stats <- ad_vs %>%
      filter(TRT01P != "Screen Failure") %>%
      group_by(.data[[var1]]) %>%
      mutate(
        n = as.character(n()),
        mean = format(round(mean(.data[[var]], na.rm = TRUE)), nsmall = 2),
        median = format(trunc(median(.data[[var]], na.rm = TRUE)), nsmall = 2),
        min = as.character(trunc(min(.data[[var]], na.rm = TRUE)), nsmall = 0),
        max = as.character(trunc(max(.data[[var]], na.rm = TRUE)), nsmall = 0),
        std = format(round(sd(.data[[var]], na.rm = TRUE)), nsmall = 3)
      ) %>%
      distinct(n, mean, median, min, std, max) %>%
      select(n, mean, median, std, min, max, var1) %>%
      pivot_longer(cols = c(n, mean, median, std, min, max), names_to = "variable", values_to = "value") %>%
      pivot_wider(names_from = var1, values_from = value) %>%
      mutate(ord = ord_)
    
    return(summary_stats)
  }
  
  # Categorical variable function
  my_function <- function(vv, ord_) {
    char <- tot_ %>%
      filter(!TRT01P == "Screen Failure") %>%
      group_by(TRT01P) %>%
      count(.data[[vv]])
    
    per <- biggn %>%
      group_by(TRT01P) %>%
      right_join(char, by = "TRT01P") %>%
      mutate(
        per = n / N1 * 100,
        perr = round(per, digits = 1),
        first = "(",
        last = "%)",
        percent = paste(n, first, perr, last)
      ) %>%
      select(TRT01P, .data[[vv]], percent) %>%
      pivot_wider(names_from = "TRT01P", values_from = "percent") %>%
      rename(variable = vv) %>%
      mutate(ord = ord_)
    
    return(per)
  }
  
  # Generate summaries
  summary_age <- new_function(var = "AGE", var1 = "TRT01P", ord_ = 1)
  summary_height <- new_function(var = "HEIGHT", var1 = "TRT01P", ord_ = 5)
  summary_weight <- new_function(var = "WEIGHT", var1 = "TRT01P", ord_ = 6)
  count_sex <- my_function(vv = "SEX", ord_ = 2)
  count_race <- my_function(vv = "RACE", ord_ = 3)
  count_ethnic <- my_function(vv = "ETHNIC", ord_ = 4)
  
  # Combine all
  fin <- rbind(summary_age, summary_height, summary_weight, count_sex, count_race, count_ethnic) %>%
    mutate(label = ifelse(variable != "NA", paste("  ", variable), variable))
  
  # Create labels
  df <- data.frame(
    label = c("Age (years)", "Sex (n)", "Race", "Ethnicity", "Height (cm)", "Weight (kg)"),
    ord = c(0.1, 1.9, 2.9, 3.9, 4.9, 5.9),
    stringsAsFactors = FALSE
  )
  
  combined <- bind_rows(fin, df) %>%
    arrange(ord) %>%
    select(-ord, -variable)
  
  # Reorder columns to put label first
  col_order <- c("label", setdiff(names(combined), "label"))
  combined <- combined[, col_order]
  
  return(combined)
}

#--------------------------------------------------
# Prepare Base Data
#--------------------------------------------------
adsl_base <- adsl

#--------------------------------------------------
# Teal Data
#--------------------------------------------------
data_teal <- teal_data(ADSL = adsl_base)

#--------------------------------------------------
# Custom Demographics Module with SAFFL and RANDFL Filters Only
#--------------------------------------------------
tm_demog_table <- function(label = "Demographics Table", dataname = "ADSL") {
  
  module(
    label = label,
    server = function(id, data) {
      moduleServer(id, function(input, output, session) {
        
        # Filter UI - Only SAFFL and RANDFL
        output$filter_ui <- renderUI({
          df <- data()[[dataname]]
          tagList(
            selectInput(
              session$ns("saffl_filter"),
              "Safety Flag (SAFFL):",
              choices = c("All", sort(unique(df$SAFFL))),
              selected = "All"
            ),
            selectInput(
              session$ns("randfl_filter"),
              "Randomized Flag (RANDFL):",
              choices = c("All", sort(unique(df$RANDFL))),
              selected = "All"
            )
          )
        })
        
        # Filtered Data - Only SAFFL and RANDFL
        filtered_data <- reactive({
          req(input$saffl_filter, input$randfl_filter)
          
          df <- data()[[dataname]]
          
          if (input$saffl_filter != "All") {
            df <- df %>% filter(SAFFL == input$saffl_filter)
          }
          if (input$randfl_filter != "All") {
            df <- df %>% filter(RANDFL == input$randfl_filter)
          }
          
          df
        })
        
        # Generate Demographics Table
        demog_table <- reactive({
          df <- filtered_data()
          
          if (nrow(df) == 0) {
            return(data.frame(label = "No data available for selected filters"))
          }
          
          create_demog_table(df)
        })
        
        # Render Table
        output$demog_table <- renderTable({
          demog_table()
        }, striped = TRUE, hover = TRUE, bordered = TRUE, na = "")
        
        # Subject Count
        output$subject_count <- renderText({
          paste0("Total Subjects: ", nrow(filtered_data()))
        })
        
      })
    },
    ui = function(id) {
      ns <- NS(id)
      tagList(
        fluidRow(
          column(
            3,
            wellPanel(
              h4("Population Filters"),
              uiOutput(ns("filter_ui"))
            )
          ),
          column(
            9,
            h3("Table 14.1.1 Demographics Summary"),
            textOutput(ns("subject_count")),
            br(),
            tableOutput(ns("demog_table"))
          )
        )
      )
    }
  )
}

#--------------------------------------------------
# Run Teal App
#--------------------------------------------------
app <- init(
  data = data_teal,
  modules = modules(
    tm_demog_table(
      label = "Demographics Table",
      dataname = "ADSL"
    )
  )
)

shiny::shinyApp(app$ui, app$server)
