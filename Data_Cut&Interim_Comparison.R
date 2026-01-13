install.packages("shinythemes")
install.packages("DT")
library(shiny)
library(data.table)
library(DT)
library(shinythemes)

# -----------------------------
# Dummy Data Creation
# -----------------------------

create_dummy_data <- function(version = "old") {
  
  set.seed(123)
  
  dt <- data.table(
    USUBJID = paste0("SUBJ", sprintf("%03d", 1:10)),
    PARAMCD = rep(c("ALT", "AST"), each = 5),
    ADT = as.Date("2023-01-01") + 1:10,
    AVAL = round(runif(10, 20, 100), 1)
  )
  
  if (version == "new") {
    
    # Modify values
    dt[USUBJID == "SUBJ003" & PARAMCD == "ALT", AVAL := AVAL + 15]
    
    # Delete one record
    dt <- dt[!(USUBJID == "SUBJ002" & PARAMCD == "AST")]
    
    # Add new subject
    dt <- rbind(
      dt,
      data.table(
        USUBJID = "SUBJ011",
        PARAMCD = "ALT",
        ADT = as.Date("2023-01-15"),
        AVAL = 55.5
      )
    )
  }
  
  dt
}

old_data <- create_dummy_data("old")
new_data <- create_dummy_data("new")

# -----------------------------
# Comparison Function
# -----------------------------

compare_data_cuts <- function(old_dt, new_dt, keys) {
  
  old_dt[, KEY := do.call(paste, c(.SD, sep = "|")), .SDcols = keys]
  new_dt[, KEY := do.call(paste, c(.SD, sep = "|")), .SDcols = keys]
  
  new_records <- new_dt[!KEY %in% old_dt$KEY]
  new_records[, CHANGE_TYPE := "New"]
  
  deleted_records <- old_dt[!KEY %in% new_dt$KEY]
  deleted_records[, CHANGE_TYPE := "Deleted"]
  
  common_old <- old_dt[KEY %in% new_dt$KEY]
  common_new <- new_dt[KEY %in% old_dt$KEY]
  
  merged <- merge(
    common_old, common_new,
    by = "KEY",
    suffixes = c("_OLD", "_NEW")
  )
  
  modified <- merged[
    AVAL_OLD != AVAL_NEW,
    .(USUBJID = USUBJID_OLD,
      PARAMCD = PARAMCD_OLD,
      ADT = ADT_OLD,
      AVAL_OLD,
      AVAL_NEW,
      CHANGE_TYPE = "Modified")
  ]
  
  list(
    new = new_records,
    deleted = deleted_records,
    modified = modified
  )
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("📊 Data Cut & Interim Comparison Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Dummy ADaM-like data comparison"),
      textInput("keys", "Key Variables",
                value = "USUBJID,PARAMCD,ADT"),
      actionButton("compare", "Run Comparison")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 verbatimTextOutput("summary")),
        tabPanel("New Records",
                 DTOutput("new_tbl")),
        tabPanel("Deleted Records",
                 DTOutput("deleted_tbl")),
        tabPanel("Modified Records",
                 DTOutput("modified_tbl"))
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output) {
  
  comparison <- eventReactive(input$compare, {
    
    keys <- trimws(unlist(strsplit(input$keys, ",")))
    
    compare_data_cuts(
      old_dt = copy(old_data),
      new_dt = copy(new_data),
      keys = keys
    )
  })
  
  output$summary <- renderPrint({
    req(comparison())
    cat("New Records:", nrow(comparison()$new), "\n")
    cat("Deleted Records:", nrow(comparison()$deleted), "\n")
    cat("Modified Records:", nrow(comparison()$modified), "\n")
  })
  
  output$new_tbl <- renderDT({
    datatable(comparison()$new)
  })
  
  output$deleted_tbl <- renderDT({
    datatable(comparison()$deleted)
  })
  
  output$modified_tbl <- renderDT({
    datatable(comparison()$modified)
  })
}

shinyApp(ui, server)
