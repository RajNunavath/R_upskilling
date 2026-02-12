#-------------------------------------------------------
# Load Libraries
#-------------------------------------------------------
library(pharmaverseadam)
library(pharmaversesdtm)
library(dplyr)
library(r2rtf)

#-------------------------------------------------------
# Load Example SDTM and ADaM Data
#-------------------------------------------------------
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

dm <- pharmaversesdtm::dm
ae <- pharmaversesdtm::ae
cm <- pharmaversesdtm::cm
lb <- pharmaversesdtm::lb
mh <- pharmaversesdtm::mh
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex

#-------------------------------------------------------
# Initialize QC Results Data Frame
#-------------------------------------------------------
qc_results <- data.frame(
  QC_Check = character(),
  Status = character(),
  Details = character(),
  stringsAsFactors = FALSE
)

#-------------------------------------------------------
# Function: Check Required Variables
#-------------------------------------------------------
check_required_vars <- function(df, domain, required_vars) {
  missing_vars <- setdiff(required_vars, names(df))
  if (length(missing_vars) == 0) {
    return(c(paste(domain, "Required Variables"), "PASS", "All required variables present"))
  } else {
    return(c(paste(domain, "Required Variables"), "FAIL", paste("Missing:", paste(missing_vars, collapse=", "))))
  }
}

#-------------------------------------------------------
# Function: Check Duplicates
#-------------------------------------------------------
check_duplicates <- function(df, domain, key_var) {
  dup <- df %>% group_by(across(all_of(key_var))) %>% filter(n() > 1)
  if (nrow(dup) == 0) {
    return(c(paste(domain, "Duplicate Check"), "PASS", "No duplicates found"))
  } else {
    return(c(paste(domain, "Duplicate Check"), "FAIL", paste("Duplicate Subjects:", paste(unique(dup[[key_var]]), collapse=", "))))
  }
}

#-------------------------------------------------------
# Function: Check Missing Numeric Variable
#-------------------------------------------------------
check_missing_numeric <- function(df, domain, var) {
  missing_count <- sum(is.na(df[[var]]))
  if (missing_count == 0) {
    return(c(paste(domain, var, "Missing Check"), "PASS", paste("No missing", var, "values")))
  } else {
    return(c(paste(domain, var, "Missing Check"), "WARNING", paste("Missing", var, "count:", missing_count)))
  }
}

#-------------------------------------------------------
# List of Domains and Checks
#-------------------------------------------------------
domains_checks <- list(
  list(df = dm, domain = "DM", req_vars = c("STUDYID", "USUBJID", "AGE", "SEX", "RACE"), dup_var = "USUBJID", numeric_vars = c("AGE")),
  list(df = ae, domain = "AE", req_vars = c("STUDYID", "USUBJID", "AESEQ", "AETERM"), dup_var = "AESEQ", numeric_vars = c()),
  list(df = cm, domain = "CM", req_vars = c("STUDYID", "USUBJID", "CMSEQ", "CMTRT"), dup_var = "CMSEQ", numeric_vars = c()),
  list(df = lb, domain = "LB", req_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD", "LBSTRESN"), dup_var = "LBSEQ", numeric_vars = c("LBSTRESN")),
  list(df = mh, domain = "MH", req_vars = c("STUDYID", "USUBJID", "MHSEQ", "MHTERM"), dup_var = "MHSEQ", numeric_vars = c()),
  list(df = vs, domain = "VS", req_vars = c("STUDYID", "USUBJID", "VSSSEQ", "VSTESTCD", "VSSTRESN"), dup_var = "VSSSEQ", numeric_vars = c("VSSTRESN")),
  list(df = ex, domain = "EX", req_vars = c("STUDYID", "USUBJID", "EXSEQ", "EXTRT"), dup_var = "EXSEQ", numeric_vars = c())
)

#-------------------------------------------------------
# Perform QC Checks
#-------------------------------------------------------
for (chk in domains_checks) {
  df <- chk$df
  domain <- chk$domain
  # Required variables
  qc_results <- rbind(qc_results, check_required_vars(df, domain, chk$req_vars))
  # Duplicate check
  qc_results <- rbind(qc_results, check_duplicates(df, domain, chk$dup_var))
  # Missing numeric variables
  if (length(chk$numeric_vars) > 0) {
    for (var in chk$numeric_vars) {
      qc_results <- rbind(qc_results, check_missing_numeric(df, domain, var))
    }
  }
}

#-------------------------------------------------------
# ADSL vs DM Subject Count Check
#-------------------------------------------------------
dm_count <- length(unique(dm$USUBJID))
adsl_count <- length(unique(adsl$USUBJID))

qc_results <- rbind(qc_results,
                    c("DM vs ADSL Subject Count",
                      ifelse(dm_count == adsl_count, "PASS", "WARNING"),
                      paste("DM:", dm_count, "| ADSL:", adsl_count)))

#-------------------------------------------------------
# AE Subjects vs DM Check
#-------------------------------------------------------
ae_only <- setdiff(unique(ae$USUBJID), unique(dm$USUBJID))
qc_results <- rbind(qc_results,
                    c("AE vs DM Subject Consistency",
                      ifelse(length(ae_only) == 0, "PASS", "FAIL"),
                      ifelse(length(ae_only) == 0, "All AE subjects exist in DM",
                             paste("AE subjects not in DM:", paste(ae_only, collapse=", ")))))


#-------------------------------------------------------
# Convert QC Results to Proper Data Frame
#-------------------------------------------------------
qc_results <- as.data.frame(qc_results, stringsAsFactors = FALSE)
colnames(qc_results) <- c("QC Check", "Status", "Details")

#-------------------------------------------------------
# Generate RTF QC Report
#-------------------------------------------------------
qc_results %>%
  rtf_body() %>%
  rtf_title("Pre-Submission QC Validation Report (SDTM + ADaM)") %>%
  rtf_footnote("Generated using R-based QC script with pharmaverse datasets.") %>%
  rtf_encode() %>%
  write_rtf("Pharmaverse_QC_Report.rtf")

cat("QC RTF Report Generated Successfully at:", getwd(), "\n")
gt_table <- gt::gt(qc_results) %>%
  gt::tab_header(
    title = "Pre-Submission QC Validation Report"
  )

gt_table  # this will display in the Viewer
