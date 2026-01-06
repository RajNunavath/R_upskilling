raw_dm <- data.frame(
  STUDYID = "ABC123",
  SITEID  = c("001", "001", "002", "002"),
  SUBJID  = c("001-001", "001-002", "002-001", "002-002"),
  INVID   = c("INV01", "INV01", "INV02", "INV02"),
  INVNAM  = c("Dr. Smith", "Dr. Smith", "Dr. Lopez", "Dr. Lopez"),
  USUBJID = c("ABC123-001-001", "ABC123-001-002",
              "ABC123-002-001", "ABC123-002-002"),
  SEX     = c("M", "F", "M", "F"),
  RACE    = c("White", "Asian", "Black or African American", "White"),
  ETHNIC  = c("Not Hispanic or Latino", "Not Hispanic or Latino",
              "Hispanic or Latino", "Not Hispanic or Latino"),
  BRTHDTC = as.Date(c("1980-04-12","1975-10-01","1990-02-14","1988-07-08")),
  COUNTRY = c("USA", "USA", "CAN", "CAN"),
  ARM     = c("Placebo", "Active", "Active", "Placebo"),
  ARMCD   = c("PBO", "ACT", "ACT", "PBO"),
  RFICDTC = as.Date(c("2020-06-01","2020-06-03","2020-06-10","2020-06-11")),
  RANDDTC = as.Date(c("2020-06-05","2020-06-08","2020-06-12","2020-06-15"))
)

dm_vars <- c(
  "STUDYID","DOMAIN","USUBJID","SUBJID","RFSTDTC","RFENDTC",
  "RFXSTDTC","RFXENDTC","RFICDTC","RFPENDTC","DTHDTC","DTHFL",
  "SITEID","INVID","INVNAM","BRTHDTC","AGE","AGEU","SEX","RACE",
  "ETHNIC","ARMCD","ARM","ACTARMCD","ACTARM","COUNTRY","DMDTC",
  "DMDY"
)




### === RAW EX ===
raw_ex <- data.frame(
  USUBJID = c("ABC123-001-001","ABC123-001-001",
              "ABC123-001-002","ABC123-001-002"),
  EXTRT = c("Active","Active","Placebo","Placebo"),
  EXSTDTC = as.Date(c("2020-06-05","2020-06-06",
                      "2020-06-08","2020-06-10")),
  EXENDTC = as.Date(c("2020-07-05","2020-07-06",
                      "2020-07-08","2020-07-10"))
)


### === DERIVE RFSTDTC & RFENDTC FROM EX ===
ex_derive <- raw_ex %>%
  group_by(USUBJID) %>%
  summarise(
    RFSTDTC = min(EXSTDTC, na.rm = TRUE),
    RFENDTC = max(EXENDTC, na.rm = TRUE)
  )

### === BUILD SDTM DM DOMAIN ===



dm <- raw_dm %>%
  left_join(ex_derive, by = "USUBJID") %>%
  # ---- CORE DM ----
mutate(
  DOMAIN   = "DM",
  
  # Reference start/end (from randomization)
  
  RFXSTDTC = RFSTDTC,
  RFXENDTC = RFENDTC,
  
  RFPENDTC = NA,
  DTHDTC = NA,
  DTHFL  = NA,
  
  # Age derivation
  AGE = floor(as.numeric(RANDDTC - BRTHDTC) / 365.25),
  AGEU = "YEARS",
  
  # Actual Arm = Assigned Arm for dummy data
  ACTARM = ARM,
  ACTARMCD = ARMCD,
  
  # DM reference date = Informed Consent
  DMDTC = RFICDTC,
  
  # Study day calculation
  DMDY = as.numeric(DMDTC - RFSTDTC) + 1
) %>%
  
  # Keep only SDTM DM variables
  select(all_of(dm_vars))
