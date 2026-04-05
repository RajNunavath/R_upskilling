install.packages(c("haven","shiny","dplyr","DT","openxlsx","readr"))
library(haven)
set.seed(100)

# ---------------- DM
DM <- data.frame(
  STUDYID = "ABC123",
  USUBJID = paste0("SUBJ", 1:50),
  SEX = sample(c("M","F","X"), 50, TRUE), # X invalid
  ARM = sample(c("Placebo","Treatment","TestArm"), 50, TRUE),
  AGE = sample(18:70, 50, TRUE)
)

# ---------------- AE
AE <- data.frame(
  STUDYID = "ABC123",
  USUBJID = sample(DM$USUBJID, 200, TRUE),
  AEDECOD = sample(c("HEADACHE","FEVER","COUGH"), 200, TRUE),
  AESTDTC = sample(seq.Date(as.Date("2023-01-01"), as.Date("2023-03-01"), by="day"), 200, TRUE),
  AEENDTC = sample(seq.Date(as.Date("2023-01-01"), as.Date("2023-03-01"), by="day"), 200, TRUE)
)
AE$AEENDTC[1:6] <- AE$AESTDTC[1:6] - 3 # date issue

# ---------------- LB
LB <- data.frame(
  STUDYID = "ABC123",
  USUBJID = sample(DM$USUBJID, 150, TRUE),
  LBTESTCD = sample(c("HGB","WBC","PLT"), 150, TRUE),
  LBORRES = sample(c(NA, 10:20), 150, TRUE),
  LBSTAT = sample(c("", "NOT DONE"), 150, TRUE)
)

# ---------------- VS
VS <- data.frame(
  STUDYID = "ABC123",
  USUBJID = sample(DM$USUBJID, 120, TRUE),
  VISIT = sample(c("Visit 1","Visit 2"), 120, TRUE),
  VSTESTCD = sample(c("SYSBP","DIABP"), 120, TRUE),
  VSORRES = sample(80:180, 120, TRUE)
)

# duplicate key issue
VS <- rbind(VS, VS[1:5, ])

# ---------------- EX
EX <- data.frame(
  STUDYID = "ABC123",
  USUBJID = sample(DM$USUBJID, 100, TRUE),
  EXTRT = sample(c("DrugA","DrugB"), 100, TRUE),
  EXSTDTC = sample(seq.Date(as.Date("2023-01-01"), as.Date("2023-02-01"), by="day"), 100, TRUE)
)

write_xpt(DM, "DM.xpt")
write_xpt(AE, "AE.xpt")
write_xpt(LB, "LB.xpt")
write_xpt(VS, "VS.xpt")
write_xpt(EX, "EX.xpt")


define_meta <- data.frame(
  Domain=c("DM","AE","LB","VS","EX"),
  RequiredVars=c(
    "USUBJID,SEX,ARM",
    "USUBJID,AESTDTC,AEENDTC",
    "USUBJID,LBORRES,LBSTAT",
    "USUBJID,VISIT,VSTESTCD",
    "USUBJID,EXSTDTC"
  )
)

write.csv(define_meta,"define_metadata.csv",row.names=FALSE)
library(shiny)
library(haven)
library(dplyr)
library(DT)
library(openxlsx)
library(readr)

ui <- fluidPage(
  titlePanel("Ultimate SDTM Pinnacle-Style Reviewer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Upload SDTM XPT Files", multiple = TRUE),
      fileInput("define", "Upload Define Metadata CSV"),
      downloadButton("dl", "Download Findings Excel")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", DTOutput("summary")),
        tabPanel("Define vs Dataset", DTOutput("definecheck")),
        tabPanel("Duplicate Keys", DTOutput("dupcheck")),
        tabPanel("Validation Findings", DTOutput("findings"))
      )
    )
  )
)

server <- function(input, output) {
  
  sdtm <- reactive({
    req(input$files)
    lst <- list()
    for(i in 1:nrow(input$files)){
      nm <- tools::file_path_sans_ext(input$files$name[i])
      lst[[nm]] <- read_xpt(input$files$datapath[i])
    }
    lst
  })
  
  define <- reactive({
    req(input$define)
    read_csv(input$define$datapath)
  })
  
  # ---------------- Summary
  output$summary <- renderDT({
    lst <- sdtm()
    df <- lapply(names(lst), function(d){
      data.frame(
        Domain=d,
        Records=nrow(lst[[d]]),
        Subjects=length(unique(lst[[d]]$USUBJID)),
        Vars=ncol(lst[[d]])
      )
    }) %>% bind_rows()
    datatable(df)
  })
  
  # ---------------- Define Check
  output$definecheck <- renderDT({
    lst <- sdtm()
    def <- define()
    res <- data.frame()
    
    for(i in 1:nrow(def)){
      d <- def$Domain[i]
      reqvars <- unlist(strsplit(def$RequiredVars[i],","))
      
      if(d %in% names(lst)){
        miss <- setdiff(reqvars, names(lst[[d]]))
        res <- rbind(res, data.frame(
          Domain=d,
          MissingVars=paste(miss, collapse=", ")
        ))
      }
    }
    datatable(res)
  })
  
  # ---------------- Duplicate Keys (VS logic)
  output$dupcheck <- renderDT({
    lst <- sdtm()
    if("VS" %in% names(lst)){
      vs <- lst$VS
      dup <- vs %>%
        group_by(USUBJID, VISIT, VSTESTCD) %>%
        filter(n()>1)
      
      datatable(data.frame(
        Domain="VS",
        DuplicateRecords=nrow(dup)
      ))
    }
  })
  
  # ---------------- Findings
  findings <- reactive({
    lst <- sdtm()
    out <- data.frame()
    
    if("AE" %in% names(lst)){
      ae <- lst$AE
      bad <- sum(as.Date(ae$AESTDTC) > as.Date(ae$AEENDTC), na.rm=TRUE)
      out <- rbind(out, data.frame(Issue="AE Date Error",Domain="AE",Count=bad))
    }
    
    if("DM" %in% names(lst)){
      dm <- lst$DM
      wrong <- dm %>% filter(!(SEX %in% c("M","F","U")))
      out <- rbind(out, data.frame(Issue="Invalid SEX",Domain="DM",Count=nrow(wrong)))
    }
    
    if("LB" %in% names(lst)){
      lb <- lst$LB
      miss <- lb %>% filter(is.na(LBORRES) & LBSTAT=="")
      out <- rbind(out, data.frame(Issue="LB Missing Result",Domain="LB",Count=nrow(miss)))
    }
    
    out
  })
  
  output$findings <- renderDT({
    datatable(findings(), options=list(rowCallback=JS(
      "function(row,data){ if(data[2]>0){ $(row).css('background-color','#ffcccc');}}"
    )))
  })
  
  output$dl <- downloadHandler(
    filename="SDTM_Findings.xlsx",
    content=function(file){
      write.xlsx(findings(), file)
    }
  )
}

shinyApp(ui, server)

