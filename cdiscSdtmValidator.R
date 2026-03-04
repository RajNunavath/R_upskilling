library(shiny)
library(DT)
library(haven)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("CDISC SDTM Validation App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Upload SDTM XPT Files",
                multiple = TRUE,
                accept = ".xpt"),
      actionButton("run", "Run Validation")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Validation Issues", DTOutput("issues")),
        tabPanel("Severity Summary", plotOutput("severity_plot")),
        tabPanel("Domain Record Count", plotOutput("domain_plot"))
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$run, {
    
    req(input$files)
    
    # Read datasets
    sdtm_data <- input$files %>%
      setNames(.$name) %>%
      map(~ haven::read_xpt(.x))
    
    issues <- data.frame(
      Domain = character(),
      Rule = character(),
      Severity = character(),
      stringsAsFactors = FALSE
    )
    
    uploaded_domains <- tools::file_path_sans_ext(input$files$name)
    
    #############################
    # REQUIRED DOMAIN CHECK
    #############################
    required_domains <- c("DM","AE","EX","LB","VS","MH","CM")
    
    missing_domains <- setdiff(required_domains, uploaded_domains)
    
    if(length(missing_domains) > 0){
      issues <- rbind(issues,
                      data.frame(
                        Domain = paste(missing_domains, collapse=","),
                        Rule = "Required Domain Missing",
                        Severity = "Error"
                      ))
    }
    
    #############################
    # DM CHECKS
    #############################
    if("DM.xpt" %in% input$files$name){
      dm <- sdtm_data[["DM.xpt"]]
      
      # Duplicate USUBJID
      if(any(duplicated(dm$USUBJID))){
        issues <- rbind(issues,
                        data.frame(Domain="DM",
                                   Rule="Duplicate USUBJID",
                                   Severity="Error"))
      }
      
      # SEX CT check
      if(any(!dm$SEX %in% c("M","F","U"))){
        issues <- rbind(issues,
                        data.frame(Domain="DM",
                                   Rule="Invalid SEX values",
                                   Severity="Error"))
      }
    }
    
    #############################
    # CROSS DOMAIN CHECKS
    #############################
    if(all(c("DM.xpt","AE.xpt") %in% input$files$name)){
      
      dm <- sdtm_data[["DM.xpt"]]
      ae <- sdtm_data[["AE.xpt"]]
      
      invalid_ae <- setdiff(ae$USUBJID, dm$USUBJID)
      
      if(length(invalid_ae) > 0){
        issues <- rbind(issues,
                        data.frame(Domain="AE",
                                   Rule="AE.USUBJID not in DM",
                                   Severity="Error"))
      }
    }
    
    #############################
    # EX DATE CHECK
    #############################
    if("EX.xpt" %in% input$files$name){
      ex <- sdtm_data[["EX.xpt"]]
      
      if(any(ex$EXENDTC < ex$EXSTDTC, na.rm=TRUE)){
        issues <- rbind(issues,
                        data.frame(Domain="EX",
                                   Rule="EXENDTC before EXSTDTC",
                                   Severity="Error"))
      }
    }
    
    #############################
    # OUTPUT TABLE
    #############################
    output$issues <- renderDT(issues)
    
    #############################
    # SEVERITY SUMMARY
    #############################
    severity_summary <- issues %>%
      group_by(Severity) %>%
      summarise(Count=n())
    
    output$severity_plot <- renderPlot({
      ggplot(severity_summary,
             aes(x=Severity,y=Count,fill=Severity))+
        geom_col()+
        theme_minimal()+
        labs(title="Validation Issues by Severity")
    })
    
    #############################
    # DOMAIN RECORD COUNT
    #############################
    domain_counts <- data.frame(
      Domain = uploaded_domains,
      Records = map_int(sdtm_data, nrow)
    )
    
    output$domain_plot <- renderPlot({
      ggplot(domain_counts,
             aes(x=Domain,y=Records))+
        geom_col(fill="steelblue")+
        theme_minimal()+
        labs(title="Record Count by Domain")
    })
    
  })
}

shinyApp(ui, server)
