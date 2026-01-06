
rm(list=ls())
rm(list=ls())
install.packages("tidyverse")
install.packages("r2rtf")
install.packages("admiral")
install.packages("pharmaversesdtm")
install.packages("pharmaverseadam")
library(tidyverse)
library(r2rtf)
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(pharmaverseadam)
library(lubridate)
library(stringr)  
#set up ADSL data
adsl<-adsl %>% 
  filter(TRT01P!="Screen Failure")
asl<-adsl %>% 
  filter(TRT01P!="Screen Failure") %>% 
  arrange(TRT01P) %>%
  mutate(TRT01P="Total")
asl
tot_ <- bind_rows(asl,adsl)
#bringing height weight bmi from vs data
vs1 <- convert_blanks_to_na(vs)
vss<-vs %>%
  group_by(USUBJID, VSTESTCD) %>%
  summarise(VSORRES = mean(as.numeric(VSORRES), na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = VSTESTCD,
    values_from = VSORRES
  )
#merging with adsl data
ad_vs<-left_join(tot_,vss,by="USUBJID")

biggn<-ad_vs %>% 
  filter(!TRT01P=="Screen Failure") %>% 
  group_by(TRT01P) %>% 
  count(TRT01P) %>% 
  mutate(N1=n) %>% 
  select(-n)

new_function <- function(var, var1,ord_) {
  # Calculate summary statistics grouped by the variable 'var1'
  summary_stats <- ad_vs %>%
    filter(TRT01P!="Screen Failure") %>% 
    group_by(.data[[var1]]) %>%
    mutate(
      n = as.character(n()),
      mean = format(round(mean(.data[[var]], na.rm = TRUE)),nsmall=2),
      median = format(trunc(median(.data[[var]]), na.rm = TRUE),nsmall=2),
      min = as.character(trunc(min(.data[[var]]), na.rm = TRUE),nsmall=0),
      max = as.character(trunc(max(.data[[var]]), na.rm = TRUE),nsmall=0),
      std = format(round(sd(.data[[var]], na.rm = TRUE)),nsmall=3)
    ) %>% 
    distinct(n,mean,median,min,std,max) %>% 
    select(n, mean, median, std, min, max, var1) %>%
    pivot_longer(cols = c(n, mean, median, std, min, max), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = var1, values_from = value) %>% 
    mutate(ord=ord_)
  
  
  
  # Return the summary statistics as a new data frame
  return(summary_stats)
}
# Call the function for 'age' grouped by 'sex'
summary_age <- new_function(var = "AGE", var1 = "TRT01P",ord_=1)
summary_height <- new_function(var = "HEIGHT", var1 = "TRT01P",ord_=5)
summary_weight <- new_function(var = "WEIGHT", var1 = "TRT01P",ord_=6)
#summary_age_sex4 <- new_function(var = "TRTDURD", var1 = "TRT01P")
# Print the result
print(summary_age)


#creating function for char variables

my_function<-function(vv,ord_){
  char<- tot_ %>% 
    filter(!TRT01P=="Screen Failure") %>% 
    group_by(TRT01P) %>% 
    count(.data[[vv]]) 
  
  #per<-right_join(x=biggn,y=data2,by="ARM") 
  per<-biggn %>% 
    group_by(TRT01P) %>% 
    right_join(char,by="TRT01P") %>% 
    mutate(per=n/N1*100,
           perr=round(per,digits=1),
           first="(",
           last="%)",
           percent=paste(n,first,perr,last)) %>% 
    select(TRT01P,.data[[vv]],percent)  %>% 
    pivot_wider(names_from = "TRT01P", values_from = "percent") %>% 
    rename(variable=vv) %>% 
    mutate(ord=ord_)
  
  return(per)
}
count_sex<-my_function(vv="SEX",ord_=2)
count_race<-my_function(vv="RACE",ord_=3)
count_ethnic<-my_function(vv="ETHNIC",ord_=4)

#combing all datasets
fin<-rbind(summary_age,summary_height,summary_weight,count_sex,count_race,count_ethnic) %>% 
  mutate(label=ifelse(variable!="NA",paste("  ",variable),variable))
#creating dummy lables
df <- data.frame(
  label = c("Age (years)", "Sex (n)", "Race", "Ethnicity", "Height (cm)","Weight (kg)"),
  ord = c(0.1, 1.9,2.9,3.9,4.9,5.9),
  stringsAsFactors = FALSE
)
combined <- bind_rows(fin, df) %>% 
  arrange(ord)
