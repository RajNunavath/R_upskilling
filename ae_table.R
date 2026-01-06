#set up the adsl data
adsl1<-adsl %>% 
  filter(ARM!="SCREEN FAILURE") %>% 
  arrange(ARM)


# adding total row for treatment variable
adsl_tot<-adsl %>% 
  filter(ARM!="SCREEN FAILURE") %>% 
  group_by(ARM) %>% 
  mutate(ARM="Total")

#combining both datasets for getting total count
# taking bign count
ad_total<-rbind(adsl1,adsl_tot)

bign<-ad_total %>% 
  filter(ARM!="Screen Failure") %>% 
  group_by(ARM) %>% 
  count(ARM) %>% 
  mutate(N1=n) %>% 
  select(-n)

#set up the ae data
adae<-adae %>% 
  # mutate(TRT101P=TRTP) %>% 
  filter(ARM!="SCREEN FAILURE") %>% 
  arrange(ARM)

adae1<-adae %>% 
  filter(ARM!="SCREEN FAILURE") %>% 
  group_by(ARM) %>% 
  mutate(ARM="Total")

adae_tot <- rbind(adae,adae1)

# count for subject atleat one TEAE
data1<-adae_tot %>% 
  arrange(USUBJID,ARM) %>% 
  distinct(USUBJID,ARM) %>% 
  count(ARM)

# merge the data with adsl to caculate percentage
one<- bign %>% 
  group_by(ARM) %>% 
  left_join(data1,by="ARM") %>% 
  mutate(tot=n/N1*100,
         per=round(tot,digit=1),
         first="(",
         last=")",
         percent=paste(n,first,per,last)) %>% 
  select(ARM,percent)  %>% 
  pivot_wider(names_from = "ARM", values_from = "percent") %>% 
  mutate(ORD=0,LABEL="Any TEAE")


# count for aebodsys
# count for subject atleat one TEAE
data2<-adae_tot %>% 
  arrange(USUBJID,ARM,AEBODSYS) %>% 
  distinct(USUBJID,ARM,AEBODSYS) %>% 
  count(ARM,AEBODSYS)

two<- bign %>% 
  group_by(ARM) %>% 
  left_join(data2,by="ARM") %>% 
  mutate(tot=n/N1*100,
         per=round(tot,digit=1),
         first="(",
         last=")",
         percent=paste(n,first,per,last)) %>% 
  select(ARM,AEBODSYS,percent)  %>% 
  pivot_wider(names_from = "ARM", values_from = "percent") %>% 
  mutate(ORD=1,LABEL=AEBODSYS)

# count for aebodsys and prefered term wise
data3<-adae_tot %>% 
  arrange(USUBJID,ARM,AEBODSYS,AEDECOD) %>% 
  distinct(USUBJID,ARM,AEBODSYS,AEDECOD) %>% 
  count(ARM,AEBODSYS,AEDECOD)

three<- bign %>% 
  group_by(ARM) %>% 
  left_join(data3,by="ARM") %>% 
  mutate(tot=n/N1*100,
         per=round(tot,digit=1),
         first="(",
         last=")",
         percent=paste(n,first,per,last)) %>% 
  select(ARM,AEBODSYS,AEDECOD,percent)  %>% 
  pivot_wider(names_from = "ARM", values_from = "percent") %>% 
  mutate(ORD=2,LABEL=paste("   ",AEDECOD))

# combing all dataset to bring final structure
final<-bind_rows(two,three) %>% 
  arrange(AEBODSYS,ORD,AEDECOD,LABEL)
# mutate(ne_column=ifelse(LABEL=="NA",AEBODSYS,AEDECOD))

final1<-bind_rows(one,final) %>% 
  select(-AEDECOD,-AEBODSYS)

