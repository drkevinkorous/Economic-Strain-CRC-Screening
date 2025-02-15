#===========================================================================================#
## Analysis Code
## Paper Title: Perceived Economic Strain, Subjective Social Status, and Colorectal Cancer 
##              Screening Utilization in U.S. Men––A Cross-Sectional Analysis
## Authors: Anonymous
#===========================================================================================#

library(naniar)
library(lavaan)
library(tidyverse)

## Data ----
# Load data 
sesuptake<-readRDS(file="crcsuptake_analytic file_OSF.RDS")

# Filter data (NHOTH should be filtered out due to lack of generalizing to a specific population)
sesuptake<-filter(sesuptake,sraceth!="NHOTH")

### Recode variables for analysis ----
## CRC Screening Uptake ----
miss_var_summary(select(sesuptake,crcsstool,crcsexam))
miss_case_summary(select(sesuptake,crcsstool,crcsexam))
sesuptake<-sesuptake%>%mutate_at(vars(crcsstool,crcsexam),~(case_when(
                                 . == "No" ~ 0,
                                 . == "Yes" ~ 1)))
table(sesuptake$crcsstool)
table(sesuptake$crcsexam)
# Tests Combined
sesuptake<-sesuptake%>%mutate(crcsuptake=case_when(crcsstool==1|crcsexam==1~1,
                                                   crcsstool==0&crcsexam==0~0))
table(sesuptake$crcsuptake)

# Up-to-Date with Screening (logical if answered Yes to a test)
table(sesuptake$crcsup2date)
# Completed & Up-to-Date
sesuptake<-sesuptake%>%mutate(crcsuptake2date=case_when(
                              crcsuptake==1&crcsup2date=="Yes"~1,
                              is.na(crcsuptake)~NA_real_,
                              T~0))

table(sesuptake$crcsuptake2date)

## Economic Strain Scale (Pearlin et al., 1981) ----
# Missingness
miss_var_summary(sesuptake[,c("endsmeet1","endsmeet2")])
miss_case_summary(sesuptake[,c("endsmeet1","endsmeet2")])
miss_var_summary(select(sesuptake,starts_with("unmet")))
miss_case_summary(select(sesuptake,starts_with("unmet")))

# Recode - Making Ends Meet Items
sesuptake<-sesuptake%>%mutate(endsmeet1=case_when(
                      endsmeet1=="No difficulty at all"~0,
                      endsmeet1=="A little difficulty"~1, 
                      endsmeet1=="Some difficulty"~2,
                      endsmeet1=="Quite a bit of difficulty"~3,
                      endsmeet1=="A great deal of difficulty"~4),
                      endsmeet2=case_when(
                      endsmeet2=="More than enough money left over"~0,
                      endsmeet2=="Some money left over"~1, 
                      endsmeet2=="Just enough money to make ends meet"~2,
                      endsmeet2=="Not enough money to make ends meet"~3))

# View summary
table(sesuptake$endsmeet1)
table(sesuptake$endsmeet2)
# Standardize and average approach (e.g., 10.1111/fare.12098,10.1177/019251392013001002)
sesuptake$endsmeet1_z<-scale(sesuptake$endsmeet1)
sesuptake$endsmeet2_z<-scale(sesuptake$endsmeet2)
sesuptake$endsmeetz<-rowMeans(cbind(sesuptake$endsmeet1_z,sesuptake$endsmeet2_z),na.rm=T)
summary(sesuptake$endsmeetz)

# Recode - Unmet Material Needs Items
sesuptake<-sesuptake%>%mutate_at(vars(starts_with("unmet")),~(case_when(
                         . == "Strongly agree" ~ 0,
                         . == "Agree" ~ 1,
                         . == "Neither agree nor disagree" ~ 2,
                         . == "Disagree" ~ 3,
                         . == "Strongly disagree" ~ 4)))
# View summary
summary(select(sesuptake,starts_with("unmet")))

# Omega for Reliability
#ci.reliability(data=sesuptake[,c("unmet1","unmet2","unmet3","unmet4","unmet5","unmet6","unmet7")],
#               type="omega", conf.level=0.95, interval.type="bca", B=1000)

# Mean Score as used in prior research
sesuptake$m_unmet<-rowMeans(select(sesuptake,starts_with("unmet")), na.rm = TRUE)
psych::describe(sesuptake$m_unmet)
psych::describeBy(sesuptake$m_unmet,group=sesuptake$crcsuptake)
sesuptake$z_unmet<-scale(sesuptake$m_unmet)

# Standardized individual items
sesuptake$umet1_z<-scale(sesuptake$unmet1)
sesuptake$umet2_z<-scale(sesuptake$unmet2)
sesuptake$umet3_z<-scale(sesuptake$unmet3)
sesuptake$umet4_z<-scale(sesuptake$unmet4)
sesuptake$umet5_z<-scale(sesuptake$unmet5)
sesuptake$umet6_z<-scale(sesuptake$unmet6)
sesuptake$umet7_z<-scale(sesuptake$unmet7)

## Economic Strain Latent Score
# Missing Completely at Random (MCAR) Test (Little, 1988)
mcar_test(select(sesuptake,unmet1,unmet2,unmet3,unmet4,unmet5,unmet6,unmet7,endsmeet1,endsmeet2))
# Create latent variable score (multistage factor score regression: see https://doi.org/10.3758/s13428-020-01398-0)
es.mod<-'esfs=~unmet1+unmet2+unmet3+unmet4+unmet5+unmet6+unmet7+endsmeet1+endsmeet2'
es.fit<-cfa(es.mod,missing="ML",data=sesuptake,std.lv=T)
summary(es.fit,fit.measures=T)
# Correlations between items
lavCor(es.fit)
# Add factor scores to dataframe
es_fs <- lavPredict(es.fit)
sesuptake<-bind_cols(sesuptake,es_fs)
varlabs<-c("Unmet\nHome","Unmet\nClothing","Unmet\nFurniture","Unmet\nCar",
           "Unmet\nFood","Unmet\nMedical","Unmet\nLeisure", "Difficulty\nPaying\nBills","Money\nLeft\nOver","Economic\nStrain")
semTools::reliability(es.fit)
semPlot::semPaths(es.fit,"std",intercepts=F,nodeLabels=varlabs,sizeMan = 8,style="lisrel",
                  sizeLat = 13,edge.label.cex=.9,edge.color="black",asize=2,esize=1,fade=F,font=1,edge.label.font=2)
title("Confirmatory Factor Anaylsis - Economic Strain Scale",family='sans',line=2.5)

# Summary of factor score
summary(sesuptake$esfs)
hist(sesuptake$esfs)
psych::describe(sesuptake$esfs)
psych::describeBy(sesuptake$esfs,group=sesuptake$crcsstool)
psych::describeBy(sesuptake$esfs,group=sesuptake$crcsexam)
psych::describeBy(sesuptake$esfs,group=sesuptake$crcsuptake2date)

## Economic Strain Standardized Z Score (sensitivity test)
sesuptake$esz<-rowMeans(cbind(sesuptake$endsmeet1_z,sesuptake$endsmeet2_z,
                              sesuptake$umet1_z,sesuptake$umet2_z,
                              sesuptake$umet3_z,sesuptake$umet4_z,
                              sesuptake$umet5_z,sesuptake$umet6_z,
                              sesuptake$umet7_z),na.rm=T)

summary(sesuptake$esz)

## Subjective Social Status (Ladder) ----
miss_var_summary(select(sesuptake,srsss))
miss_case_summary(select(sesuptake,srsss))
sesuptake<-sesuptake%>%mutate(srsss=case_when(
                              srsss=="1 (bottom of the ladder)"~1,
                              srsss=="2"~2,
                              srsss=="3"~3,
                              srsss=="4"~4,
                              srsss=="5"~5,
                              srsss=="6"~6,
                              srsss=="7"~7,
                              srsss=="8"~8,
                              srsss=="9"~9,
                              srsss=="10 (top of the ladder)"~10))
table(sesuptake$srsss)
hist(sesuptake$srsss)
psych::describe(sesuptake$srsss)
psych::describeBy(sesuptake$srsss,group=sesuptake$crcsstool)
psych::describeBy(sesuptake$srsss,group=sesuptake$crcsexam)
psych::describeBy(sesuptake$srsss,group=sesuptake$crcsuptake2date)

## Covariates ----
# Household Income
miss_var_summary(select(sesuptake,hhinc))
sesuptake<-sesuptake%>%mutate(hhinc=case_when(
                       hhinc=="Less than $20,000"~19999,
                       hhinc=="$20,000 to $24,000"~22000,
                       hhinc=="$25,000 to $29,000"~27000,   
                       hhinc=="$30,000 to $34,000"~32000,  
                       hhinc=="$35,000 to $39,000"~37000,   
                       hhinc=="$40,000 to $44,000"~42000,
                       hhinc=="$45,000 to $49,000"~47000,   
                       hhinc=="$50,000 to $74,000"~62000,  
                       hhinc=="$75,000 to $99,000"~87000, 
                       hhinc=="$100,000 to $124,000"~112000, 
                       hhinc=="$125,00 to $149,000"~137000,  
                       hhinc=="$150,000 or more"~150000))
summary(sesuptake$hhinc)
hist(sesuptake$hhinc)
psych::describe(sesuptake$hhinc)
psych::describeBy(sesuptake$hhinc,group=sesuptake$crcsstool)
psych::describeBy(sesuptake$hhinc,group=sesuptake$crcsexam)
psych::describeBy(sesuptake$hhinc,group=sesuptake$crcsuptake2date)
sesuptake$hhinc_z<-scale(sesuptake$hhinc)
summary(sesuptake$hhinc_z)

# Educational Attainment
miss_var_summary(select(sesuptake,edu))
table(sesuptake$edu)
sesuptake<-sesuptake%>%mutate(edu=factor(case_when(
                              edu=="8th grade or less"|                       
                              edu=="9th grade"|                      
                              edu=="10th grade"|                                    
                              edu=="11th grade"|                                    
                              edu=="12th grade, no diploma"~"Some High School",
                              edu=="GED or equivalent"|                             
                              edu=="High school graduate"~"High School Diploma or GED",
                              edu=="Some college, no degree" ~ "Some College",
                              edu=="Associate degree"|                              
                              edu=="Bachelor's degree (e.g., BA, BS, BBA)"~"College Degree",
                              edu=="Master's degree (e.g., MS, MPH, MBA)"|          
                              edu=="Professional school degree (e.g., MD, DDS, JD)"|
                              edu=="Doctoral degree (e.g., PhD, EdD, PsyD)"~"Advanced Degree"),
                              levels = c("Some High School","High School Diploma or GED","Some College",
                              "College Degree","Advanced Degree")))
table(sesuptake$edu)
sesuptake%>%group_by(crcsstool,edu)%>%summarise(n())
sesuptake%>%group_by(crcsexam,edu)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,edu)%>%summarise(n())

# Associations with economic strain and subjective social status
rcorr(as.matrix(sesuptake[,c("hhinc","esfs","srsss")],type='pearson'))
summary(aov(hhinc ~ edu, data = sesuptake))
boxplot(hhinc ~ edu, data = sesuptake)
sesuptake%>%group_by(edu)%>%summarise(mean=mean(hhinc,na.rm=T))
summary(aov(esfs ~ edu, data = sesuptake))
boxplot(esfs ~ edu, data = sesuptake)
sesuptake%>%group_by(edu)%>%summarise(mean=mean(esfs,na.rm=T))
summary(aov(srsss ~ edu, data = sesuptake))
boxplot(srsss ~ edu, data = sesuptake)
sesuptake%>%group_by(edu)%>%summarise(mean=mean(srsss,na.rm=T))

# Age stays at continuous (scale to make a unit change more meaningful)
summary(sesuptake$age)
psych::describe(sesuptake$age)
psych::describeBy(sesuptake$age,group=sesuptake$crcsstool)
psych::describeBy(sesuptake$age,group=sesuptake$crcsexam)
psych::describeBy(sesuptake$age,group=sesuptake$crcsuptake2date)
sesuptake$age_z<-scale(sesuptake$age)

# Age categorized
sesuptake<-sesuptake%>%mutate(agecat=case_when(
                             age>=45&age<50~"45 to 49",
                             age>=50&age<65~"50 to 64",
                             age>=65~"65 to 75"))
table(sesuptake$agecat)
sesuptake%>%group_by(crcsstool,agecat)%>%summarise(n())
sesuptake%>%group_by(crcsexam,agecat)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,agecat)%>%summarise(n())

# Self-identified race/ethnicity (keep categories as is)
miss_var_summary(sesuptake[,"sraceth"])
table(sesuptake$sraceth)
sesuptake%>%group_by(crcsstool,sraceth)%>%summarise(n())
sesuptake%>%group_by(crcsexam,sraceth)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,sraceth)%>%summarise(n())

# Relationship Status
miss_var_summary(sesuptake[,"relationship"])
table(sesuptake$relationship)
sesuptake<-sesuptake%>%mutate(relationship=as.factor(case_when(
                      relationship=="Married"|relationship=="In a relationship, but not married"~"Married or In Relationship",
                      relationship=="Divorced"|relationship=="Separated"|relationship=="Widowed"|relationship=="Single"~"Single, Divorced, Separated, or Widowed")))
table(sesuptake$relationship)
sesuptake%>%group_by(crcsstool,relationship)%>%summarise(n())
sesuptake%>%group_by(crcsexam,relationship)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,relationship)%>%summarise(n())

# Health Insurance (no recoding needed)
miss_var_summary(sesuptake[,"insurance"])
table(sesuptake$insurance)
sesuptake%>%group_by(crcsstool,insurance)%>%summarise(n())
sesuptake%>%group_by(crcsexam,insurance)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,insurance)%>%summarise(n())

# Family History of CRC 
miss_var_summary(sesuptake[,"crcfamily"])
sesuptake<-sesuptake%>%mutate(crcfamily=case_when(crcfamily=="Yes"~"Yes",
                                                  crcfamily=="No"|crcfamily=="Unsure"~"No or Unsure"))
table(sesuptake$crcfamily)
sesuptake%>%group_by(crcsstool,crcfamily)%>%summarise(n())
sesuptake%>%group_by(crcsexam,crcfamily)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,crcfamily)%>%summarise(n())

# Primary Care Physician 
miss_var_summary(sesuptake[,"pcare"])
table(sesuptake$pcare)
sesuptake%>%group_by(crcsstool,pcare)%>%summarise(n())
sesuptake%>%group_by(crcsexam,pcare)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,pcare)%>%summarise(n())

# Screening Recommended Past 12 Months (recode NAs as "No" because logic skipped this question)
sesuptake<-sesuptake%>%mutate(crcsadvice=replace_na(crcsadvice,"No"))
table(sesuptake$crcsadvice)
sesuptake%>%group_by(crcsstool,crcsadvice)%>%summarise(n())
sesuptake%>%group_by(crcsexam,crcsadvice)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,crcsadvice)%>%summarise(n())

# CRC Diagnosis (no recoding needed)
miss_var_summary(sesuptake[,"crcdiag"])
table(sesuptake$crcdiag)
sesuptake%>%group_by(crcsstool,crcdiag)%>%summarise(n())
sesuptake%>%group_by(crcsexam,crcdiag)%>%summarise(n())
sesuptake%>%group_by(crcsuptake2date,crcdiag)%>%summarise(n())

### Missing Data ----
gg_miss_var(select(sesuptake,age,sraceth,relationship,edu,insurance,pcare,crcdiag,crcfamily,
                   crcsadvice,hhinc,cutbacks,esfs,srsss,crcsstool,crcsexam,crcsuptake2date))
gg_miss_case(select(sesuptake,age,sraceth,relationship,edu,insurance,pcare,crcdiag,crcfamily,
                    crcsadvice,hhinc,cutbacks,esfs,srsss,crcsstool,crcsexam,crcsuptake2date))
miss_var_summary(select(sesuptake,age,sraceth,relationship,edu,insurance,pcare,crcdiag,crcfamily,
                        crcsadvice,hhinc,cutbacks,esfs,srsss,crcsstool,crcsexam,crcsuptake2date))
miss_case_summary(select(sesuptake,age,sraceth,relationship,edu,insurance,pcare,crcdiag,crcfamily,
                         crcsadvice,hhinc,cutbacks,esfs,srsss,crcsstool,crcsexam,crcsuptake2date))
pct_miss_case(select(sesuptake,age,sraceth,relationship,edu,insurance,pcare,crcdiag,crcfamily,
                     crcsadvice,hhinc,cutbacks,esfs,srsss,crcsstool,crcsexam,crcsuptake2date))
gg_miss_upset(select(sesuptake,age,sraceth,relationship,edu,insurance,pcare,crcdiag,crcfamily,
                     crcsadvice,hhinc,cutbacks,esfs,srsss,crcsstool,crcsexam,crcsuptake2date))

# Missing Completely at Random (MCAR) Test (Little, 1988)
mcar_test(select(sesuptake,age,sraceth,relationship,edu,insurance,pcare,crcdiag,crcfamily,
                 crcsadvice,hhinc,cutbacks,esfs,srsss,crcsstool,crcsexam,crcsuptake2date))

#### Logistic Regression ----
### Ever Completed Stool-Based Test ----
summary(es.esfs<-glm(crcsstool~esfs,data=sesuptake,family=binomial))
sink("es.esfs.txt")
print(exp(cbind(OR = coef(es.esfs), confint(es.esfs))))
sink()
summary(es.sss<-glm(crcsstool~srsss,data=sesuptake,family=binomial))
sink("es.sss.txt")
print(exp(cbind(OR = coef(es.sss), confint(es.sss))))
sink()
## Model 1 
es.1<-glm(crcsstool~esfs+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree"),
                    data=sesuptake,family=binomial)

summary(es.1)
car::vif(es.1)
sink("es.1.txt")
summary(es.1)
sink()
# Transform to ORs
es.1OR<-exp(cbind(OR = coef(es.1), confint(es.1)))
sink("es.1OR.txt")
print(es.1OR)
sink()

# Model 2
es.2<-glm(crcsstool~esfs+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree")+
                    relevel(factor(agecat),ref="50 to 64")+
                    relevel(factor(sraceth),ref="NHWHT")+
                    relevel(factor(insurance),ref="Yes")+
                    relevel(factor(relationship),ref="Married or In Relationship")+
                    relevel(factor(crcfamily),ref= "No or Unsure")+
                    relevel(factor(pcare),ref= "Yes")+
                    relevel(factor(crcsadvice),ref= "No"),
                    data=sesuptake,family=binomial)
summary(es.2)
car::vif(es.2)
sink("es.2.txt")
summary(es.2)
sink()
# Transform to ORs
es.2OR<-exp(cbind(OR = coef(es.2), confint(es.2)))
sink("es.2OR.txt")
print(es.2OR)
sink()

### Ever Completed Exam-Based Test ----
summary(ex.esfs<-glm(crcsexam~esfs,data=sesuptake,family=binomial))
sink("ex.esfs.txt")
print(exp(cbind(OR = coef(ex.esfs), confint(ex.esfs))))
sink()
summary(ex.sss<-glm(crcsexam~srsss,data=sesuptake,family=binomial))
sink("ex.sss.txt")
print(exp(cbind(OR = coef(ex.sss), confint(ex.sss))))
sink()
## Model 1 
ex.1<-glm(crcsexam~esfs+
                   srsss+
                   hhinc_z+
                   relevel(edu,ref="Advanced Degree"),
                   data=sesuptake,family=binomial)

summary(ex.1)
car::vif(ex.1)
sink("ex.1.txt")
summary(ex.1)
sink()
# Transform to ORs
ex.1OR<-exp(cbind(OR = coef(ex.1), confint(ex.1)))
sink("ex.1OR.txt")
print(exp(cbind(OR = coef(ex.1), confint(ex.1))))
sink()

# Model 2
ex.2<-glm(crcsexam~esfs+
                   srsss+
                   hhinc_z+
                   relevel(edu,ref="Advanced Degree")+
                   relevel(factor(agecat),ref="50 to 64")+
                   relevel(factor(sraceth),ref="NHWHT")+
                   relevel(factor(insurance),ref="Yes")+
                   relevel(factor(relationship),ref="Married or In Relationship")+
                   relevel(factor(crcfamily),ref= "No or Unsure")+
                   relevel(factor(pcare),ref= "Yes")+
                   relevel(factor(crcsadvice),ref= "No"),
                   data=sesuptake,family=binomial)
summary(ex.2)
car::vif(ex.2)
sink("ex.2.txt")
summary(ex.2)
sink()
# Transform to ORs
ex.2OR<-exp(cbind(OR = coef(ex.2), confint(ex.2)))
sink("ex.2OR.txt")
print(ex.2OR)
sink()

### Up-to-Date with Screening ----
summary(up.esfs<-glm(crcsuptake2date~esfs,data=sesuptake,family=binomial))
sink("up.esfs.txt")
print(exp(cbind(OR = coef(up.esfs), confint(up.esfs))))
sink()
summary(up.sss<-glm(crcsuptake2date~srsss,data=sesuptake,family=binomial))
sink("up.sss.txt")
print(exp(cbind(OR = coef(up.sss), confint(up.sss))))
sink()
## Model 1 
up.1<-glm(crcsuptake2date~esfs+
                          srsss+
                          hhinc_z+
                          relevel(edu,ref="Advanced Degree"),
                          data=sesuptake,family=binomial)

summary(up.1)
car::vif(up.1)
sink("up.1.txt")
summary(up.1)
sink()
# Transform to ORs
up.1OR<-exp(cbind(OR = coef(up.1), confint(up.1)))
sink("up.1OR.txt")
print(exp(cbind(OR = coef(up.1), confint(up.1))))
sink()

# Model 2
up.2<-glm(crcsuptake2date~esfs+
                          srsss+
                          hhinc_z+
                          relevel(edu,ref="Advanced Degree")+
                          relevel(factor(agecat),ref="50 to 64")+
                          relevel(factor(sraceth),ref="NHWHT")+
                          relevel(factor(insurance),ref="Yes")+
                          relevel(factor(relationship),ref="Married or In Relationship")+
                          relevel(factor(crcfamily),ref= "No or Unsure")+
                          relevel(factor(pcare),ref= "Yes")+
                          relevel(factor(crcsadvice),ref= "No"),
                          data=sesuptake,family=binomial)
summary(up.2)
car::vif(up.2)
sink("up.2.txt")
summary(up.2)
sink()
# Transform to ORs
up.2OR<-exp(cbind(OR = coef(up.2), confint(up.2)))
sink("up.2OR.txt")
print(up.2OR)
sink()

#### Logistic Regression (No CRC) ----
### Ever Completed Stool-Based Test ----
summary(esncrc.esfs<-glm(crcsstool~esfs,data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial))
sink("esncrc.esfs.txt")
print(exp(cbind(OR = coef(esncrc.esfs), confint(esncrc.esfs))))
sink()
summary(esncrc.sss<-glm(crcsstool~srsss,data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial))
sink("esncrc.sss.txt")
print(exp(cbind(OR = coef(esncrc.sss), confint(esncrc.sss))))
sink()
## Model 1 
esncrc.1<-glm(crcsstool~esfs+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree"),
                    data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial)

summary(esncrc.1)
car::vif(esncrc.1)
sink("esncrc.1.txt")
summary(esncrc.1)
sink()
# Transform to ORs
esncrc.1OR<-exp(cbind(OR = coef(esncrc.1), confint(esncrc.1)))
sink("esncrc.1OR.txt")
print(esncrc.1OR)
sink()

# Model 2
esncrc.2<-glm(crcsstool~esfs+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree")+
                    relevel(factor(agecat),ref="50 to 64")+
                    relevel(factor(sraceth),ref="NHWHT")+
                    relevel(factor(insurance),ref="Yes")+
                    relevel(factor(relationship),ref="Married or In Relationship")+
                    relevel(factor(crcfamily),ref= "No or Unsure")+
                    relevel(factor(pcare),ref= "Yes")+
                    relevel(factor(crcsadvice),ref= "No"),
                    data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial)
summary(esncrc.2)
car::vif(esncrc.2)
sink("esncrc.2.txt")
summary(esncrc.2)
sink()
# Transform to ORs
esncrc.2OR<-exp(cbind(OR = coef(esncrc.2), confint(esncrc.2)))
sink("esncrc.2OR.txt")
print(esncrc.2OR)
sink()

### Ever Completed Exam-Based Test ----
summary(exncrc.esfs<-glm(crcsexam~esfs,data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial))
sink("exncrc.esfs.txt")
print(exp(cbind(OR = coef(exncrc.esfs), confint(exncrc.esfs))))
sink()
summary(exncrc.sss<-glm(crcsexam~srsss,data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial))
sink("exncrc.sss.txt")
print(exp(cbind(OR = coef(exncrc.sss), confint(exncrc.sss))))
sink()
## Model 1 
exncrc.1<-glm(crcsexam~esfs+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree"),
                   data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial)

summary(exncrc.1)
car::vif(exncrc.1)
sink("exncrc.1.txt")
summary(exncrc.1)
sink()
# Transform to ORs
exncrc.1OR<-exp(cbind(OR = coef(exncrc.1), confint(exncrc.1)))
sink("exncrc.1OR.txt")
print(exp(cbind(OR = coef(exncrc.1), confint(exncrc.1))))
sink()

# Model 2
exncrc.2<-glm(crcsexam~esfs+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree")+
                   relevel(factor(agecat),ref="50 to 64")+
                   relevel(factor(sraceth),ref="NHWHT")+
                   relevel(factor(insurance),ref="Yes")+
                   relevel(factor(relationship),ref="Married or In Relationship")+
                   relevel(factor(crcfamily),ref= "No or Unsure")+
                   relevel(factor(pcare),ref= "Yes")+
                   relevel(factor(crcsadvice),ref= "No"),
                   data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial)
summary(exncrc.2)
car::vif(exncrc.2)
sink("exncrc.2.txt")
summary(exncrc.2)
sink()
# Transform to ORs
exncrc.2OR<-exp(cbind(OR = coef(exncrc.2), confint(exncrc.2)))
sink("exncrc.2OR.txt")
print(exncrc.2OR)
sink()

### Up-to-Date with Screening ----
summary(upncrc.esfs<-glm(crcsuptake2date~esfs,data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial))
sink("upncrc.esfs.txt")
print(exp(cbind(OR = coef(upncrc.esfs), confint(upncrc.esfs))))
sink()
summary(upncrc.sss<-glm(crcsuptake2date~srsss,data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial))
sink("upncrc.sss.txt")
print(exp(cbind(OR = coef(upncrc.sss), confint(upncrc.sss))))
sink()
## Model 1 
upncrc.1<-glm(crcsuptake2date~esfs+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree"),
                          data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial)

summary(upncrc.1)
car::vif(upncrc.1)
sink("upncrc.1.txt")
summary(upncrc.1)
sink()
# Transform to ORs
upncrc.1OR<-exp(cbind(OR = coef(upncrc.1), confint(upncrc.1)))
sink("upncrc.1OR.txt")
print(exp(cbind(OR = coef(upncrc.1), confint(upncrc.1))))
sink()

# Model 2
upncrc.2<-glm(crcsuptake2date~esfs+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree")+
                          relevel(factor(agecat),ref="50 to 64")+
                          relevel(factor(sraceth),ref="NHWHT")+
                          relevel(factor(insurance),ref="Yes")+
                          relevel(factor(relationship),ref="Married or In Relationship")+
                          relevel(factor(crcfamily),ref= "No or Unsure")+
                          relevel(factor(pcare),ref= "Yes")+
                          relevel(factor(crcsadvice),ref= "No"),
                          data=sesuptake[which(sesuptake$crcdiag=="No"),],family=binomial)
summary(upncrc.2)
car::vif(upncrc.2)
sink("upncrc.2.txt")
summary(upncrc.2)
sink()
# Transform to ORs
upncrc.2OR<-exp(cbind(OR = coef(upncrc.2), confint(upncrc.2)))
sink("upncrc.2OR.txt")
print(upncrc.2OR)
sink()

#### Logistic Regression (Economic Strain Z Score - Adjusted Model 2) ----
# Stool-Based Model 2
es.2z<-glm(crcsstool~esz+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree")+
                     relevel(factor(agecat),ref="50 to 64")+
                     relevel(factor(sraceth),ref="NHWHT")+
                     relevel(factor(insurance),ref="Yes")+
                     relevel(factor(relationship),ref="Married or In Relationship")+
                     relevel(factor(crcfamily),ref= "No or Unsure")+
                     relevel(factor(pcare),ref= "Yes")+
                     relevel(factor(crcsadvice),ref= "No"),
                     data=sesuptake,family=binomial)
summary(es.2z)
sink("es.2z.txt")
summary(es.2z)
sink()
# Transform to ORs
es.2zOR<-exp(cbind(OR = coef(es.2z), confint(es.2z)))
sink("es.2zOR.txt")
print(es.2zOR)
sink()

# Exam-Based Model 2
ex.2z<-glm(crcsexam~esz+
                    srsss+
                    hhinc_z+
                    relevel(edu,ref="Advanced Degree")+
                    relevel(factor(agecat),ref="50 to 64")+
                    relevel(factor(sraceth),ref="NHWHT")+
                    relevel(factor(insurance),ref="Yes")+
                    relevel(factor(relationship),ref="Married or In Relationship")+
                    relevel(factor(crcfamily),ref= "No or Unsure")+
                    relevel(factor(pcare),ref= "Yes")+
                    relevel(factor(crcsadvice),ref= "No"),
                    data=sesuptake,family=binomial)
summary(ex.2z)
sink("ex.2z.txt")
summary(ex.2z)
sink()
# Transform to ORs
ex.2zOR<-exp(cbind(OR = coef(ex.2z), confint(ex.2z)))
sink("ex.2zOR.txt")
print(ex.2zOR)
sink()

# Up-to-Date Model 2
up.2z<-glm(crcsuptake2date~esz+
                           srsss+
                           hhinc_z+
                           relevel(edu,ref="Advanced Degree")+
                           relevel(factor(agecat),ref="50 to 64")+
                           relevel(factor(sraceth),ref="NHWHT")+
                           relevel(factor(insurance),ref="Yes")+
                           relevel(factor(relationship),ref="Married or In Relationship")+
                           relevel(factor(crcfamily),ref= "No or Unsure")+
                           relevel(factor(pcare),ref= "Yes")+
                           relevel(factor(crcsadvice),ref= "No"),
                           data=sesuptake,family=binomial)
summary(up.2z)
sink("up.2z.txt")
summary(up.2z)
sink()
# Transform to ORs
up.2zOR<-exp(cbind(OR = coef(up.2z), confint(up.2z)))
sink("up.2zOR.txt")
print(up.2zOR)
sink()

#### Logistic Regression (Interactions) ----
# Stool-Based Model 2
es.2int<-glm(crcsstool~esfs*hhinc_z+
                       srsss*hhinc_z+
                       esfs*relevel(edu,ref="Advanced Degree")+
                       srsss*relevel(edu,ref="Advanced Degree")+ 
                       relevel(factor(agecat),ref="50 to 64")+
                       relevel(factor(sraceth),ref="NHWHT")+
                       relevel(factor(insurance),ref="Yes")+
                       relevel(factor(relationship),ref="Married or In Relationship")+
                       relevel(factor(crcfamily),ref= "No or Unsure")+
                       relevel(factor(pcare),ref= "Yes")+
                       relevel(factor(crcsadvice),ref= "No"),
                       data=sesuptake,family=binomial)
summary(es.2int)
sink("es.2int.txt")
summary(es.2int)
sink()
# Transform to ORs
es.2intOR<-exp(cbind(OR = coef(es.2int), confint(es.2int)))
es.2intOR<-as.data.frame(round(es.2intOR,2))
es.2intOR<-es.2intOR%>%mutate_if(is.numeric, ~format(., nsmall = 2)) %>% 
                 unite(col=CI,`2.5 %`:`97.5 %`, sep = ",", remove=T)
write.csv(es.2intOR,"es.2intOR.csv")

# Exam-Based Model 2
ex.2int<-glm(crcsexam~esfs*hhinc_z+
                      srsss*hhinc_z+
                      esfs*relevel(edu,ref="Advanced Degree")+
                      srsss*relevel(edu,ref="Advanced Degree")+ 
                      relevel(factor(agecat),ref="50 to 64")+
                      relevel(factor(sraceth),ref="NHWHT")+
                      relevel(factor(insurance),ref="Yes")+
                      relevel(factor(relationship),ref="Married or In Relationship")+
                      relevel(factor(crcfamily),ref= "No or Unsure")+
                      relevel(factor(pcare),ref= "Yes")+
                      relevel(factor(crcsadvice),ref= "No"),
                      data=sesuptake,family=binomial)
summary(ex.2int)
sink("ex.2int.txt")
summary(ex.2int)
sink()
# Transform to ORs
ex.2intOR<-exp(cbind(OR = coef(ex.2int), confint(ex.2int)))
ex.2intOR<-as.data.frame(round(ex.2intOR,2))
ex.2intOR<-ex.2intOR%>%mutate_if(is.numeric, ~format(., nsmall = 2)) %>% 
                 unite(col=CI,`2.5 %`:`97.5 %`, sep = ",", remove=T)
write.csv(ex.2intOR,"ex.2intOR.csv")

# Up-to-Date Model 2
up.2int<-glm(crcsuptake2date~esfs*hhinc_z+
                             srsss*hhinc_z+
                             esfs*relevel(edu,ref="Advanced Degree")+
                             srsss*relevel(edu,ref="Advanced Degree")+ 
                             relevel(factor(agecat),ref="50 to 64")+
                             relevel(factor(sraceth),ref="NHWHT")+
                             relevel(factor(insurance),ref="Yes")+
                             relevel(factor(relationship),ref="Married or In Relationship")+
                             relevel(factor(crcfamily),ref= "No or Unsure")+
                             relevel(factor(pcare),ref= "Yes")+
                             relevel(factor(crcsadvice),ref= "No"),
                             data=sesuptake,family=binomial)
summary(up.2int)
sink("up.2int.txt")
summary(up.2int)
sink()
# Transform to ORs
up.2intOR<-exp(cbind(OR = coef(up.2int), confint(up.2int)))
up.2intOR<-as.data.frame(round(up.2intOR,2))
up.2intOR<-up.2intOR%>%mutate_if(is.numeric, ~format(., nsmall = 2)) %>% 
                 unite(col=CI,`2.5 %`:`97.5 %`, sep = ",", remove=T)
write.csv(up.2intOR,"up.2intOR.csv")

## Plot significant interaction with predicted probabilities ----
# Loop function to calculate predicted probabilities at different SDs of hhinc_z
for(i in seq_along(sesupplotdat$esfs)){
    sesupplotdat$predval.inc.min2sd[[i]]<-exp(es.2int$coefficients[1]+es.2int$coefficients[2]*-2+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*-2*sesupplotdat$esfs[[i]])/(1+exp(es.2int$coefficients[1]+es.2int$coefficients[2]*-2+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*-2*sesupplotdat$esfs[[i]]))
    sesupplotdat$predval.inc.min1sd[[i]]<-exp(es.2int$coefficients[1]+es.2int$coefficients[2]*-1+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*-1*sesupplotdat$esfs[[i]])/(1+exp(es.2int$coefficients[1]+es.2int$coefficients[2]*-1+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*-1*sesupplotdat$esfs[[i]]))
    sesupplotdat$predval.inc.m[[i]]<-exp(es.2int$coefficients[1]+es.2int$coefficients[2]*0+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*0*sesupplotdat$esfs[[i]])/(1+exp(es.2int$coefficients[1]+es.2int$coefficients[2]*0+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*0*sesupplotdat$esfs[[i]]))
    sesupplotdat$predval.inc.pls1sd[[i]]<-exp(es.2int$coefficients[1]+es.2int$coefficients[2]*1+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*1*sesupplotdat$esfs[[i]])/(1+exp(es.2int$coefficients[1]+es.2int$coefficients[2]*1+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*1*sesupplotdat$esfs[[i]]))
    sesupplotdat$predval.inc.pls2sd[[i]]<-exp(es.2int$coefficients[1]+es.2int$coefficients[2]*2+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*2*sesupplotdat$esfs[[i]])/(1+exp(es.2int$coefficients[1]+es.2int$coefficients[2]*2+es.2int$coefficients[3]*sesupplotdat$esfs[[i]]+es.2int$coefficients[21]*2*sesupplotdat$esfs[[i]]))
   }
# Plot lines
sesupplotdat%>%
  pivot_longer(cols = starts_with('predval.inc.'),names_to ='level.inc',values_to = 'predval')%>%
  ggplot(aes(x=esfs,y=predval,linetype=level.inc))+
  geom_line()+
  labs(x='Perceived Economic Strain',y="Predicted Probability of\nCompleting Stool-Based Test",linetype="Household Income:\nMean and Standard Deviation (SD)")+
  scale_x_continuous(breaks=c(-2.0,-1.0,0.0,1.0,2.0))+
  scale_linetype_manual(values=c('longdash','dotdash','solid','dashed','dotted'),
                        breaks=c('predval.inc.min2sd','predval.inc.min1sd','predval.inc.m',
                        'predval.inc.pls1sd','predval.inc.pls2sd'),
                        labels=c('-2 SD','-1 SD','Mean','+1 SD','+2 SD'))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title = element_text(family='serif',colour='black',size=14),
        legend.text = element_text(family='serif',colour='black',size = 12),
        legend.title = element_text(family='serif',colour='black',size = 12),
        legend.position = c(1,1),legend.justification = c(1,1),
        legend.background = element_blank(),legend.key = element_blank(),
        axis.text = element_text(family='serif',colour='black',size = 12))
