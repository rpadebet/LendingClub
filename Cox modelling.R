#' ----------------------------------------------------------
#' Using Sample Loans to frame dataset to feed into Cox Model
#' ----------------------------------------------------------

# Load Libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)

# Load the sample Data
loan_sample_all_2012H1<-fread("Sample Loans 36 2012H1.csv", header=T)

# Select the necessary columns
loan_sample_all_model_2012H1<-as.tbl(loan_sample_all_2012H1)%>%
    filter(as.Date(MONTH_BEGIN_PAY_DT,origin=Sys.Date()) < 
               as.Date("2016-10-01",origin=Sys.Date()))%>%
    select(id,int_rate,MONTH.AGE.BEGIN,DEFAULT,UNEMP.RT_ISS,UNEMP.RT.BEGIN_DT)%>%
    mutate(int_rate = int_rate-mean(int_rate))%>%
    mutate(UNEMP.RT.BEGIN_DT = UNEMP.RT.BEGIN_DT-mean(UNEMP.RT.BEGIN_DT))%>%
    mutate(UNEMP.RT_ISS = UNEMP.RT_ISS-mean(UNEMP.RT_ISS))%>%
    mutate(MONTH.AGE.BEGIN = as.factor(MONTH.AGE.BEGIN))

# Fit linear models

loan_linear_mdl_2012H1_Cur<-lm(DEFAULT ~ int_rate + UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                               data = loan_sample_all_model_2012H1)
loan_linear_mdl_2012H1_Iss<-lm(DEFAULT ~ int_rate + UNEMP.RT_ISS+MONTH.AGE.BEGIN -1,
                               data = loan_sample_all_model_2012H1)
loan_linear_mdl_2012H1_Both<-lm(DEFAULT ~ int_rate + UNEMP.RT_ISS+ UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                                data = loan_sample_all_model_2012H1)


model_stats7<-model_summary(model = loan_linear_mdl_2012H1_Cur,
                            experiment = 7,geography = "ALL",period = "2012H1")
model_stats8<-model_summary(model = loan_linear_mdl_2012H1_Iss,
                            experiment = 8,geography = "ALL",period = "2012H1")
model_stats9<-model_summary(model = loan_linear_mdl_2012H1_Both,
                            experiment = 9,geography = "ALL",period = "2012H1")

all_model_stats<-as.data.table(all_model_stats)
all_model_stats<-rbind(model_stats9,model_stats8,model_stats7,all_model_stats)

####### Another Date Range Q42014 -2015
# Load the sample Data
loan_sample_all_2014Q4_2015<-fread("Sample Loans 36 2014Q4-2015.csv", header=T)

# Select the necessary columns
loan_sample_all_model_2014Q4_2015<-as.tbl(loan_sample_all_2014Q4_2015)%>%
    filter(as.Date(MONTH_BEGIN_PAY_DT,origin=Sys.Date()) < 
               as.Date("2016-10-01",origin=Sys.Date()))%>%
    select(id,int_rate,MONTH.AGE.BEGIN,DEFAULT,UNEMP.RT_ISS,UNEMP.RT.BEGIN_DT)%>%
    mutate(int_rate = int_rate-mean(int_rate))%>%
    mutate(UNEMP.RT.BEGIN_DT = UNEMP.RT.BEGIN_DT-mean(UNEMP.RT.BEGIN_DT))%>%
    mutate(UNEMP.RT_ISS = UNEMP.RT_ISS-mean(UNEMP.RT_ISS))%>%
    mutate(MONTH.AGE.BEGIN = as.factor(MONTH.AGE.BEGIN))

# Fit linear models

loan_linear_mdl_2014Q4_2015_Cur<-lm(DEFAULT ~ int_rate + UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                                    data = loan_sample_all_model_2014Q4_2015)
loan_linear_mdl_2014Q4_2015_Iss<-lm(DEFAULT ~ int_rate + UNEMP.RT_ISS+MONTH.AGE.BEGIN -1,
                                    data = loan_sample_all_model_2014Q4_2015)
loan_linear_mdl_2014Q4_2015_Both<-lm(DEFAULT ~ int_rate + UNEMP.RT_ISS+ UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                                     data = loan_sample_all_model_2014Q4_2015)


model_stats10<-model_summary(model = loan_linear_mdl_2014Q4_2015_Cur,
                            experiment = 10,geography = "ALL",period = "2014Q4_2015")
model_stats11<-model_summary(model = loan_linear_mdl_2014Q4_2015_Iss,
                            experiment = 11,geography = "ALL",period = "2014Q4_2015")
model_stats12<-model_summary(model = loan_linear_mdl_2014Q4_2015_Both,
                            experiment = 12,geography = "ALL",period = "2014Q4_2015")

all_model_stats<-rbind(model_stats10,model_stats11,model_stats12,all_model_stats)



write.csv(all_model_stats,"All Model Stats.csv",row.names = F)

all_model_stats_simple<-all_model_stats[Attribute %in% c("int_rate",
                                                         "UNEMP.RT.BEGIN_DT",
                                                         "UNEMP.RT_ISS",
                                                         "fico_range_low"),]

write.csv(all_model_stats_simple,"All Model Stats Simple.csv",row.names = F)



#### Experiments with time dummy
loan_sample_all_2011_2015<-fread("Sample Loans 36 2011_2015.csv", header=T)

# Select the necessary columns
loan_sample_all_2011_2015_d<-as.tbl(loan_sample_all_2011_2015)%>%
    filter(as.Date(MONTH_BEGIN_PAY_DT,origin=Sys.Date()) < 
               as.Date("2016-10-01",origin=Sys.Date()))%>%
    mutate(TIME_DUMMY = as.factor(year(as.Date(issue_d,origin=Sys.Date())+months(MONTH.AGE.BEGIN))))%>%
    select(id,int_rate,MONTH.AGE.BEGIN,DEFAULT,UNEMP.RT_ISS,UNEMP.RT.BEGIN_DT,TIME_DUMMY)%>%
    mutate(int_rate = int_rate-mean(int_rate))%>%
    mutate(UNEMP.RT.BEGIN_DT = UNEMP.RT.BEGIN_DT-mean(UNEMP.RT.BEGIN_DT))%>%
    mutate(UNEMP.RT_ISS = UNEMP.RT_ISS-mean(UNEMP.RT_ISS))%>%
    mutate(MONTH.AGE.BEGIN = as.factor(MONTH.AGE.BEGIN))


# Fit linear models

loan_mdl_2011_2015d_Cur<-lm(DEFAULT ~ int_rate + UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN+TIME_DUMMY -1,
                            data = loan_sample_all_2011_2015_d)
loan_mdl_2011_2015d_Iss<-lm(DEFAULT ~ int_rate + UNEMP.RT_ISS+MONTH.AGE.BEGIN+TIME_DUMMY -1,
                            data = loan_sample_all_2011_2015_d)
loan_mdl_2011_2015d_Both<-lm(DEFAULT ~ int_rate + UNEMP.RT_ISS+ UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN+TIME_DUMMY -1,
                             data = loan_sample_all_2011_2015_d)


model_stats13<-model_summary(model = loan_mdl_2011_2015d_Cur,
                             experiment = 13,geography = "ALL",period = "2011_2015_TIMEDUMMY")
model_stats14<-model_summary(model = loan_mdl_2011_2015d_Iss,
                             experiment = 14,geography = "ALL",period = "2011_2015_TIMEDUMMY")
model_stats15<-model_summary(model = loan_mdl_2011_2015d_Both,
                             experiment = 15,geography = "ALL",period = "2011_2015_TIMEDUMMY")


all_model_stats<-rbind(model_stats13,model_stats14,model_stats15,all_model_stats)

all_model_stats_simple<-all_model_stats[Attribute %in% c("int_rate",
                                                         "UNEMP.RT.BEGIN_DT",
                                                         "UNEMP.RT_ISS",
                                                         "fico_range_low"),]



all_model_stats<-all_model_stats[Experiment<=12,]


# Select the necessary columns
loan_sample_all_2011_2015_di<-as.tbl(loan_sample_all_2011_2015)%>%
    filter(as.Date(MONTH_BEGIN_PAY_DT,origin=Sys.Date()) < 
               as.Date("2016-10-01",origin=Sys.Date()))%>%
    mutate(TIME_DUMMY = as.factor(year(as.Date(issue_d,origin=Sys.Date()))))%>%
    select(id,int_rate,MONTH.AGE.BEGIN,DEFAULT,UNEMP.RT_ISS,UNEMP.RT.BEGIN_DT,TIME_DUMMY)%>%
    mutate(int_rate = int_rate-mean(int_rate))%>%
    mutate(UNEMP.RT.BEGIN_DT = UNEMP.RT.BEGIN_DT-mean(UNEMP.RT.BEGIN_DT))%>%
    mutate(UNEMP.RT_ISS = UNEMP.RT_ISS-mean(UNEMP.RT_ISS))%>%
    mutate(MONTH.AGE.BEGIN = as.factor(MONTH.AGE.BEGIN))

loan_mdl_2011_2015di_Cur<-lm(DEFAULT ~ int_rate + UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN+TIME_DUMMY -1,
                             data = loan_sample_all_2011_2015_di)
model_stats16<-model_summary(model = loan_mdl_2011_2015di_Cur,
                             experiment = 16,geography = "ALL",period = "2011_2015_TIMEDUMMY_ISS")


