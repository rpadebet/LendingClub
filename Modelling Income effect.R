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
    select(id,int_rate,MONTH.AGE.BEGIN,DEFAULT,UNEMP.RT_ISS,UNEMP.RT.BEGIN_DT,AVG.INC.ZIP)%>%
    mutate(int_rate = int_rate-mean(int_rate))%>%
    mutate(UNEMP.RT.BEGIN_DT = UNEMP.RT.BEGIN_DT-mean(UNEMP.RT.BEGIN_DT,na.rm = T))%>%
    mutate(UNEMP.RT_ISS = UNEMP.RT_ISS-mean(UNEMP.RT_ISS,na.rm = T))%>%
    mutate(MONTH.AGE.BEGIN = as.factor(MONTH.AGE.BEGIN))%>%
    mutate(AVG.INC.ZIP = AVG.INC.ZIP-median(AVG.INC.ZIP,na.rm = T))

data<-loan_sample_all_model_2012H1[complete.cases(loan_sample_all_model_2012H1),]

# Fit linear models

loan_linear_mdl_2012H1_Cur<-lm(DEFAULT ~ int_rate + AVG.INC.ZIP + UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                               data = data)
loan_linear_mdl_2012H1_Iss<-lm(DEFAULT ~ int_rate + AVG.INC.ZIP + UNEMP.RT_ISS+MONTH.AGE.BEGIN -1,
                               data = data)
loan_linear_mdl_2012H1_Both<-lm(DEFAULT ~ int_rate + AVG.INC.ZIP + UNEMP.RT_ISS+ UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                                data = data)


model_stats7<-model_summary(model = loan_linear_mdl_2012H1_Cur,
                            experiment = 7,geography = "ALL",period = "2012H1")
model_stats8<-model_summary(model = loan_linear_mdl_2012H1_Iss,
                            experiment = 8,geography = "ALL",period = "2012H1")
model_stats9<-model_summary(model = loan_linear_mdl_2012H1_Both,
                            experiment = 9,geography = "ALL",period = "2012H1")

all_model_stats<-fread("All Model Stats.csv",header = T)
all_model_stats[,]<-NULL

all_model_stats<-rbind(model_stats9,model_stats8,model_stats7,all_model_stats)


################################################################################
####             Another Date Range Q42014 -2015                         #######
################################################################################



# Load the sample Data
loan_sample_all_2014Q4_2015<-fread("Sample Loans 36 2014Q4-2015.csv", header=T)

# Select the necessary columns
loan_sample_all_model_2014Q4_2015<-as.tbl(loan_sample_all_2014Q4_2015)%>%
    filter(as.Date(MONTH_BEGIN_PAY_DT,origin=Sys.Date()) < 
               as.Date("2016-10-01",origin=Sys.Date()))%>%
    select(id,int_rate,MONTH.AGE.BEGIN,DEFAULT,UNEMP.RT_ISS,UNEMP.RT.BEGIN_DT,AVG.INC.ZIP)%>%
    mutate(int_rate = int_rate-mean(int_rate))%>%
    mutate(UNEMP.RT.BEGIN_DT = UNEMP.RT.BEGIN_DT-mean(UNEMP.RT.BEGIN_DT,na.rm = T))%>%
    mutate(UNEMP.RT_ISS = UNEMP.RT_ISS-mean(UNEMP.RT_ISS,na.rm = T))%>%
    mutate(MONTH.AGE.BEGIN = as.factor(MONTH.AGE.BEGIN))%>%
    mutate(AVG.INC.ZIP = AVG.INC.ZIP-median(AVG.INC.ZIP,na.rm = T))

# Fit linear models

loan_linear_mdl_2014Q4_2015_Cur<-lm(DEFAULT ~ int_rate + AVG.INC.ZIP+UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                                    data = loan_sample_all_model_2014Q4_2015)
loan_linear_mdl_2014Q4_2015_Iss<-lm(DEFAULT ~ int_rate+ AVG.INC.ZIP + UNEMP.RT_ISS+MONTH.AGE.BEGIN -1,
                                    data = loan_sample_all_model_2014Q4_2015)
loan_linear_mdl_2014Q4_2015_Both<-lm(DEFAULT ~ int_rate + AVG.INC.ZIP+ UNEMP.RT_ISS+ UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                                     data = loan_sample_all_model_2014Q4_2015)


model_stats10<-model_summary(model = loan_linear_mdl_2014Q4_2015_Cur,
                             experiment = 10,geography = "ALL",period = "2014Q4_2015")
model_stats11<-model_summary(model = loan_linear_mdl_2014Q4_2015_Iss,
                             experiment = 11,geography = "ALL",period = "2014Q4_2015")
model_stats12<-model_summary(model = loan_linear_mdl_2014Q4_2015_Both,
                             experiment = 12,geography = "ALL",period = "2014Q4_2015")

all_model_stats<-rbind(model_stats10,model_stats11,model_stats12,all_model_stats)



################################################################################
####             Another Date Range 2011 -2015                         #######
################################################################################



# Load the sample Data
loan_sample_all_2011_2015<-fread("Sample Loans 36 2011_2015.csv", header=T)

# Select the necessary columns
loan_sample_all_model_2011_2015<-as.tbl(loan_sample_all_2011_2015)%>%
    filter(as.Date(MONTH_BEGIN_PAY_DT,origin=Sys.Date()) < 
               as.Date("2016-10-01",origin=Sys.Date()))%>%
    select(id,int_rate,MONTH.AGE.BEGIN,DEFAULT,UNEMP.RT_ISS,UNEMP.RT.BEGIN_DT,AVG.INC.ZIP)%>%
    mutate(int_rate = int_rate-mean(int_rate))%>%
    mutate(UNEMP.RT.BEGIN_DT = UNEMP.RT.BEGIN_DT-mean(UNEMP.RT.BEGIN_DT,na.rm = T))%>%
    mutate(UNEMP.RT_ISS = UNEMP.RT_ISS-mean(UNEMP.RT_ISS,na.rm = T))%>%
    mutate(MONTH.AGE.BEGIN = as.factor(MONTH.AGE.BEGIN))%>%
    mutate(AVG.INC.ZIP = AVG.INC.ZIP-median(AVG.INC.ZIP,na.rm = T))

# Fit linear models

loan_linear_mdl_2011_2015_Cur<-lm(DEFAULT ~ int_rate+ AVG.INC.ZIP + UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                                    data = loan_sample_all_model_2011_2015)
loan_linear_mdl_2011_2015_Iss<-lm(DEFAULT ~ int_rate+ AVG.INC.ZIP + UNEMP.RT_ISS+MONTH.AGE.BEGIN -1,
                                    data = loan_sample_all_model_2011_2015)
loan_linear_mdl_2011_2015_Both<-lm(DEFAULT ~ int_rate+ AVG.INC.ZIP + UNEMP.RT_ISS+ UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN -1,
                                     data = loan_sample_all_model_2011_2015)


model_stats4<-model_summary(model = loan_linear_mdl_2011_2015_Cur,
                             experiment = 4,geography = "ALL",period = "2011_2015")
model_stats5<-model_summary(model = loan_linear_mdl_2011_2015_Iss,
                             experiment = 5,geography = "ALL",period = "2011_2015")
model_stats6<-model_summary(model = loan_linear_mdl_2011_2015_Both,
                             experiment = 6,geography = "ALL",period = "2011_2015")

all_model_stats<-rbind(model_stats4,model_stats5,model_stats6,all_model_stats)

