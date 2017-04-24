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
loan_sample<-read.csv("Sample Loans CA 36 2012_2015.csv", header=T)

# Select the necessary columns
loan_sample_model<-as.tbl(loan_sample)%>%
    filter(as.Date(MONTH_BEGIN_PAY_DT,origin=Sys.Date()) < as.Date("2016-10-01",origin=Sys.Date()))%>%
    select(id,int_rate,MONTH.AGE.BEGIN,DEFAULT,UNEMP.RT.BEGIN_DT)%>%
    mutate(int_rate = int_rate-mean(int_rate))%>%
    mutate(UNEMP.RT.BEGIN_DT = UNEMP.RT.BEGIN_DT-mean(UNEMP.RT.BEGIN_DT))%>%
    mutate(MONTH.AGE.BEGIN = as.factor(MONTH.AGE.BEGIN))

# Fit a linear model

loan_linear_mdl<-lm(DEFAULT ~ int_rate + UNEMP.RT.BEGIN_DT+MONTH.AGE.BEGIN,data = loan_sample_model)

summary(loan_linear_mdl)
