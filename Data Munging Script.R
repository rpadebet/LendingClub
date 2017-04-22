
## Load the libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(stringi)
library(stringr)

## Read the Data
url<-"https://www.lendingclub.com/info/download-data.action"
loan_2007_2011<-fread(input = "Original Data/LoanStats3a_securev1.csv",header = T,skip = 0)
loan_2012_2013<-fread(input = "Original Data/LoanStats3b_securev1.csv",header = T,skip = 1)
loan_2014<-fread(input = "Original Data/LoanStats3c_securev1.csv",header = T,skip = 1)
loan_2015<-fread(input = "Original Data/LoanStats3d_securev1.csv",header = T,skip = 1)

loan_ALL <- rbind(loan_2007_2011,loan_2012_2013,loan_2014,loan_2015)

# rm(loan_2007.2011,loan_2012.2013,loan_2014,loan_2015)

head(loan_ALL,5)

### Selecting Column Subset

initial_cols<-c("id","application_type","loan_amnt","funded_amnt","term","int_rate","grade","sub_grade","emp_length","home_ownership","annual_inc","verification_status","annual_inc_joint","issue_d","loan_status","purpose","zip_code","addr_state","dti","dti_joint","earliest_cr_line","fico_range_low","fico_range_high","inq_last_6mths","open_acc","total_acc","pub_rec","revol_util","last_pymnt_d","next_pymnt_d","last_credit_pull_d","last_fico_range_high","last_fico_range_low","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","out_prncp","out_prncp_inv")

loan_sub<-loan_ALL[,initial_cols,with=FALSE]

numeric_cols<-c("loan_amnt","funded_amnt","int_rate","annual_inc","annual_inc_joint","dti","dti_joint","fico_range_low","fico_range_high","inq_last_6mths","last_fico_range_high","last_fico_range_low","open_acc","total_acc","revol_util","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","out_prncp","out_prncp_inv")
factor_cols<-c("id","application_type","term","grade","sub_grade","emp_length","home_ownership","verification_status","loan_status","purpose","zip_code","addr_state","pub_rec")
date_cols<-c("issue_d","earliest_cr_line","last_pymnt_d")


## Cleaning the interest rate column to remove % sign
loan_sub$int_rate<-gsub("%","",x = loan_sub$int_rate)

## Cleaning up term column to remove trailing spaces
loan_sub$term<-str_trim(loan_sub$term)

## Specifying class of columns
loan_sub[,(numeric_cols):= lapply(.SD, as.numeric), .SDcols = numeric_cols]
loan_sub[,(factor_cols):= lapply(.SD, as.factor), .SDcols = factor_cols]

### Clean up dates
loan_sub$issue_d<-as_date(ifelse(stri_length(loan_sub$issue_d)==6,
                                    as.Date(gsub("^","01-",x=loan_sub$issue_d),format="%d-%b-%y"),
                                    as.Date(gsub("^","01-",x=loan_sub$issue_d),format="%d-%b-%Y") ))

loan_sub$earliest_cr_line<-as_date(ifelse(stri_length(loan_sub$earliest_cr_line)==6,
                                             as.Date(gsub("^","01-",x=loan_sub$earliest_cr_line),
                                                     format="%d-%b-%y",origin = "1900-01-01"),
                                             as.Date(gsub("^","01-",x=loan_sub$earliest_cr_line),
                                                     format="%d-%b-%Y",origin = "1900-01-01") ))

    ### cleaning up older credit histories 
    ### which are shown as future credit histories. about 412 records

loan_sub[earliest_cr_line>"2015-01-01",earliest_cr_line:=earliest_cr_line-years(100)]

loan_sub$last_pymnt_d<-as_date(ifelse(stri_length(loan_sub$last_pymnt_d)==6,
                                         as.Date(gsub("^","01-",x=loan_sub$last_pymnt_d),
                                                 format="%d-%b-%y",origin = "1900-01-01"),
                                         as.Date(gsub("^","01-",x=loan_sub$last_pymnt_d),
                                                 format="%d-%b-%Y",origin = "1900-01-01") ))

summary(loan_sub)


### Creating new attributes and prediction columns

loan_mod<-loan_sub

## Performing Loans
loan_mod$PERFORM<-ifelse(loan_mod$loan_status %in% c("Current","Fully Paid"),1,0)
## Return for each Loan
loan_mod[,RETURN := ((total_pymnt/funded_amnt)-1)/ifelse(loan_mod$term %in% c("36 months"),3,6) ]
## Age in days of each Loan
loan_mod$AGE <- loan_mod$last_pymnt_d-loan_mod$issue_d
## Maturity Date for each Loan
loan_mod[,MATURITY:=issue_d+months(ifelse(loan_mod$term %in% c("36 months"),36,60))][,PREPAID:=0]
## Current Loans
loan_mod[,CURRENT:=ifelse(loan_status=="Current",1,0)]
## Prepaid status for each Loan (select only fully paid loans)
loan_mod[(PERFORM==1)&(CURRENT==0),PREPAID:=ifelse(last_pymnt_d<MATURITY,1,0)]
## Vintage of Loan
loan_mod[,VINTAGE:=year(issue_d)]

## Clean up age column and last payment date column
loan_mod[is.na(AGE),last_pymnt_d:=issue_d][is.na(AGE),AGE:=0]

### Splitting 36 month and 60 month loans
loan_36<-loan_mod[term=="36 months",.(id,loan_amnt,total_pymnt,term,
                             int_rate,grade,emp_length,annual_inc,dti,
                             issue_d,last_pymnt_d,purpose,addr_state,
                             earliest_cr_line,fico_range_low,
                             RETURN,AGE,MATURITY,VINTAGE,
                             PERFORM,CURRENT,PREPAID)]

loan_60<-loan_mod[term=="60 months",.(id,loan_amnt,total_pymnt,term,
                             int_rate,grade,emp_length,annual_inc,dti,
                             issue_d,last_pymnt_d,purpose,addr_state,
                             earliest_cr_line,fico_range_low,
                             RETURN,AGE,MATURITY,VINTAGE,
                             PERFORM,CURRENT,PREPAID)]


### Creating Wide table form with intermediate payment dates

for(i in seq(1:36)){
    loan_36[,paste0("M",i):= issue_d+months(i)]
}

for(i in seq(1:60)){
    loan_60[,paste0("M",i):= issue_d+months(i)]
}


### Converting it back into tall form
loan_36_hist<-as.tbl(loan_36)%>%
    gather(key = MONTH.AGE, value=NEXT_PAY_DT,M1:M36)%>%
    as.data.table()

loan_60_hist<-as.tbl(loan_60)%>%
    gather(key = MONTH.AGE, value=NEXT_PAY_DT,M1:M60)%>%
    as.data.table()

### Creating Survival Column
loan_36_hist[,SURVIVED:=ifelse(NEXT_PAY_DT<=last_pymnt_d,1,0)]
loan_60_hist[,SURVIVED:=ifelse(NEXT_PAY_DT<=last_pymnt_d,1,0)]

### Recombining Data Sets
loan_hist<-rbind(loan_36_hist,loan_60_hist)

### Getting Unemployment Data
unemp.data<-read.csv("State Unemployment Data Monthly History.csv",header = T)
unemp.data$state<-as.factor(str_trim(unemp.data$state))
unemp.data$date<-as_date(unemp.data$date)
unemp.data<-as.data.table(unemp.data)

### Joining with Unemployment Data
loan_hist[unemp.data,UNEMP.RT_ISS:=i.value,on=c(addr_state="state",issue_d="date")]
loan_hist[unemp.data,UNEMP.RT.PAY:=i.value,on=c(addr_state="state",last_pymnt_d="date")]


### Saving as a file
write.csv(loan_hist,"Loan History.csv")
