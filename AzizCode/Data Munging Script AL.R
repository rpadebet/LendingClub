
## Load the libraries
library(data.table)
library(tidyverse)
library(lubridate)
library(stringi)
library(stringr)

setwd("c:/Aziz/Rfiles/data/")

dir()

# read in the data


# helper functions 
testifpresent = function(z,v) {
  # tests if each character element of z is in any of the elements in character vector v
  # z and v are assumed to be unique
  
  return(any(grepl(z,v)))
}


my.format.date = function(x) {
  # adds in the first of the month to a MonthName-YYYY format and then converts to standard date format
  temp = paste0("01-", x)
  tempout = parse_date_time(temp,"d b Y")
  return(tempout)
}

clean_and_extract = function(tempfile) {
  # cleans up and extracts the necessary data from tempfile
  # returns a data.frame to be appended on to the rest of the rows
  tempexpand = unzip(tempfile)
  
  # check if we need to skip the first couple of rows
  skipvalue = 0
  
  
  for (q in 1:10) {
    checkskip = try(read.csv(tempexpand, skip = skipvalue, nrows = 1000))
    if (is(checkskip, "try-error"))
      skipvalue = skipvalue + 1
    else
      q = 11
  }
  
  qqqorig = fread(
    tempexpand, blank.lines.skip = TRUE,
    fill = TRUE,
    skip = skipvalue,
    select  = keep_cols
  )
  qqq = qqqorig
  qqq = as.data.frame(qqq)
  
  ##### handle special cases
  
  ## interest rate --  remove % sign
  qqq$int_rate <- gsub("%","",x = qqq$int_rate)
  
  ## term -- remove the word "months"
  qqq$term <- gsub("months","",x = qqq$term)
  
  # format numeric
  # figure out what subset of names are present
  ispresent = sapply(numeric_cols, testifpresent, names(qqq))
  numeric_temp = numeric_cols[ispresent]
  
  # note: apply() coerces the result to numeric. lapply() allows for any type
  qqq[,numeric_temp] = as.data.frame(lapply(qqq[,numeric_temp],as.numeric))
  
  
  # format dates
  qqq[,date_cols] = as.data.frame(lapply(qqq[,date_cols], my.format.date))

  # format factors
  temp = as.data.frame(lapply(qqq[,factor_cols], as.factor))
  qqq[,factor_cols] = temp
  
  # drop obvious junk
  isok = with(qqq,!is.na(loan_amnt) &
                !is.na(int_rate) & !is.na(issue_d))
  sum(!isok)
  qqq = qqq[isok,]
  return(qqq)
}



#empty data.frame to store everything
alldata1 = data.frame()
alldata2 = data.frame()

# identify data columns of interest
date_cols<-c("issue_d", "earliest_cr_line","last_pymnt_d")

factor_cols<-c("id","member_id","application_type","grade","sub_grade",
               "emp_length","home_ownership","verification_status","loan_status",
               "purpose","zip_code","addr_state","emp_title",
               "pub_rec")

numeric_cols<-c("loan_amnt","funded_amnt","int_rate","term","annual_inc","installment",
                "annual_inc_joint","dti","dti_joint","fico_range_low",
                "fico_range_high","inq_last_6mths","last_fico_range_high","last_fico_range_low",
                "open_acc","total_acc","revol_util",
                "total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int",
                "total_rec_late_fee","recoveries","out_prncp","out_prncp_inv")

char_cols = c("")

keep_cols = c(date_cols,numeric_cols,factor_cols,char_cols)

# specify the filenames -- only the unique parts
#filenames = c("3a", "3b", "3c", "3d", "_2016Q1", "_2016Q2", "_2016Q3", "_2016Q4")

filenames = c("3a", "3b", "3c", "3d")

nfiles = length(filenames)


for (j in 1:nfiles) {
  tempfile = paste0("LoanStats", filenames[j], ".csv.zip")
  print(tempfile)
  ##### cleanup temp.data.frame before adding it to master file to keep the file size manageable
  qqqout = clean_and_extract(tempfile)
  print(nrow(qqqout))
  alldata1 = rbind.data.frame(alldata1,qqqout)
}


save(alldata1, file = "alldata1.RData")                          

# ## process files one at a time, and then stack them together in one humungous file
# 
# ## Read the Data
# #url<-"https://www.lendingclub.com/info/download-data.action"
# 
# temp = unzip("LoanStats3a.csv.zip")
# temp2 = read.csv(temp, skip =1, nrow = 100)
# loan_2007_2011<-fread(input = temp,header = T,skip = 0)
# 
# loan_2007_2011<-fread(input = "Original Data/LoanStats3a_securev1.csv",header = T,skip = 0)
# loan_2012_2013<-fread(input = "Original Data/LoanStats3b_securev1.csv",header = T,skip = 1)
# loan_2014<-fread(input = "Original Data/LoanStats3c_securev1.csv",header = T,skip = 1)
# loan_2015<-fread(input = "Original Data/LoanStats3d_securev1.csv",header = T,skip = 1)
# 
# loan_ALL <- rbind(loan_2007_2011,loan_2012_2013,loan_2014,loan_2015)
# 
# # rm(loan_2007.2011,loan_2012.2013,loan_2014,loan_2015)
# 
# head(loan_ALL,5)
# 
# ### Selecting Column Subset
# 
# 
# # columns to keep
# 
# 
# 
# # initial_cols<-c("id","application_type","loan_amnt","funded_amnt","term","int_rate","grade","sub_grade","emp_length","home_ownership","annual_inc","verification_status","annual_inc_joint","issue_d","loan_status","purpose","zip_code","addr_state","dti","dti_joint","earliest_cr_line","fico_range_low","fico_range_high","inq_last_6mths","open_acc","total_acc","pub_rec","revol_util","last_pymnt_d","next_pymnt_d","last_credit_pull_d","last_fico_range_high","last_fico_range_low","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee","recoveries","out_prncp","out_prncp_inv")
# 
# loan_sub<-loan_ALL[,initial_cols,with=FALSE]
# 
# 
# 
# 
# ## Specifying class of columns
# loan_sub[,(numeric_cols):= lapply(.SD, as.numeric), .SDcols = numeric_cols]
# loan_sub[,(factor_cols):= lapply(.SD, as.factor), .SDcols = factor_cols]
# 
# ### Clean up dates
# loan_sub$issue_d<-as_date(ifelse(stri_length(loan_sub$issue_d)==6,
#                                     as.Date(gsub("^","01-",x=loan_sub$issue_d),format="%d-%b-%y"),
#                                     as.Date(gsub("^","01-",x=loan_sub$issue_d),format="%d-%b-%Y") ))
# 
# loan_sub$earliest_cr_line<-as_date(ifelse(stri_length(loan_sub$earliest_cr_line)==6,
#                                              as.Date(gsub("^","01-",x=loan_sub$earliest_cr_line),
#                                                      format="%d-%b-%y",origin = "1900-01-01"),
#                                              as.Date(gsub("^","01-",x=loan_sub$earliest_cr_line),
#                                                      format="%d-%b-%Y",origin = "1900-01-01") ))
# 
#     ### cleaning up older credit histories 
#     ### which are shown as future credit histories. about 412 records
# 
# loan_sub[earliest_cr_line>"2015-01-01",earliest_cr_line:=earliest_cr_line-years(100)]
# 
# loan_sub$last_pymnt_d<-as_date(ifelse(stri_length(loan_sub$last_pymnt_d)==6,
#                                          as.Date(gsub("^","01-",x=loan_sub$last_pymnt_d),
#                                                  format="%d-%b-%y",origin = "1900-01-01"),
#                                          as.Date(gsub("^","01-",x=loan_sub$last_pymnt_d),
#                                                  format="%d-%b-%Y",origin = "1900-01-01") ))
# 
# summary(loan_sub)
# 
# 
# ### Creating new attributes and prediction columns
# 
# loan_mod<-loan_sub
# 
# ## Defaulted Delinquent Current Performing Loans
# loan_mod$DEFAULT<-ifelse(loan_mod$loan_status %in% c("Charged Off"),1,0)
# loan_mod$DELINQ<-ifelse(!(loan_mod$loan_status %in% c("Charged Off","Fully Paid","Current")),1,0)
# loan_mod$CURRENT<-ifelse(loan_mod$loan_status %in% c("Current"),1,0)
# loan_mod$FULLPAID <-ifelse(loan_mod$loan_status %in% c("Fully Paid"),1,0)
# 
# ## Return for each Loan
# # loan_mod[,RETURN := ((total_pymnt/funded_amnt)-1)/ifelse(loan_mod$term %in% c("36 months"),3,6) ]
# ## Age in days of each Loan
# loan_mod$AGE <- loan_mod$last_pymnt_d-loan_mod$issue_d
# 
# ## Maturity Date for each Loan and initializing all loans to not prepaid
# loan_mod[,MATURITY:=issue_d+months(ifelse(loan_mod$term %in% c("36 months"),36,60))][,PREPAID:=0]
# ## Prepaid status for each Loan (select only fully paid loans)
# loan_mod[(FULLPAID==1),PREPAID:=ifelse(last_pymnt_d<MATURITY,1,0)]
# ## Vintage of Loan
# loan_mod[,VINTAGE:=year(issue_d)]
# 
# ## Clean up age column and last payment date column
# loan_mod[is.na(AGE),last_pymnt_d:=issue_d][is.na(AGE),AGE:=0]
# 
# ### Splitting 36 month and 60 month loans
# loan_36<-loan_mod[term=="36 months",.(id,loan_amnt,total_pymnt,term,
#                              int_rate,grade,emp_length,annual_inc,dti,
#                              issue_d,last_pymnt_d,purpose,addr_state,
#                              earliest_cr_line,fico_range_low,
#                              AGE,MATURITY,VINTAGE,
#                              DEFAULT,DELINQ,CURRENT,FULLPAID,PREPAID)]
# 
# loan_60<-loan_mod[term=="60 months",.(id,loan_amnt,total_pymnt,term,
#                                       int_rate,grade,emp_length,annual_inc,dti,
#                                       issue_d,last_pymnt_d,purpose,addr_state,
#                                       earliest_cr_line,fico_range_low,
#                                       AGE,MATURITY,VINTAGE,
#                                       DEFAULT,DELINQ,CURRENT,FULLPAID,PREPAID)]
# 
# 
# ### Creating Wide table form with intermediate payment dates
# 
# for(i in seq(1:36)){
#     loan_36[,paste0("M",i):= issue_d+months(i)]
# }
# 
# for(i in seq(1:60)){
#     loan_60[,paste0("M",i):= issue_d+months(i)]
# }
# 
# 
# ### Converting it back into tall form with each record now a loan of 1 month maturity
# loan_36_hist<-as.tbl(loan_36)%>%
#     gather(key = MONTH.AGE, value=MONTH_END_PAY_DT,M1:M36)%>%
#     mutate(MONTH.AGE.END = gsub("M","",MONTH.AGE))%>%
#     mutate(MONTH.AGE.BEGIN = as.numeric(MONTH.AGE.END) - 1)%>%
#     mutate(MONTH_BEGIN_PAY_DT = MONTH_END_PAY_DT - months(1))%>%
#     as.data.table()
# 
# loan_60_hist<-as.tbl(loan_60)%>%
#     gather(key = MONTH.AGE, value=MONTH_END_PAY_DT,M1:M60)%>%
#     mutate(MONTH.AGE.END = gsub("M","",MONTH.AGE))%>%
#     mutate(MONTH.AGE.BEGIN = as.numeric(MONTH.AGE.END) - 1)%>%
#     mutate(MONTH_BEGIN_PAY_DT = MONTH_END_PAY_DT - months(1))%>%
#     as.data.table()
# 
# ### Creating Survival Flag Column
# 
# # For defaulted loans (survived until next month of last payment)
# loan_36_hist[(DEFAULT==1)|(DELINQ==1),SURVIVED:=ifelse(MONTH_BEGIN_PAY_DT<=last_pymnt_d+months(1),1,0)]
# loan_60_hist[(DEFAULT==1)|(DELINQ==1),SURVIVED:=ifelse(MONTH_BEGIN_PAY_DT<=last_pymnt_d+months(1),1,0)]
# 
# # For other loans (survived only the month the last payment was received)
# loan_36_hist[!((DEFAULT==1)|(DELINQ==1)),SURVIVED:=ifelse(MONTH_BEGIN_PAY_DT<=last_pymnt_d,1,0)]
# loan_60_hist[!((DEFAULT==1)|(DELINQ==1)),SURVIVED:=ifelse(MONTH_BEGIN_PAY_DT<=last_pymnt_d,1,0)]
# 
# 
# # Removing Observations when loan has not survived
# loan_36_prune <- as.tbl(loan_36_hist)%>%
#     filter(SURVIVED==1)%>%
#     mutate(DEFAULT = ifelse((last_pymnt_d+months(1)==MONTH_BEGIN_PAY_DT),0,1))
# 
# loan_60_prune <- as.tbl(loan_60_hist)%>%
#     filter(SURVIVED==1)
# 
# # Changing Default and Delinquent columns to be 1 only in the month of default 
# # and removing survived column
# loan_36_prune <- mutate(loan_36_prune,SURVIVED = NULL)
# loan_36_prune <- as.data.table(loan_36_prune)    
# loan_36_prune[DEFAULT==1,DEFAULT:=ifelse(MONTH_BEGIN_PAY_DT<last_pymnt_d+months(1),0,1)]
# loan_36_prune[DELINQ==1,DELINQ:=ifelse(MONTH_BEGIN_PAY_DT<last_pymnt_d+months(1),0,1)]
# 
# loan_60_prune <- mutate(loan_60_prune,SURVIVED = NULL)
# loan_60_prune <- as.data.table(loan_60_prune)    
# loan_60_prune[DEFAULT==1,DEFAULT:=ifelse(MONTH_BEGIN_PAY_DT<last_pymnt_d+months(1),0,1)]
# loan_60_prune[DELINQ==1,DELINQ:=ifelse(MONTH_BEGIN_PAY_DT<last_pymnt_d+months(1),0,1)]
# 
# ### Recombining Data Sets
# loan_pruned<-rbind(loan_36_prune,loan_60_prune)
# 
# 
# ### Simplifying Dataset to fit model
# loan_mdl<-as.tbl(loan_pruned)%>%
#     select(id,loan_amnt,term,int_rate,issue_d,last_pymnt_d,addr_state,fico_range_low,
#            MATURITY,VINTAGE,MONTH_BEGIN_PAY_DT,MONTH_END_PAY_DT,
#            MONTH.AGE.BEGIN,MONTH.AGE.END,DEFAULT,DELINQ,
#            FULLPAID,CURRENT,PREPAID)%>%
#     arrange(id,MONTH.AGE.BEGIN)
#     
# 
# ### Getting Unemployment Data
# unemp.data<-read.csv("State Unemployment Data Monthly History.csv",header = T)
# unemp.data$state<-as.factor(str_trim(unemp.data$state))
# unemp.data$date<-as_date(unemp.data$date)
# unemp.data<-as.data.table(unemp.data)
# 
# ### Joining with Unemployment Data
# loan_mdl<-as.data.table(loan_mdl)
# loan_mdl[unemp.data,UNEMP.RT_ISS:=i.value,on=c(addr_state="state",issue_d="date")]
# loan_mdl[unemp.data,UNEMP.RT.BEGIN_DT:=i.value,on=c(addr_state="state",MONTH_BEGIN_PAY_DT="date")]
# 

### Saving as a file for each vintage and term

# Savings Files.R script