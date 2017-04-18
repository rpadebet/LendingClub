loan_lc_dt<-loan_lc[,date_cols,with=FALSE]
loan_lc_dt$issue_d<-gsub("^","01-",x = loan_lc_dt$issue_d)
loan_lc_dt$earliest_cr_line<-gsub("^","01-",x = loan_lc_dt$earliest_cr_line)


head(loan_lc$int_rate)

library(stringi)
dates <- loan_lc_sub$earliest_cr_line

dates_len<-sapply(dates,str_length)
dates_df<-as.data.frame(cbind(dates,dates_len))


clean_dates<-function(d){
    
    if(str_length(d)==6){
        d1<-as.Date(gsub("^","01-",x=d),format="%d-%b-%y")
    }
    else {
        d1<-as.Date(gsub("^","01-",x=d),format="%d-%b-%Y")
    }
    return(d1)
}

clean_history<-as_date(ifelse(stri_length(loan_lc_sub$earliest_cr_line)==6,
                                             as.Date(gsub("^","01-",x=loan_lc_sub$earliest_cr_line),format="%d-%b-%y"),
                                             as.Date(gsub("^","01-",x=loan_lc_sub$earliest_cr_line),format="%d-%b-%Y") ))



library(data.table)
library(car)
as.data.table(loan_test)

loan_defaults<-loan_test[PERFORM==0,.(id,loan_amnt,total_pymnt,term,int_rate,grade,emp_length,annual_inc,dti,
                                      issue_d,last_pymnt_d,purpose,addr_state,earliest_cr_line,fico_range_low,RETURN,AGE)]

loan_paid<-loan_test[PERFORM==1,.(id,loan_amnt,total_pymnt,term,int_rate,grade,emp_length,annual_inc,dti,
                                      issue_d,last_pymnt_d,purpose,addr_state,earliest_cr_line,fico_range_low,RETURN,AGE)]


loan_lc_sub[id==1069800]

par(mfrow=c(2,1))
hist(as.numeric(loan_defaults$AGE))
hist(as.numeric(loan_paid$AGE))


par(mfrow=c(2,1))
hist(as.numeric(loan_defaults$RETURN))
hist(as.numeric(loan_paid$RETURN))