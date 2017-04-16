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


head(dates_df[dates_len==6,],100)


