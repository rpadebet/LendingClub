loan_lc_dt<-loan_lc[,date_cols,with=FALSE]
loan_lc_dt$issue_d<-gsub("^","01-",x = loan_lc_dt$issue_d)
loan_lc_dt$earliest_cr_line<-gsub("^","01-",x = loan_lc_dt$earliest_cr_line)


head(loan_lc$int_rate)


