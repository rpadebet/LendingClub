
head(loans)
library(data.table)
library(dplyr)

loans<-as.data.table(loans)

loans[is.na(AGE),last_pymnt_d:=issue_d][is.na(AGE),AGE:=0]
loans[unemp.data,UNEMP_RATE_LASTPAY_DT:=i.value,on=c(addr_state="state",last_pymnt_d="date")]

loan_36<-loans[term=="36 months",]

loan_60<-loans[term=="60 months",]

loan_36_summary<-as.tbl(loan_36)%>%
    select(loan_amnt,total_pymnt,int_rate,grade,addr_state,issue_d,
           UNEMP_RATE_ISSUE_DT,fico_range_low,ifelse(is.na(AGE),0,AGE),PERFORM)%>%
    group_by(addr_state,grade,year(issue_d))%>%
    summarise(sum(loan_amnt),sum(total_pymnt),mean(int_rate),
              mean(UNEMP_RATE_ISSUE_DT),mean(AGE),mean(fico_range_low),1-mean(as.numeric(PERFORM)-1))

write.csv(loan_36_summary,"36_lc_summary_stats.csv")


loan_AK_36_summary<-as.tbl(loan_36)%>%
    filter(addr_state=="AK")%>%
    select(loan_amnt,total_pymnt,int_rate,grade,addr_state,issue_d,
           UNEMP_RATE_ISSUE_DT,fico_range_low,AGE,PERFORM)%>%
    group_by(addr_state,grade,year(issue_d))%>%
    summarise(sum(loan_amnt),sum(total_pymnt),mean(int_rate),
              mean(UNEMP_RATE_ISSUE_DT),mean(AGE),mean(fico_range_low),1-mean(as.numeric(PERFORM)-1))

View(loan_AK_36_summary)
colnames(loan_AK_36_summary)<-c("STATE","GRADE","ISSUE.YEAR","AVG.LOAN.AMT","AVG.PYMT.AMT","AVG.INT.RT","AVG.UNEMP.RT","AVG.AGE","AVG.FICO","DEFAULTS")

loan_AK_36<-loan_36[addr_state=="AK",]
loan_AK_36_hist<-loan_AK_36[,.(id,issue_d,last_pymnt_d,PERFORM)]
loan_AK_36_hist[,MATURITY:=issue_d+months(36)][,PREPAID:=0]
loan_AK_36_hist[(PERFORM==1)&(year(last_pymnt_d)<2017),PREPAID:=ifelse(last_pymnt_d<MATURITY,1,0)]

for(i in seq(1:36)){
    loan_AK_36_hist[,paste0("M",i):= issue_d+months(i)]
}
library(tidyr)

loan_AK_36_hist_long<-as.tbl(loan_AK_36_hist)%>%
    gather(key = MONTH.AGE, value=NEXT_PAY_DT,M1:M36)


loan_AK_36_hist_long<-as.data.table(loan_AK_36_hist_long)

loan_AK_36_hist_long[,SURVIVED:=ifelse(NEXT_PAY_DT<=last_pymnt_d,1,0)]
    
    