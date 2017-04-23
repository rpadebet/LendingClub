### Script to sample loans and build cox proportional hazard model

state <- "CA"
vintage <-c(2012:2015)
TERM <-"36 months"
sample_size<-1000

loan_mod_s<-loan_mod[(VINTAGE %in% vintage)&(term == TERM)&(addr_state == state),]

## Sampling Loans
loan_ids<-if(length(loan_mod_s$id)>sample_size){
                sample(loan_mod_s$id,sample_size)
            } else{
                sample(loan_mod_s$id,length(loan_mod_s$id))
        }
                


## Select data by vintage, term and state
loan_dt<-loan_hist[(id %in% loan_ids),]
loan_dt[,MONTH.AGE:=gsub("M","",MONTH.AGE)]

N<-length(loan_ids)


write.csv(loan_dt,"Sample Loans CA 36 2012_2015.csv")

loan_summary<-as.tbl(loan_dt)%>%
    group_by(MONTH.AGE)%>%
    summarize(INT.RATE=sum(int_rate*SURVIVED)/sum(SURVIVED),
              FICO=sum(fico_range_low*SURVIVED)/sum(SURVIVED),
              UNEMP.RT.ISS = sum(UNEMP.RT_ISS*SURVIVED)/sum(SURVIVED),
              UNEMP.RT.CURR = mean(UNEMP.RT.PAY),
              CUM_DEFAULT= N-sum(SURVIVED))%>%
    arrange(CUM_DEFAULT)%>%
    
    

View(loan_summary)


