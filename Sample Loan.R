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
loan_dt<-loan_mdl[(id %in% loan_ids),]

N<-length(loan_ids)


write.csv(loan_dt,"Sample Loans CA 36 2012_2015.csv")


#####################################################
# state <- "CA"
vintage <-c(2011:2015)
TERM <-"36 months"
sample_size<-100000

loan_mod_s<-loan_mod[(VINTAGE %in% vintage)&(term == TERM),]

## Sampling Loans
loan_ids<-if(length(loan_mod_s$id)>sample_size){
    sample(loan_mod_s$id,sample_size)
} else{
    sample(loan_mod_s$id,length(loan_mod_s$id))
}



## Select data by vintage, term and state
loan_dt<-loan_mdl[(id %in% loan_ids),]

N<-length(loan_ids)


write.csv(loan_dt,"Sample Loans 36 2011_2015.csv")

