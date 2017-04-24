#' Saving Files

#2007-2011
loan_hist_36_2007_2011 <- loan_mdl[(term=="36 months")&(VINTAGE %in% c(2007:2011)),]
write.csv(loan_hist_36_2007_2011,"LoanHistory/Loan History 36_2007_2011.csv")

loan_hist_60_2007_2011 <- loan_mdl[(term=="60 months")&(VINTAGE %in% c(2007:2011)),]
write.csv(loan_hist_60_2007_2011,"LoanHistory/Loan History 60_2007_2011.csv")

# 2012-2013
loan_hist_36_2012_2013 <- loan_mdl[(term=="36 months")&(VINTAGE %in% c(2012:2013)),]
write.csv(loan_hist_36_2012_2013,"LoanHistory/Loan History 36_2012_2013.csv")

loan_hist_60_2012_2013 <- loan_mdl[(term=="60 months")&(VINTAGE %in% c(2012:2013)),]
write.csv(loan_hist_60_2012_2013,"LoanHistory/Loan History 60_2012_2013.csv")

# 2014
loan_hist_36_2014 <- loan_mdl[(term=="36 months")&(VINTAGE==2014),]
write.csv(loan_hist_36_2014,"LoanHistory/Loan History 36_2014.csv")

loan_hist_60_2014 <- loan_mdl[(term=="60 months")&(VINTAGE==2014),]
write.csv(loan_hist_60_2014,"LoanHistory/Loan History 60_2014.csv")

# 2015
loan_hist_36_2015 <- loan_mdl[(term=="36 months")&(VINTAGE==2015),]
write.csv(loan_hist_36_2015,"LoanHistory/Loan History 36_2015.csv")

loan_hist_60_2015 <- loan_mdl[(term=="60 months")&(VINTAGE==2015),]
write.csv(loan_hist_60_2015,"LoanHistory/Loan History 60_2015.csv")
