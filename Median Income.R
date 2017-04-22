## Median Household Income Data
# Got from Census.gov. Data available per state per year (county data also available but unusable for our purpose)

filename<-paste0("./Median Income/SAIPESNC_19APR17_12_43_03_32.csv")

MedianIncome<-fread(filename,header=T)
head(MedianIncome)

MedianIncome<-MedianIncome[,.(Year,`State / County Name`,`Median Household Income in Dollars`,`All Ages in Poverty Percent`)]
colnames(MedianIncome)<-c("Year","State","MedIncome","PovertyPerc")
MedianIncome[,`MedianInc`:=gsub("$","",MedianIncome$MedIncome,fixed = T)]
MedianIncome[,`MedianInc`:=gsub(",","",MedianIncome$MedianInc,fixed = T)]
MedianIncome$MedIncome<-NULL
