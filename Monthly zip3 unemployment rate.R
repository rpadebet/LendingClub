## County Zip5, Zip3, Unemp Rate County Mapping from Aziz's analysis

load("~/R Projects/LendingClub/AzizCode/AzizData/unemp_annual_county.RData")
load("~/R Projects/LendingClub/AzizCode/AzizData/zip_county_table.RData")


unemp.data<-read.csv("State Unemployment Data Monthly History.csv",header = T)
unemp.data$state<-as.factor(str_trim(unemp.data$state))
unemp.data$date<-as_date(unemp.data$date)
unemp.data$year<-year(unemp.data$date)
unemp.data<-as.data.table(unemp.data)

unemp.county.annual<-as.data.table(unemp_annual_county)
## Selecting only interested columns
unemp.county.annual<-unemp.county.annual[!(is.na(zip3)) ,.(state_abbrev,year,zip3,county_factor)]
## transforming zip3 to match zip_code in loan data set
unemp.county.annual<-unemp.county.annual[,zip3:=paste0(zip3,"xx")]
## Creating wide table format for each zip3 adding 12 months
for(i in seq(1:12)){
    unemp.county.annual[,paste0("M",i):= ymd(paste0(year,"0101"))+months(i-1)]
}

## Converting to long form. repeating each previous record 12 times : once for each month
unemp.county.annual<-as.tbl(unemp.county.annual)%>%
    gather(key = MONTH,value = date,M1:M12)%>%
    mutate(MONTH=NULL)%>%
    arrange(zip3,date,year)

## Joining with monthly state unemployment table
unemp.county.annual<-as.data.table(unemp.county.annual)
unemp.county.annual[unemp.data,unemp_rate_state_month:=i.value,on=c(state_abbrev="state",date="date")]

## Estimating unemployment rate at zip3 level as state unemployment rate for the month*county factor
# ---- this assumes county factor calculated at beginning of the year stays constant throughout the year
unemp.data.county<-unemp.county.annual[,value:=county_factor*unemp_rate_state_month]


## Save the file
write.csv(unemp.data.county,"Monthly Zip3 Unemployment Rate.csv")
