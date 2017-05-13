## This data is got from https://www.irs.gov/uac/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi

#'Individual Income Tax ZIP Code Data
#' ZIP Code data show selected income and tax items classified by State, ZIP Code, 
#' and size of adjusted gross income. 
#' Data are based on individual income tax returns filed with the IRS 
#' and are available for Tax Years 1998, 2001, and 2004 through 2014. 
#' The data include items, such as:

# * Number of returns, which approximates the number of households
# * Number of personal exemptions, which approximates the population
# * Adjusted gross income 
# * Wages and salaries
# * Dividends before exclusion
# * Interest received  

# ZIP Code data for years 1998, 2001, 
# and 2004 through 2014 are available as single Zip files containing all State Excel files.

## Libraries
library(data.table)
library(tidyverse)
library(lubridate)

## 2011-2014 Data Downloaded and stored in folder "IRS Income Data by Zipcode"

folder<-"IRS Income Data by Zipcode"

D_2011<-fread(paste0(folder,"/11zpallagi.csv"),header=T)
D_2012<-fread(paste0(folder,"/12zpallagi.csv"),header=T)
D_2013<-fread(paste0(folder,"/13zpallagi.csv"),header=T)
D_2014<-fread(paste0(folder,"/14zpallagi.csv"),header=T)


# AGI_STUB
# Size of adjusted gross income
# 0 = No AGI Stub
# 1 = '$1 under $25,000'
# 2 = '$25,000 under $50,000'
# 3 = '$50,000 under $75,000'
# 4 = '$75,000 under $100,000'
# 5 = '$100,000 under $200,000'
# 6 = '$200,000 or more'


# We are interested in
cols_inc2011<-c("STATEFIPS", #Federal Identification Code
            "STATE", # State
            "ZIPCODE", # 5 digit zipcode
            "agi_stub", # AGI_STUB below
            "A00100", # AGI
            "N1", # Number of Returns
            "N00200", # Number of Returns with Salaries and Wages
            "N06500", # Number of Returns with Income Tax
            "A06500", # Total Tax amount
            "A04800"  # Taxable Income amount
            )

cols_inc2012<-c("STATEFIPS", #Federal Identification Code
             "STATE", # State
             "zipcode", # 5 digit zipcode
             "AGI_STUB", # AGI_STUB below
             "A00100", # AGI
             "N1", # Number of Returns
             "N00200", # Number of Returns with Salaries and Wages
             "N06500", # Number of Returns with Income Tax
             "A06500", # Total Tax amount
             "A04800"  # Taxable Income amount
)

cols_inc2013<-c("STATEFIPS", #Federal Identification Code
                "STATE", # State
                "zipcode", # 5 digit zipcode
                "agi_stub", # AGI_STUB below
                "A00100", # AGI
                "N1", # Number of Returns
                "N00200", # Number of Returns with Salaries and Wages
                "N06500", # Number of Returns with Income Tax
                "A06500", # Total Tax amount
                "A04800"  # Taxable Income amount
)

cols_inc2014<-c("STATEFIPS", #Federal Identification Code
                "STATE", # State
                "zipcode", # 5 digit zipcode
                "agi_stub", # AGI_STUB below
                "A00100", # AGI
                "N1", # Number of Returns
                "N00200", # Number of Returns with Salaries and Wages
                "N06500", # Number of Returns with Income Tax
                "A06500", # Total Tax amount
                "A04800"  # Taxable Income amount
)

cols_named<-c("FIPS", #Federal Identification Code
                "State", # State
                "Zip", # 5 digit zipcode
                "AGIBracket", # AGI_STUB below
                "AGI", # AGI
                "NumReturns", # Number of Returns
                "NumReturnsWithSalary", # Number of Returns with Salaries and Wages
                "NumReturnsWithIncTax", # Number of Returns with Income Tax
                "TotalTax", # Total Tax amount
                "TotalTaxableIncome"  # Taxable Income amount
)

I_2011<-D_2011[,cols_inc2011,with=FALSE]
setnames(I_2011,cols_inc2011,cols_named)
I_2011<-I_2011[,YEAR:=2011]

I_2012<-D_2012[,cols_inc2012,with=FALSE]
setnames(I_2012,cols_inc2012,cols_named)
I_2012<-I_2012[,YEAR:=2012]

I_2013<-D_2013[,cols_inc2013,with=FALSE]
setnames(I_2013,cols_inc2013,cols_named)
I_2013<-I_2013[,YEAR:=2013]

I_2014<-D_2014[,cols_inc2014,with=FALSE]
setnames(I_2014,cols_inc2014,cols_named)
I_2014<-I_2014[,YEAR:=2014]

## Since 2015 and 2016 data isn't available yet, let us assume they look like 2014 data

I_2015<-D_2014[,cols_inc2014,with=FALSE]
setnames(I_2015,cols_inc2014,cols_named)
I_2015<-I_2015[,YEAR:=2015]

I_2016<-D_2014[,cols_inc2014,with=FALSE]
setnames(I_2016,cols_inc2014,cols_named)
I_2016<-I_2016[,YEAR:=2016]

Income_Data_Raw<-rbind(I_2011,I_2012,I_2013,I_2014,I_2015,I_2016)


Income_Data_Raw<-as.tbl(Income_Data_Raw)

library(stringr)

Income_Data<-as.tbl(Income_Data_Raw)%>%
    filter(!(Zip %in% c(0,99999)))%>%
    mutate(Zip3 = paste0(as.integer(Zip/100),"xx"))%>%
    mutate(Zip3 = ifelse(nchar(Zip3)==4,paste0("0",Zip3),Zip3))%>%
    select(State,YEAR,Zip3,AGI,NumReturns)%>%
    group_by(State,Zip3,YEAR)%>%
    summarise(AvgIncome = sum((AGI*NumReturns))/sum(NumReturns) )%>%
    mutate(AvgIncome = round(AvgIncome,0))%>%
    ungroup()%>%
    select(State,YEAR,Zip3,AvgIncome)%>%
    arrange(State,YEAR,Zip3,desc(AvgIncome))
    
           
write.csv(x = Income_Data,file = "Income Data.csv",row.names = F)


