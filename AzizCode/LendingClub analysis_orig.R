## Load survival package
library(survival)
library(rms)

# library(xlsx)
library(lubridate)
library(plyr)
library(data.table)

setwd("c:/Aziz/Rfiles/data/")

dir()


##### read and clean up raw loan data ###############
xorig = read.csv("LendingClub.csv")
unemporig = read.csv("state unemployment data monthly.csv")

# make a copy so that I don't have to keep re-reading the data
x = xorig
unempdata = unemporig

##### cleanup unemp data
names(unempdata)
summary(unempdata)

unempdata$date = mdy(unempdata$date)


##### cleanup loan data
names(x)
summary(x)


# format as date
x$issue_d = mdy(x$issue_d)
x$last_pymnt_d = mdy(x$last_pymnt_d)
x$earliest_cr_line = mdy(x$earliest_cr_line)

# remove extranous strings from "term" field and make numeric
temp = substr(as.character(x$term),1,2)
temp2 = as.numeric(temp)
x$term = temp2

# drop empty records
temp1 = with(x, !is.na(issue_d) & !is.na(term))

# review  # of obs dropped
sum(!temp1)
x = x[temp1,]

##### perform calculations

# calculate age of loan
x$myage = as.numeric(with(x,last_pymnt_d - issue_d))
x$endmonth = ceiling(x$myage/30.5)
x$endmonth[x$endmonth == 0] = 1

# since loan history is truncated, fill in endmonth where loan is not yet due
currentdate = max(x$last_pymnt_d, na.rm = T)
x$islive = with(x, as.numeric(currentdate - issue_d)<term*30.5)
sum(x$islive)

# construct default flag 
x$isdefault = with(x, (!islive & total_pymnt<loan_amnt) | (islive & (currentdate - last_pymnt_d) > 4*30.5 & total_pymnt<loan_amnt) )
# x$isdefault = with(x, total_pymnt<loan_amnt)

x$isdefault_loss = with(x, isdefault & total_pymnt<loan_amnt)
#x2$isdefault_loss = with(x2, isdefault & total_pymnt<loan_amnt)

# construct flag for default within 1yr of origination
x$isdefault1 = with(x, isdefault & myage <=366 & !is.na(myage))
summary(x)

x$isdefault18 = with(x, isdefault & myage <=18*30.5 & !is.na(myage))


# add some additional date information
x$issue_year = with(x, lubridate::year(issue_d))

ddply(x, .(issue_year,term), plyr::summarize, annual_obs = length(issue_d))



############# subset data for further analysis ################
# iswindow = with(x, 2012<= issue_year & issue_year <=2013 )
# iswindow = with(x, 2010<= issue_year & issue_year <=2015 & x$issue_d <= as.Date("2012-06-30"))
iswindow = with(x, 2011<= issue_year & issue_year <=2015 )

# buffer age by 2 extra months since we have rounded off issue_dt to 1st of each month and to add a bit more padding
isok = with(x, !is.na(issue_d) & term == 36 & myage <= (2+ term)*31 & !is.na(myage) & !is.na(int_rate) & !is.na(UNEMP_RATE_ISSUE_DT))

tokeep = iswindow & isok 
#tokeep = iswindow &  isok
sum(tokeep)


x2 = x[tokeep ,  ]
#x2$is2013 = x2$issue_year == 2013
nrow(x2)
summary(x2)

# examine observations where age > term

# xtemp = x[ is2012h1 & with(x, !is.na(issue_d) & term == 36 & myage > term * 31) ,]
# nrow(xtemp)
# summary(xtemp)

# add unemployment bands

# makebands = function(xfoo,upper1,upper2){
#   
#   u1 = quantile(xfoo, upper1)
#   u3 = quantile(xfoo, upper2)
#   
#   nobs = length(xfoo)
#   u1 = rep(u1,nobs)
#   u3 = rep(u3,nobs)
#   
#   tempout = matrix(NA,nrow = nobs)
#   tempout[xfoo<=u1] = 1
#   tempout[!(xfoo<=u1) & (xfoo<=u3)] = 2
#   tempout[!(xfoo<=u1) & !(xfoo<=u3)] = 3
#   
#   
#   # explicitly assigning faster than ifelse
#   
#   #tempout = as.integer(tempout)  #as.integer makes as.factor run faster
#   #tempout = ifelse(xfoo <= u1, 1, ifelse(xfoo <= u3,2,3))
#   #tempout = factor(tempout)
#   tempoutfactor = factor(tempout, nmax = 3)
#   
#   return(tempoutfactor)
# }

makebandsv2 = function(xin,vbands){
  qlocal = rank(xin, ties.method = "average")/length(xin)
  xout = findInterval(qlocal,vbands)+1
  return(xout)
}

x2$unempbins = makebandsv2(x2$UNEMP_RATE_ISSUE_DT,c(.25,.75))
x2$intbins = makebandsv2(x2$int_rate ,c(.25,.75))
x2$lgd = 1- x2$total_pymnt/x2$loan_amnt
x2$lgd[!x2$isdefault] = NA

summary(x2$lgd)
summary(x2$lgd[!is.na(x2$lgd)])

# note -- use plyr::summarize as other packages also have the summarize() function

ddply(x2, .(intbins,unempbins), plyr::summarize, 
      meanrate = mean(int_rate), 
      meanump = mean(UNEMP_RATE_ISSUE_DT),
      pd = mean(isdefault),
      pd1 = mean(isdefault1),
      nobs = length(isdefault)
#      nobs1 = length(isdefault1)
)


x2 = ddply(x2, .(issue_year), plyr::mutate, 
      intbinsyr = makebandsv2(int_rate,c(.25,.75)),
      unempbinsyr = makebandsv2(UNEMP_RATE_ISSUE_DT,c(.25,.75))
      )

# x2 = ddply(x2, .(issue_year), plyr::mutate, 
#            intbinsyr = cut(int_rate, 3, labels = FALSE),
#            unempbinsyr = cut(UNEMP_RATE_ISSUE_DT ,3, labels = FALSE)
#)
# ddply(x2, .(issue_year,intbins,unempbins), plyr::summarize, 
#       meanrate = mean(int_rate), 
#       meanump = mean(UNEMP_RATE_ISSUE_DT),
#       pd = mean(isdefault),
#       pd1 = mean(isdefault1),
#       nobs = length(isdefault),
#       nobs1 = length(isdefault1)
# )

# summarize by each bin
default_summary = ddply(x2, .(issue_year,intbinsyr,unempbinsyr), plyr::summarize, 
      meanrate = mean(int_rate), 
      meanump = mean(UNEMP_RATE_ISSUE_DT),
      pd = 100*mean(isdefault),
#      pd1 = 100*mean(isdefault1),
      meanlgd = mean(lgd, na.rm = T),
#      pd18 = mean(isdefault18),
#      nobs = length(isdefault),
      nobs = length(isdefault)
)

# keep track of highest unemp bin in each (year, int_rate) bin
default_summary = ddply(default_summary, .(issue_year,intbinsyr), mutate, 
             hi_pd = pd[unempbinsyr == 3], 
             hi_ump = meanump[unempbinsyr == 3],
             minobs = min(nobs)
             )

default_summary$pdratio = with(default_summary, hi_pd/pd)
default_summary$delta_ump = with(default_summary, hi_ump - meanump)

default_summary


## write a generic plot function to work with d_ply
plot_function = function(zin, plotcolor,yvarname,binvarid, ...){
  # write out a function to plot the yvar vs year of issue for each bin of interest
  # this is called by d_ply, so input is a data.frame
  
  xvar = zin$issue_year
  yvar = zin[,yvarname]
  
  # keep track of bin_no to choose plot color
  bin_no = zin[,binvarid]

    points(xvar, yvar, col = plotcolor[bin_no], cex = 2, lwd = 2)
  lines(xvar, yvar,  col = plotcolor[bin_no], lwd = 2, ...)
}


##### plot default rates for unempbinsyr == 1 & 3
# only keep first bin, as its got all the data necessary for this plot
plot_color = c("green3", "orange3", "red3")

zzz = default_summary[default_summary$unempbinsyr == 1, ]

# create blank plot
plot(zzz$issue_year, NA*zzz$issue_year, ylim = c(0.0,20), xlab = "Year of Loan Issue", ylab = "Default Rates")

d_ply(zzz, .(intbinsyr), plot_function, plot_color, "pd", "intbinsyr")

# plot defaults for unempbinsyr == 3

zzz = default_summary[default_summary$unempbinsyr == 3, ]
d_ply(zzz, .(intbinsyr), plot_function, plot_color, "pd", "intbinsyr", lty = 2)


legend(2011,4, c("Low Risk", "Moderate Risk", "Risky"), text.col = plot_color)



##### plot the default rate ratio

# only keep first bin, as its got all the data necessary for this plot
zzz = default_summary[default_summary$unempbinsyr == 1, ]

# create blank plot
plot(zzz$issue_year, NA*zzz$issue_year, ylim = c(0.9,1.4), xlab = "Year of Loan Issue", ylab = "Default Rate Ratio")
abline(1,0, col = "lightgrey")

d_ply(zzz, .(intbinsyr), plot_function, plot_color, "pdratio", "intbinsyr")

legend(2013,1.3, c("Safe", "Average Risk", "Risky"), text.col = plot_color)



##### plot unemployment rates for intbinsyr == 2. Only plot low and high unemployment
# only keep first bin, as its got all the data necessary for this plot
zzz = default_summary[default_summary$intbinsyr == 2, ]

# create blank plot
plot(zzz$issue_year, NA*zzz$issue_year, ylim = c(0.0,15), xlab = "Year of Loan Issue", ylab = "Unemployment Rate")

# temp = heat_hcl(4, c = c(80,30), l = c(30, 90), power = c(1/5, 1.5))
# plot_color_unemp = temp[c(3,2,1)]

zzz = default_summary[default_summary$intbinsyr == 2, ]
plot_function(zzz[zzz$unempbinsyr == 1,],rep("grey30",3), "meanump", "unempbinsyr")
plot_function(zzz[zzz$unempbinsyr == 3,],rep("grey30",3), "meanump", "unempbinsyr", lty = 2)

# d_ply(zzz, .(unempbinsyr), plot_function, plot_color_unemp, "meanump", "unempbinsyr")
#legend(2011,4, c("Low Unemployment States", "Medium  Unemployment States", "High Unemployment States"), text.col = plot_color_unemp)


##### Cox proportional hazard rate models

x2$int_rate_norm = x2$int_rate-12
x2$UNEMP_RATE_ISSUE_DT_norm = x2$UNEMP_RATE_ISSUE_DT - 6

z2 = x2[x2$issue_year>=2010,]
nrow(z2)

z2$yearfactor = as.factor(z2$issue_year)
coxout = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm+ as.factor(issue_year) +UNEMP_RATE_ISSUE_DT_norm, data = z2 )
summary(coxout)

coxout2 = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm + as.factor(issue_year) +UNEMP_RATE_ISSUE_DT_norm*as.factor(issue_year), data = z2 )
summary(coxout2)

coxout3 = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm*as.factor(issue_year) +UNEMP_RATE_ISSUE_DT_norm*as.factor(issue_year), data = z2 )
summary(coxout3)

coxout4 = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm*as.factor(issue_year) +UNEMP_RATE_ISSUE_DT_norm, data = z2 )
summary(coxout4)

coxout5 = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm+ UNEMP_RATE_ISSUE_DT_norm*as.factor(intbinsyr)*as.factor(issue_year), data = z2 )
summary(coxout5)

coxout6 = coxph(Surv(z2$myage,z2$isdefault) ~ UNEMP_RATE_ISSUE_DT_norm*as.factor(intbinsyr)*as.factor(issue_year), data = z2 )
summary(coxout5)

# pull out required coeffs

pull_coeffs = function(coxoutput, coeffstring){
  
  labelnames = names(coxoutput$coefficients)
  tosearch = paste0(coeffstring, "*")
  
  isumpcoeffs = grepl(tosearch, labelnames )
  tempnames = labelnames[isumpcoeffs]
  tempcoeffs = coxout3$coefficients[isumpcoeffs]
  
  temporder = order(tempnames)
  tempnames = tempnames[temporder]
  tempcoeffs = tempcoeffs[temporder]
  
  isyearcoeffs = grepl("201*", tempnames )
  firstyear = tempcoeffs[!isyearcoeffs]
  yearcoeffs = firstyear + tempcoeffs[isyearcoeffs]
  allcoeffs = c(firstyear,yearcoeffs)
  return(allcoeffs)
}


make_plot = function(q, ytitle){
  plot(2010:2015, q, lwd = 2, cex = 2, xlab = "Year of Loan Issue", ylab =ytitle)
  lines(2010:2015, q, lwd = 2)
  abline(0,0, col = "lightgrey")
}

coeffs2plot = pull_coeffs(coxout3, "UNEMP_RATE_ISSUE_DT")
make_plot(coeffs2plot, "Unemployment Rate Coefficient")

coeffs2plot = pull_coeffs(coxout3, "int_rate_norm")
make_plot(coeffs2plot, "Interest Rate Coefficient")

# plot baseline survival curve for all covariates equal to zero (which means for 2010, int_rate = 12%, unemp = 6%)
w = basehaz(coxout3, centered = FALSE)

hazard_rate = (1/(1-lag(w$hazard)))* c(0, diff(w$hazard)/diff(w$time/365))

hazard_smooth = predict(loess(hazard_rate~w$time, degree = 1, span = .1))

plot(w$time/365, w$hazard, type = "l", col = "blue")
points(w$time/365, hazard_smooth)

plot(w$time/365, hazard_rate, type = "l", col = "blue")

# regression

lmout = lm(isdefault~as.factor(issue_year)*int_rate_norm + UNEMP_RATE_ISSUE_DT_norm, data = x2, subset = issue_year>=2011)
summary(lmout)

t = hist(x2$int_rate, breaks = 15)

t = loess(IS_DEFAULT~int_rate, x2, degree = 1, enp.target = 10)
plot(x2$int_rate, predict(t))

lmout = lm(IS_DEFAULT~int_rate*UNEMP_RATE_ISSUE_DT, x2)
summary(lmout)


## make up data in format for cox() 

temp1 = isok & with(x, 2012<=year & year<=2013 )
sum(temp1)
z = x2[temp1, c("id", "isdefault", "myage", "issue_d", "last_pymnt_d")]

# rep.row<-function(x,n){
#   matrix(rep(x,each=n),nrow=n)
# }
# 
# rep.col<-function(x,n){
#   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
# }

# reformat_data = function(z){
#   
#   nloans = nrow(z)
#   nmonths = 40
# 
#   # isdefaultmat = matrix(data = -1, nloans,nmonths)
#   # tendmat = rep.row(1:nmonths,nloans)
#   
#   z$endmonth = ceiling(z$myage/30.5)
#   z$endmonth[z$myage == 0] = 1
# 
#   if(exists("all.df") ) rm(all.df)
#   if(exists("curr.df") ) rm(curr.df)
# 
#   all.df = data.frame()
# 
#   for (j in 1:nloans){
#     print(j)
#     jend = z$endmonth[j]
#     time2 = 1:jend
#     jstatus = rep(FALSE,jend)
#     jstatus[jend] = z$isdefault[j]
#     jid = rep(z$id[j],jend)
#     curr.df = data.frame(id = jid,tstart = time2 - 1, tend = time2, jstatus)
#     all.df = rbind(all.df,curr.df)
#   }
# return(all.df)
# }
# 
# reformat_data(z)
# 
# xtemp = x2  
# tokeep = with(xtemp, endmonth == 30 & isdefault == TRUE)
# sum(tokeep)




#z = x2[1:1000,]


if(exists("all.df") ) rm(all.df)
all.df = data.frame()
if(exists("curr.df") ) rm(curr.df)
logical_array = c(FALSE,TRUE)

maxmonth = 40
for (j in 1:maxmonth){
  print(j)
  
  for(k in 1:2){
    logical_k = logical_array[k]
    tokeep = with(z, endmonth == j & isdefault == logical_k)
    ncurr = length(tokeep)
    if( ncurr>0){
    current_ids = z$id[tokeep]
    curr.df = convert_to_cox_format(current_ids, j,logical_k)
    all.df = rbind(all.df,curr.df)
    }
  }
}

sum(z$endmonth) + sum(z$endmonth == 0)
nrow(all.df)

sum(z$isdefault)
sum(all.df$isdefault)

summary(all.df)

convert_to_cox_format = function(idarray,age, endstate){
# converts obs to a standard format
  # NOTE designed to only handle loans where each loan is of the same age and has the same terminal end state
  # inputs
  # indexarray    ID to keep track of each loan
  # age           age of each loan, as an integer
  # endstate      end state of each loan
  
  nloans = length(idarray)
  temp1 = rep(idarray, each = age )
  time2 = rep(1:age,nloans)
  isdefault_array = rep( c(rep(FALSE, age-1), endstate), nloans)
  temp.df = data.frame(id = idarray,time2,isdefault = isdefault_array)
  return(temp.df)
}
   
tokeep = with(x2, endmonth == 30 &  isdefault == 1)
z = x2[tokeep,]
sum(tokeep)
t = convert_to_cox_format(x2$id[tokeep],30,1) 
summary(t)

sum(z$endmonth) + sum(z$endmonth == 0)
nrow(t)

sum(z$isdefault)
sum(t$isdefault)
