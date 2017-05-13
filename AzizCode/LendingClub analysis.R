## Load survival package
library(survival)
library(rms)

# library(xlsx)
library(lubridate)
library(plyr)
library(data.table)

setwd("c:/Aziz/Rfiles/data/")

dir()


######### helper functions
calc_ztime = function(x){

  # calculates months since Jan-2010, with Jan-2010 == 1
  temp_month = month(x)
  temp_year = year(x)
  ztime = (temp_month - 1) + 12* (temp_year - 2010) + 1
  return(ztime)
}

fill_w_linear_model = function(xin,yin, lmformula){
  # fills in missing values of "y", by fitting a a linear model to the non-missing values
  # inputs
  # xin       X variables for linear interpolation
  # yin       y variables for linear interpolation
  #lmformula  formula for lm call, entered as a character string
  
  # output
  # predicted y values for the observations that have missing y values
  
  ismissing = is.na(y)
  
  lmout = lm(formula = as.formula(lmformula))
  ypred = predict(lmout,xin)
  ypred_missing = pred[ismissing]
  return(ypred_missing)
}


fill_missing_age = function(zin){
  # fills in missing values of myage using linear regression
  
  # inputs
  # z   data.frame containing values to use for building the model

  # output
  # predicted age for observations where myage is missing
  
  zin$guess_age = with(zin, total_pymnt/installment)
  lmout = lm(myage~ guess_age*as.factor(term), data = zin)

    # summary(lmout)
  
  # predict ages for missing myage values
  
  zinv2 = zin[is.na(zin$myage),]
  rm(zin)

    pred_age = predict(lmout, zinv2)
  
  # round off to nearest integer
  pred_age = floor(pred_age+.5)
  # bound predictions by 1 at the bottom and by term at the top
  pred_age[pred_age < 1] = 1
  pred_age[pred_age >zinv2$term] = zinv2$term[pred_age >zinv2$term]
  
  # only return values for missing myage values
  return( pred_age)
}


##### read  raw loan data ###############

load("alldata1.RData")


##### read and cleanup unemp data

unempdata = fread( "state unemployment data monthly.csv", drop = 1)
names(unempdata)
summary(unempdata)

names(unempdata)[1] = "state_unemp"

unempdata$date = mdy(unempdata$date)
unempdata$unemp_month = calc_ztime(unempdata$date)


##### merge unemp data with loan data
loan_n_unemp = merge(alldata1,unempdata, by.x = c("issue_d", "addr_state"), by.y = c("date", "state"), all.x = TRUE, all.y = FALSE)

# rename unemployment variable appropriately
temp = which("state_unemp" == names(loan_n_unemp))
names(loan_n_unemp)[temp] = "unemp_issue_d"


# add some additional  information
loan_n_unemp$issue_year = with(loan_n_unemp, lubridate::year(issue_d))
loan_n_unemp$zip3 = substr(loan_n_unemp$zip_code,1,3)

# merge in county-level data
load("unemp_annual_county.RData")

names(unemp_annual_county)

temp = merge(loan_n_unemp, unemp_annual_county, by.x = c("issue_year", "zip3"), by.y = c("year" ,"zip3"), all.x = TRUE)
loan_n_unemp = temp
loan_n_unemp$unemp_county_issue_d = with(loan_n_unemp, unemp_issue_d * county_factor)


save("loan_n_unemp", file = "loan_n_unemp.RData")


##### begin analysis ######
load("loan_n_unemp.RData")

##cleanup merged  data


x = loan_n_unemp

rm(loan_n_unemp)

names(x)
summary(x)


## check for bad records -- should not be any
temp1 = with(x, is.na(issue_d)  | is.na(term) | is.na(int_rate) | is.na(unemp_issue_d))
sum(temp1)


##### perform calculations

# calculate loan_time = all dates in terms of months since Jan-2010, with Jan-2010 == 1
x$issue_month = calc_ztime(x$issue_d)
x$last_month = calc_ztime(x$last_pymnt_d)

# construct default flag 
x$isdefault = grepl("Charged Off", x$loan_status)

#x$isdefault = with(x, (!islive & total_pymnt<loan_amnt) | (islive & (currentdate - last_pymnt_d) > 4*30.5 & total_pymnt<loan_amnt) )
# x$isdefault = with(x, total_pymnt<loan_amnt)

# keep track of defaults where total payment is less than loan amount
x$isdefault_loss = with(x, isdefault & total_pymnt<loan_amnt)


# calculate age of loan
x$myageorig = x$last_month - x$issue_month
x$myage = x$myageorig

x$myage[x$myage == 0] = 1

# since last_pymnt_d is missing for some loans which are charged off, corresponding value of myage is also missing.
# use regression to estimate missing age

x$myage[is.na(x$myageorig)] = fill_missing_age(x[x$isdefault,])

#summary(x$myage)

# construct flag for default within 1yr of origination
x$isdefault12 = with(x, isdefault & myage <=12 & !is.na(myage))
x$isdefault18 = with(x, isdefault & myage <=18 & !is.na(myage))
x$isdefault24 = with(x, isdefault & myage <=24 & !is.na(myage))
x$isdefault30 = with(x, isdefault & myage <=30 & !is.na(myage))
# summary(x)

# count observations for each year and each term
ddply(x, .(issue_year), plyr::summarize, 
      annual_obs_36 = length(issue_d[term == 36]),
      annual_obs_60 = length(issue_d[term == 60]) )



############# subset data for further analysis ################
# iswindow = with(x, 2010<= issue_year & issue_year <=2015 & x$issue_d <= as.Date("2012-06-30"))

# only keep data from 2011- 2015 as older data has issues
iswindow = with(x, 2011<= issue_year & issue_year <=2015 )

# buffer age by 6 extra months for late payments
isok1 = with(x, !is.na(issue_d) & !is.na(myage) & !is.na(int_rate) & !is.na(unemp_issue_d))
isok2 = with(x,  myage <= (6+ term)  & total_pymnt >=0 & loan_amnt > 0)
sum(!isok1)
sum(!isok2)

isok = isok1 & isok2

tokeep = iswindow & isok  & x$term == 36
#tokeep = iswindow &  isok
sum(tokeep)


x2 = x[tokeep ,  ]
#x2$is2013 = x2$issue_year == 2013
nrow(x2)
# summary(x2)

# examine observations where age > term
#summary(x2[x2$myage > 36,])



# add interst rate and unemployment bands
makebandsv2 = function(xin,vbands){
  qlocal = rank(xin, ties.method = "average")/length(xin)
  xout = findInterval(qlocal,vbands)+1
  return(xout)
}

x2$unempbins_county = makebandsv2(x2$unemp_county_issue_d,c(.25,.75))
x2$unempbins = makebandsv2(x2$unemp_issue_d,c(.25,.75))
x2$intbins = makebandsv2(x2$int_rate ,c(.25,.75))

# need to check this as a non-trivial fraction of the defaults have negative LGD. Its possible (because of interest), but needs to be checked
x2$lgd = 1- x2$total_pymnt/x2$loan_amnt
x2$lgd[!x2$isdefault] = NA

summary(x2$lgd [x2$isdefault])


# note -- use plyr::summarize as other packages also have the summarize() function

ddply(x2, .(intbins,unempbins), plyr::summarize, 
      meanrate = mean(int_rate), 
      meanump = mean(unemp_issue_d),
      pdraw = 100*mean(isdefault),
      pd12 = 100*mean(isdefault12),
      nobs = length(isdefault)
#      nobs1 = length(isdefault1)
)


# make bands by each year -- this is to ensure that the bins are "low" and "high" on a relative basis each year, even if the average level has drifted
x2 = ddply(x2, .(issue_year), plyr::mutate, 
      intbinsyr = makebandsv2(int_rate,c(.25,.75)),
      unempbinsyr = makebandsv2(unemp_issue_d,c(.25,.75)),
      unempcountybinsyr = makebandsv2(unemp_county_issue_d,c(.25,.75)))


# summarize by each bin
default_summary = ddply(x2, .(issue_year,intbinsyr,unempcountybinsyr), plyr::summarize, 
      meanrate = mean(int_rate), 
      meanump = mean(unemp_issue_d),
      pd18 = 100*mean(isdefault18),
      pd21 = 100*mean(isdefault & (myage <=21)),
      pd24 = 100*mean(isdefault & (myage <=24)),
      pd30 = 100*mean(isdefault & (myage <=30)),
      pdraw = 100*mean(isdefault),
      pdloss = 100*mean(isdefault_loss),
      profitability = mean(total_pymnt/loan_amnt),
      profitability2 = 100*(1/3)*(mean((total_pymnt+recoveries)/loan_amnt) - 1),
      meanlgd = mean(lgd, na.rm = T),
      nobs = length(isdefault)
)

# examine ratio of defaults at different time points to estimate the aging effect

# temp1 = default_summary = ddply(x2, .(issue_year,intbinsyr), plyr::summarize, 
#                                 meanrate = mean(int_rate), 
#                                 meanump = mean(unemp_issue_d),
#                                 pd18 = 100*mean(isdefault18),
#                                 pd21 = 100*mean(isdefault & (myage <=21)),
#                                 pd24 = 100*mean(isdefault & (myage <=24)),
#                                 pd30 = 100*mean(isdefault & (myage <=30)),
#                                 pdraw = 100*mean(isdefault),
#                                 pdloss = 100*mean(isdefault_loss),
#                                 meanlgd = mean(lgd, na.rm = T),
#                                 nobs = length(isdefault)
# )
# temp1

temp2 =  ddply(x2, .(issue_year), plyr::summarize, 
                                meanrate = mean(int_rate), 
                                meanump = mean(unemp_issue_d),
                                pd18 = 100*mean(isdefault18),
                                pd21 = 100*mean(isdefault & (myage <=21)),
                                pd24 = 100*mean(isdefault & (myage <=24)),
                                pd30 = 100*mean(isdefault & (myage <=30)),
                                pdraw = 100*mean(isdefault),
                                pdloss = 100*mean(isdefault_loss),
                                meanlgd = mean(lgd, na.rm = T),
                                nobs = length(isdefault)
)
temp2

# scale raw default rates to obtain the ones used for analysis
default_summary$pd = default_summary$pdraw
is2014 = default_summary$issue_year == 2014
is2015 = default_summary$issue_year == 2015

default_summary$pd[is2014] = default_summary$pdraw[is2014] * 1.1
default_summary$pd[is2015] = default_summary$pdraw[is2015] * 1.3

default_summary

# # keep track of highest unemp bin in each (year, int_rate) bin
# default_summary = ddply(default_summary, .(issue_year,intbinsyr), mutate, 
#              hi_pd = pd[unempbinsyr == 3], 
#              hi_ump = meanump[unempbinsyr == 3],
#              minobs = min(nobs)
#              )

# default_summary$pdratio = with(default_summary, hi_pd/pd)
# default_summary$delta_ump = with(default_summary, hi_ump - meanump)
# 
# default_summary$pd_pd18 = with(default_summary, pd/pd18)
# default_summary$pd_pd21 = with(default_summary, pd/pd21)
# default_summary$pd_pd24 = with(default_summary, pd/pd24)
# default_summary$pd_pd30 = with(default_summary, pd/pd30)
# 
# # keep a copy as a backup 
# default_summary_orig = default_summary
# 
# 
# ddply(default_summary, .(issue_year,intbinsyr), summarize, 
#       mean_pd_pd18 = mean(pd_pd18),
#       mean_pd_pd21 = mean(pd_pd21),
#       mean_pd_pd24 = mean(pd_pd24),
#       mean_pd_pd30 = mean(pd_pd30))
# 
# default_summary[, c(1,2,3,6,8,9,11)]

#default_summary = default_summary_orig[default_summary_orig$issue_year >= 2010, ]



################# plot results #########################################

## generic barplot function to work with d_ply
barplot_function = function(zin, plotcolor, yvarname,binvarid, ylim.value, ...){
  # plot the yvarname variable vs year of issue for each bin of interest
  # this is called by d_ply, so input is a data.frame
  # inputs
  # zin       data.frame
  #plotcolor  array of colors to use for the plot series
  
  names.arg.value = as.character(unique(zin[,binvarid]))
  
  temporder = with(zin, order(intbinsyr, unempcountybinsyr)) 
 zin = zin[temporder,]
  
  barplot( zin[,yvarname], beside = TRUE, 
           space = c(.3,1), 
           col = rep(plot_color,each = 2),
           names.arg = names.arg.value,
           offset = .0,
           density = rep(c(NA,20), 6) ,
           ylim = ylim.value, 
           lwd = 2,...)
  }

##### plot default rates
tokeep = with(default_summary, unempcountybinsyr!=2 & issue_year <=2015 )
temp1 = default_summary[tokeep,]
par(mfrow = c(1,5), mar = c(3,2.5,3,0))

tempfn = function(x,ylim.value, ...){ barplot_function(x,plot_color, 
                                            "pd", "issue_year", ylim.value, 
                                            cex.axis = 1.5, 
                                            cex.names = 1.75, 
                                            ...)}

# plot first function outside of the loop to put in the legend
tempfn(temp1[temp1$issue_year == 2011,],ylim.value = c(0,25))
legend(2,25, c("Low Risk", "Moderate Risk", "Risky"), text.col = plot_color, 
       horiz = "FALSE", cex = 1.4)

d_ply(temp1[temp1$issue_year!=2011,], .(issue_year), tempfn, c(0,25), axes = FALSE)


##### plot profitability2
tokeep = with(default_summary, unempcountybinsyr!=2 & issue_year <=2013 )
temp1 = default_summary[tokeep,]
par(mfrow = c(1,5), mar = c(3,2.5,3,0))

tempfn = function(x,ylim.value, ...){ barplot_function(x,plot_color, 
                                                       "profitability2", "issue_year", ylim.value, 
                                                       cex.axis = 1.5, 
                                                       cex.names = 1.75, 
                                                       ...)}

# plot first function outside of the loop to put in the legend
tempfn(temp1[temp1$issue_year == 2011,],ylim.value = c(0,5))
legend(2,5, c("Low Risk", "Moderate Risk", "Risky"), text.col = plot_color, 
       horiz = "FALSE", cex = 1.4)

d_ply(temp1[temp1$issue_year!=2011,], .(issue_year), tempfn, c(0,5), axes = FALSE)


##  generic plot function to work with d_ply
plot_function = function(zin, plotcolor,yvarname,binvarid, ...){
  # plot the yvarname variable vs year of issue for each bin of interest
  # this is called by d_ply, so input is a data.frame
  # inputs
  # zin     
  #plotcolor  array of colors to use for the plot series
  

  xvar = zin$issue_year
  yvar = zin[,yvarname]
  
  # keep track of bin_no to choose plot color
  bin_no = zin[,binvarid]

#  points(xvar, yvar, col = plotcolor[bin_no], cex = 2, lwd = 2)
#  lines(xvar, yvar,  col = plotcolor[bin_no], lwd = 2, ...)
  lines(xvar, yvar,  col = plotcolor[bin_no], cex = 2, lwd = 2,type = "o", ...)
}



##### plot default rates for unempbinsyr == 1 & 3
plot_color = c("green3", "orange3", "red3")

# create blank plot
plot_years = unique((sort(default_summary$issue_year)))
plot(plot_years, NA*plot_years, ylim = c(0.0,25), xlab = "Year of Loan Issue", ylab = "Default Rates")

# plots for unembninsyr == 1
zzz= default_summary[default_summary$unempbinsyr == 1, ] 
d_ply(zzz, .(intbinsyr), plot_function, plot_color, "pd", "intbinsyr")

d_ply(zzz, .(intbinsyr), function(x) summary(x))


# plots for unempbinsyr == 3
zzz = default_summary[default_summary$unempbinsyr == 3, ]
d_ply(zzz, .(intbinsyr), plot_function, plot_color, "pd", "intbinsyr", lty = 2)


legend(2011,3, c("Low Risk", "Moderate Risk", "Risky"), text.col = plot_color, horiz = "TRUE")



##### plot unemployment rates for intbinsyr == 2. Only plot low and high unemployment

# create blank plot
plot(plot_years, NA*plot_years, ylim = c(0.0,15), xlab = "Year of Loan Issue", ylab = "Unemployment Rate")

zzz = default_summary[default_summary$intbinsyr == 2, ]
plot_function(zzz[zzz$unempbinsyr == 1,],rep("grey30",3), "meanump", "unempbinsyr")
plot_function(zzz[zzz$unempbinsyr == 3,],rep("grey30",3), "meanump", "unempbinsyr", lty = 2)



##### plot profitability for unempbinsyr == 1 & 3
plot_color = c("green3", "orange3", "red3")

# create blank plot
plot_years = unique((sort(default_summary$issue_year)))
plot(plot_years, NA*plot_years, ylim = c(1.05,1.15), xlab = "Year of Loan Issue", ylab = "Default Rates", xlim = c(2011,2013))

# plots for unembninsyr == 1
zzz= default_summary[default_summary$unempbinsyr == 1, ] 
d_ply(zzz, .(intbinsyr), plot_function, plot_color, "profitability", "intbinsyr")


# plots for unempbinsyr == 3
zzz = default_summary[default_summary$unempbinsyr == 3, ]
d_ply(zzz, .(intbinsyr), plot_function, plot_color, "profitability", "intbinsyr", lty = 2)


legend(2011,3, c("Low Risk", "Moderate Risk", "Risky"), text.col = plot_color, horiz = "TRUE")

##### plot profitability2 for unempbinsyr == 1 & 3

# create blank plot
plot(plot_years, NA*plot_years, ylim = c(1.0,1.2), xlab = "Year of Loan Issue", ylab = "Default Rates", xlim = c(2011,2013))

# plots for unembninsyr == 1
zzz= default_summary[default_summary$unempbinsyr == 1 & default_summary$issue_year <=2013, ] 
d_ply(zzz, .(intbinsyr), plot_function, plot_color, "profitability2", "intbinsyr")


# plots for unempbinsyr == 3
zzz = default_summary[default_summary$unempbinsyr == 3, ]
d_ply(zzz, .(intbinsyr), plot_function, plot_color, "profitability2", "intbinsyr", lty = 2)


legend(2011,3, c("Low Risk", "Moderate Risk", "Risky"), text.col = plot_color, horiz = "TRUE")


##### make a bar plot of the profitability ###### 
# pull out bare minimum of rows and cols needed
temp = with(default_summary, default_summary[issue_year<=2013 & unempbinsyr !=2, c("issue_year", "intbinsyr", "unempbinsyr", "profitability2")])

temp2 = reshape(temp, v.names = "profitability2", timevar = "issue_year", direction = "wide", idvar = c("intbinsyr", "unempbinsyr"))

temp3 = 1/3 * (as.matrix(temp2[,-c(1,2)]) - 1)

barplot( temp3, beside = TRUE, 
          space = c(.2,1), 
          col = rep(plot_color,each = 2),
          names.arg = unique(sort(temp$issue_year)),
          offset = .0,
          density = rep(c(NA,30), 6) ,
          ylim = c(0,0.05), 
          lwd = 2)

##### Cox proportional hazard rate models

x2$int_rate_norm = x2$int_rate-12
x2$unemp_issue_d_norm = x2$unemp_issue_d - 6

z2 = x2[x2$issue_year>=2011,]
nrow(z2)

coxout = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm+ as.factor(issue_year) +unemp_issue_d_norm, data = z2 )
summary(coxout)

coxout2 = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm + as.factor(issue_year) +unemp_issue_d_norm*as.factor(issue_year), data = z2 )
summary(coxout2)

coxout3 = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm*as.factor(issue_year) +unemp_issue_d_norm*as.factor(issue_year), data = z2 )
summary(coxout3)

coxout3loss = coxph(Surv(z2$myage,z2$isdefault_loss) ~ int_rate_norm*as.factor(issue_year) +unemp_issue_d_norm*as.factor(issue_year), data = z2 )
summary(coxout3loss)

coxout3_18 = coxph(Surv(z2$myage,z2$isdefault18) ~ int_rate_norm*as.factor(issue_year) +unemp_issue_d_norm*as.factor(issue_year), data = z2 )
summary(coxout3_18)

coxout4 = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm*as.factor(issue_year) +unemp_issue_d_norm, data = z2 )
summary(coxout4)

coxout5 = coxph(Surv(z2$myage,z2$isdefault) ~ int_rate_norm+ unemp_issue_d_norm*as.factor(intbinsyr)*as.factor(issue_year), data = z2 )
summary(coxout5)

coxout6 = coxph(Surv(z2$myage,z2$isdefault) ~ unemp_issue_d_norm*as.factor(intbinsyr)*as.factor(issue_year), data = z2 )
summary(coxout5)

# pull out required coeffs from the output of the Cox model and calculate the effects as total effects

total_effect_coeffs = function(coxoutput, coeffstring){
  # inputs
  # coxoutput   cox object
  #coeffstring  string of variable name of interest
  
  labelnames = names(coxoutput$coefficients)
  tosearch = paste0(coeffstring, "*")
  
  iscoeff = grepl(tosearch, labelnames )
  tempnames = labelnames[iscoeff]
  tempcoeffs = coxout3$coefficients[iscoeff]
  
  temporder = order(tempnames)
  tempnames = tempnames[temporder]
  tempcoeffs = tempcoeffs[temporder]
  
  isyearcoeffs = grepl("201*", tempnames )
  firstyear = tempcoeffs[!isyearcoeffs]
  yearcoeffs = firstyear + tempcoeffs[isyearcoeffs]
  allcoeffs = c(firstyear,yearcoeffs)
  return(allcoeffs)
}


coeff_plot = function(xvalues,coeffvalues, ytitle){
  plot(xvalues, coeffvalues, lwd = 2, cex = 2, xlab = "Year of Loan Issue", ylab =ytitle)
  lines(xvalues, coeffvalues, lwd = 2)
  abline(0,0, col = "lightgrey")
}

coeffs2plot = total_effect_coeffs(coxout3, "unemp_issue_d")
coeff_plot(2011:2015,coeffs2plot, "Unemployment Rate Coefficient")

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
