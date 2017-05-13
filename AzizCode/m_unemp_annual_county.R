library(data.table)
library(readxl)

# file.url = "https://www.bls.gov/lau/laucnty14.xlsx"
# file.name = "laucnty14.xlsx"
# 
# txtfile.url = "https://www.bls.gov/lau/laucnty14.txt"
# 
# download.file(url = file.url, destfile = file.name, method = "curl")
# toss = fread(txtfile.url, sep = "\t", skip = 5, header = FALSE)
# t = toss[1]
# t
# 
# toss2 = readLines(file.input)
# toss3 = read.fwf(file.input, skip = 5, widths = c())

# get the file somehow to the \data directory

read_unemp_county = function(temp.file){
  temp.df = read_excel(temp.file, skip = 4)
  temp.df = temp.df[,-c(1,6)]
  
  names(temp.df) = c("state_code", "fips_code", "county_name_raw", "year", "nlabor", "nemployed", "nunemp", "unemprate_county")
  
  # drop empty rows -- most likely a data read issue
  todrop = with(temp.df, is.na(nlabor) & is.na(unemprate_county))
  temp.df = temp.df[!todrop,]
  
  # pull out the state abbreviation
  # find location of comma separating name from state
  temp = regexpr( ", ", temp.df$county_name_raw)
  
  temp.df$state_abbrev = substr(temp.df$county_name_raw, temp+2, temp+3)
  temp.df$county_name = substr(temp.df$county_name_raw, 1, temp-1)
  
  head(temp.df)
  
  return(temp.df)
}


##### begin looping thru the files 
unemp_annual_county = data.frame()

for(j in 10:16){
temp.file = paste0("laucnty",j,".xlsx")
unemp_annual_county = rbind.data.frame(unemp_annual_county, read_unemp_county(temp.file))
}

# calculate unemployment rate relative to state-level rate

unemp_annual_county = ddply(unemp_annual_county, .(year, state_abbrev), 
                            mutate, 
                            unemprate_state = 100*sum(nunemp)/sum(nlabor),
                            county_factor = unemprate_county/unemprate_state,
                            county_ratio = max(unemprate_county)/min(unemprate_county))

# attach zip3 to data

temp = merge(unemp_annual_county, zip3_county, 
                            by.x = c("state_abbrev", "fips_code"), 
                            by.y = c( "zipstate" , "modalfips"), 
                            all.x = TRUE)


unemp_annual_county = temp

save(unemp_annual_county, file = "unemp_annual_county.RData")

# look at variation in ratios within a state-year
t3 = unique(unemp_annual_county[,c("year", "state_abbrev", "county_ratio")])

