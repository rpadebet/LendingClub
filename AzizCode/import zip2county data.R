## Load the libraries
library(data.table)
#library(tidyverse)
#library(lubridate)
#library(stringi)
#library(stringr)



# reads in the files that map the ZIP+4 to county and prepares two files
# zip5_county   map between 5-digit zip and FIPS county codes
# zip3_county   map between 3-digit zip and FIPS county codes 
##-- uses most common county within the 3-digit zip as the representative county


setwd("c:/Aziz/Rfiles/data/")

dir()

read_single_file = function(zipfilenames.curr, file.curr){  
  # reads in one text file at a time that has the zip to county mapping 
  # inputs
  # zipfilenames.curr   names of zip folder (e.g., XXX.zip)
  # file.curr           name of file in zipped folder
  
  # output
  # data.frame containing all possible combinations of the 5-digit zip to county mappings
  
  tempfile = unzip(zipfilenames.curr, file.curr )
  
  tempout = fread(tempfile, header = FALSE, skip = 1, sep = "\t")
  
  # drop temporary read files to save on memory
  t = as.vector(tempout$V1)

    rm(tempfile,tempout)
  
  
  out.df1 = data.frame(zip5  = (substr(t, 1, 5)),
                       zipstate= substr(t, 24,25),
                       zipcountyfips = (substr(t, 26,28)),
                       zipcountyname = substr(t, 29,53))
  
  out.df2 = unique(out.df1)
  
  rm(t,out.df1)
  return(out.df2)
}


##### begin looping thru the files. There are 10 files, in two folders 
# source: https://wonder.cdc.gov/wonder/sci_data/codes/fips/type_txt/cntyxref.asp

zipfilenames = c("zipctyA.zip", "zipctyB.zip")
zip5_county = data.frame()

for (j in 1:10){
  k_zipfile = ifelse(j <=5,1,2)
  zipfilenames.curr = zipfilenames[k_zipfile]
  file.curr = paste0("zipcty", j)
  
  zip5_county = rbind.data.frame(zip5_county, 
                                      read_single_file(zipfilenames.curr, file.curr ))
  nrow(zip5_county)
}

save(zip5_county,file =  "zip5_county.RData")

## make a mapping from zip3 to county, using the most frequently occuring county as the representative county

zip5_county$zip3 = substr(zip5_county$zip5,1,3)


most_common = function(xchar){
# function to return the most frequently occuring value of xchar as a character string
    tt = table(xchar)
  maxchar = names(tt)[max(tt)==tt]
  
  # if there is a tie, this will return more than one character. Take first of these
  return(maxchar[1])
}


zip3_county = ddply(zip5_county, .(zip3, zipstate), summarize, 
                    nobs = length(zip3),
                    nmaxcounty = max(table(zipcountyfips)),
                    modalfips = most_common(zipcountyfips),
                    modalcountyname = most_common(zipcountyname))

save(zip3_county,file =  "zip3_county.RData")
