
state.mapping<-read.table("./Unemployment Data/State Mappings.csv",sep="-",header = F)
state.mapping$code<-as.numeric(substr(state.mapping$V2,start = 16,stop = 18))
state.mapping$url<-as.character(str_trim(state.mapping$V1))
state.mapping$state<-as.character(substr(state.mapping$V2,start = 21,stop = 23))
state.mapping$V2<-NULL
state.mapping$V1<-NULL

## Getting Unemployment Rate from BLS for each state
## Generic Series Id = LASST010000000000003  where ST01 indicates the state id from 1-53 + 72
## and 3 in the end indicates the statistic unemployment rate
unemp.data<-as.data.table(c(series_id=NULL,value=NULL,date=NULL,state=NULL))

for(i in c(1:52))  {
    
    state_url<-paste0("https://download.bls.gov/pub/time.series/la/",state.mapping$url[i])
    print(state_url)
    state_id <- state.mapping$code[i]
    print(state_id)
    state_n<-state.mapping$state[i]
    print(state_n)
    file_name<-paste0("./Unemployment Data/",state_id,".txt")
    
    if(nchar(state_id)==1){
        state_id<-paste0("0",state_id)
    }
    
    series_code=paste0("LASST",state_id,"0000000000003")
    #download.file(state_url,file_name,method="curl")
    #state.unemp.data<-NULL
    rm(state.unemp.data)
    state.unemp.data<-fread(file_name,header = T)
    #state.unemp.rate<-NULL
    rm(state.unemp.rate)
    state.unemp.rate<-state.unemp.data[(year>2006)&(series_id==series_code),]
    state.unemp.rate[,month:=gsub("M","",state.unemp.rate$period)][,footnote_codes:=NULL]
    state.unemp.rate[,date:=dmy(paste0("01-",state.unemp.rate$month,"-",state.unemp.rate$year))]
    state.unemp.rate[,state:=str_trim(state_n)]
    state.unemp.rate$year=NULL
    state.unemp.rate$period=NULL
    state.unemp.rate$month=NULL
    unemp.data<-rbind(unemp.data,state.unemp.rate)
    
}

unemp.data[,state:=str_trim(state)]
write.csv(x = unemp.data,file = "State Unemployment Data Monthly History.csv",row.names = F)




