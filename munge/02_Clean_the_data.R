# NCES Enrollment Tables
EnrollTab<-read_csv(file.path(Data, "tabn303.10.csv"), skip = 5, na = c("---",""))
EnrollTab<-EnrollTab[rowSums(is.na(EnrollTab))!=14,-c(5,13)]
names(EnrollTab) <- c("Year","Enrollment","Fulltime","Parttime","PctParttime","Male","Female","PctFemale","Public","TotalPri","Nonprofit","ForProfit")
EnrollTab$Year<-as.numeric(gsub("([0-9]+).*$", "\\1", EnrollTab$Year))
EnrollTab <- EnrollTab[12:75,] %>% 
  mutate_if(is.character, as.double) %>%
  mutate_each(funs(./1000000), -c(Year,PctParttime,PctFemale))
EnrollTab$Projection<-rep(NA, nrow(EnrollTab))
EnrollTab$Projection[EnrollTab$Year<2014]<-"No"
EnrollTab$Projection[EnrollTab$Year>=2014]<-"Yes"

# IPEDS tuition table
# https://nces.ed.gov/programs/digest/d13/tables/dt13_330.10.asp
ttable<-read.csv(file.path(Data, "tabn330.10.csv"),header=T, sep=",", skip=6, colClasses="character", na.strings=c("---"))
publict<-ttable[66:128,c(1,3,4)]
nonproft<-ttable[196:215,c(1,3,4)]
forproft<-ttable[218:237,c(1,3,4)]
ttable<-list(publict,nonproft,forproft)
ttable<-lapply(ttable, function(x){names(x)<-c("YEAR","FYEAR","TYEAR")
    x<-x[!(x$YEAR %in% ""),]}
)
ttable[[1]]$YEAR<-c(1963:2015)
ttable[[2]]$YEAR<-c(1999:2015)
ttable[[3]]$YEAR<-c(1999:2015)
ttable<-ttable %>% 
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="YEAR"), .) %>%
  sapply(function(x){as.numeric(gsub(",", "", x))}) %>%
  data.frame()
names(ttable)<-c("YEAR","PUBFYEAR","PUBTYEAR","PRIVNONFYEAR","PRIVNONTYEAR","PRIV4FYEAR","PRIV4TYEAR")
tuition_table<-ttable
rm(ttable)

# Delta Project Institutional Expenses
keeperCols <- c("academicyear","acadsupp01","auxiliary01","hospital01","independ01","instsupp01","instruction01","pubserv01","research01","opermain01", "grants01", "other01", "studserv01")
df1<-read.csv(file.path(Data, "delta_public_release_00_13.csv"))
df2<-read.csv(file.path(Data, "delta_public_release_87_99.csv"))
etable <- do.call("rbind.fill", list(df1[keeperCols],df2[keeperCols]))
etable[ is.na(etable) ] <- NA
etable<- etable %>% 
  mutate_if(is.character, as.double) %>%
  group_by(academicyear) %>% 
  summarise_each(funs(sum(., na.rm=T))) %>%
  mutate(NetAid_Other=opermain01+grants01+other01) %>%
  select(-c(opermain01,grants01,other01)) %>% 
  mutate_each(funs(./1000000000), -academicyear)
Delta_table<-etable
rm(etable)


