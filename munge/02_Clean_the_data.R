# NCES Tables
# https://nces.ed.gov/programs/digest/d16/tables/dt16_303.10.asp?current=yes
EnrollTab<-read_csv(file.path(Data, "tabn303.10.csv"), skip = 5, na = c("---",""))
EnrollTab<-EnrollTab[rowSums(is.na(EnrollTab))!=14,-c(5,13)]
names(EnrollTab) <- c("Year","Enrollment","Fulltime","Parttime","PctParttime","Male","Female","PctFemale","Public","TotalPri","Nonprofit","ForProfit")
EnrollTab$Year<-as.numeric(gsub("([0-9]+).*$", "\\1", EnrollTab$Year))
EnrollTab <- EnrollTab[12:77,] %>% 
  mutate_if(is.character, as.double) %>%
  mutate_each(funs(./1000000), -c(Year,PctParttime,PctFemale))
EnrollTab$Projection<-rep(NA, nrow(EnrollTab))
EnrollTab$Projection[EnrollTab$Year<2016]<-"No"
EnrollTab$Projection[EnrollTab$Year>=2016]<-"Yes"

# IPEDS tuition table
# https://nces.ed.gov/programs/digest/d13/tables/dt13_330.10.asp
ttable<-read_csv(file.path(Data, "tabn330.10.csv"), skip=5, na = c("---",""))
ttable<-ttable[,c(1,3,4)]
names(ttable)<-c("YEAR","FYEAR","TYEAR")
ttable$YEAR<-as.numeric(gsub("([0-9]+).*$", "\\1", ttable$YEAR))
ttable<-sapply(ttable, function(x){as.numeric(gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x))})
ttable<-ttable[rowSums(is.na(ttable))!=3,] %>% as_data_frame()
ttable$CONTROL<-c(rep("All", 53), rep("Public",53), rep("Private", 53), rep("NonProf", 17), rep("ForProf", 17))
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

# Expenses by level
https://nces.ed.gov/programs/digest/d16/tables/dt16_334.30.asp

# IPEDS Expenses by Category 2014
# https://nces.ed.gov/ipeds/trendgenerator
publicex<-read_csv(file.path(Data, "Report_Public.csv"), skip=1, n_max = 11)
colnames(publicex)<-c("EXPENSE","PUBLIC")
privateex<-read_csv(file.path(Data, "Report_Private.csv"), skip=1, n_max=11)
colnames(privateex)<-c("EXPENSE","PRIVATE")
expensedata<-full_join(publicex,privateex, by = "EXPENSE")
expensedata$PUBLIC[12]<-expensedata$PUBLIC[7]
expensedata<-expensedata[-7,]
expensedata<- expensedata %>% 
  mutate(ALL=PUBLIC+PRIVATE) %>%
  mutate_each(funs(.*1000/1000000000), -EXPENSE)  # adjusted for 1000s, converted to billions

##########################################################################
# IPEDS Custom Data (Institutional Data) 
##########################################################################
setwd("/Users/chadgevans/Research/Projects/Data/Faculty_Trends_etc_data/Custom_Data_Files/Institutional_Data/Data")
file_list <- list.files()
datalist<-lapply(file_list, function(x){read.table(x, header=TRUE, sep=",")})
datalist <- lapply(datalist,function(x) {colnames(x) <- toupper(colnames(x));x})

names(datalist[[1]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[2]])<-c("UNITID","INSTNAME","YEAR","CONTROL","LEVEL")
names(datalist[[3]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[4]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[5]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[6]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[7]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[8]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","CARNEGIE")
names(datalist[[9]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[10]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[11]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[12]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","CARNEGIE")
names(datalist[[13]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[14]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[15]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[16]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[17]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[18]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[19]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[20]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[21]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","CARNEGIE","DEGREE")
names(datalist[[22]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[23]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[24]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[25]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[26]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[27]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[28]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[29]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL")
names(datalist[[30]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[31]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
names(datalist[[32]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","CARNEGIE")
names(datalist[[33]])<-c("UNITID","INSTNAME","YEAR","LEVEL","CONTROL","DEGREE","CARNEGIE")
data<-do.call("rbind.fill", mapply(cbind, datalist, YEAR = list(2012,1986,1984,2008,2000,2014,1988,1995,2011,2015,1980,2007,2003,1991,1985,1987,2001,2004,1992,2006,1996,1998,2013,1993,1997,2009,1989,2010,1990,2002,2005,1994,1999), SIMPLIFY = FALSE))
levels(data$LEVEL)<-c(NA,"2to4","4+","Less2","4+","2to4","Less2","2to4",NA,NA,"4+","Less2","2to4",NA, NA, "2to4","2to4","4+","4+","2to4","4+",NA,"2to4",NA,NA) # STRRONG ASSuMPTION: coded less than BA to 2to4

# Level
Inst_Level_table <- data %>% 
  select(YEAR,LEVEL) %>%
  group_by(YEAR, LEVEL) %>%
  summarise(COUNT=n()) %>%
  na.omit() %>% # Data was poorly collected from less2 schools at this time
  spread(LEVEL, COUNT) %>%
  na.omit() %>%
  `colnames<-`(c("YEAR", "TWOFOUR","FOUR","LESS2"))
  

