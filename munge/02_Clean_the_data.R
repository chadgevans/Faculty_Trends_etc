##########################################################################
# NCES Tables 
##########################################################################
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

# IPEDS
#Percentage of degree-granting postsecondary institutions with a tenure system and of full-time faculty with tenure at these institutions, by control and level of institution and selected characteristics of faculty: Selected years, 1993-94 through 2015-16
# https://nces.ed.gov/programs/digest/d14/tables/dt14_316.80.asp
table<-read_csv(file.path(Data, "tabn316.80.csv"), skip=6, na=c("---", "","‡", "†"), n_max = 25)
names(table)<-c("YEAR","ALLINST","PUBTOTAL","PUB4YRTOTAL","PUB4YRDOC","PUB4YRMA","PUB4YROTH","PUB2YR","NONPROFTOTAL","NONPROF4YRTOTAL","NONPROF4YRDOC","NONPROF4YRMA","NONPROF4YROTH","NONPROF2YR","FORPROF")
table$YEAR<-as.numeric(gsub("([0-9]+).*$", "\\1", table$YEAR))
table<-table[rowSums(is.na(table))!=15,]
table$UNIT<-as.factor(c(rep("INST",9), rep("FACULTY",9)))
Tenure_Sector_table<-table

##########################################################################
# Delta Project Data
#https://nces.ed.gov/ipeds/deltacostproject/
##########################################################################
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

# Delta Project Instructional Faculty Contracts
table<-df1 %>% 
  select(FT_tenure_T1, FT_tenure_T2, FT_tenure_T3, FT_tenure_T4, PT_tenure_T1, PT_tenure_T2, PT_tenure_T3, PT_tenure_T4) %>%
  summarise_each(funs(sum(., na.rm=T))) %>% 
  as.matrix() # Only 2013 available, thus YEAR is unnecessary
dim(table)<-c(4,2)
table<-table %>%
  as.data.frame() %>%
  mutate(CLASS=c("TTT", "NTT_MY","NTT_A","NTT_NONFAC")) %>%
  `colnames<-`(c("Fulltime", "Parttime", "CLASS"))
Contract_table<-table
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
data$CONTROL<-as.factor(data$CONTROL)
levels(data$CONTROL)<-c(NA,NA,NA,"Public","Private non-profit","Private for-profit", NA,NA,NA,NA,NA,"Private for-profit","Private for-profit","Private non-profit","Private non-profit",NA,"Private for-profit","Private non-profit","Private non-profit",NA,"Public","Public","Public","Private non-profit") # Last two NA's were because of "only private" 
levels(data$DEGREE)<-c(NA, "Degree","NonDegree",NA,"NonDegree","NonDegree","NonDegree","Degree","Degree","Degree","Degree","Degree","NonDegree","Degree","Degree","Degree","Degree","Degree",NA,"Degree","Degree","Degree","Degree","NonDegree","Degree",NA,NA,"Degree","Degree",NA,"Degree", "NonDegree","NonDegree","NonDegree",NA)
levels(data$CARNEGIE)<-c(NA,rep("ASSOCS",14),rep("BAS",3),"RESDOC",rep("MASTERS",3),"SPECIAL", NA, rep("SPECIAL",3),"RESDOC","RESDOC",rep("SPECIAL",5),"TRIBAL",NA,rep("SPECIAL",9),NA,"ASSOCS",rep("BAS",3),rep("RESDOC",2),rep("MASTERS",2),rep("SPECIAL",5),"TRIBAL","ASSOCS",rep("BAS",2), rep("RESDOC",2), rep("MASTERS",2), "SPECIAL",NA,"SPECIAL","RESDOC","RESDOC", "SPECIAL",rep("ASSOCS",9),rep("BAS",4),rep("RESDOC",3),rep("MASTERS",3),rep("SPECIAL",13),NA,"ASSOCS","BAS",rep("RESDOC",2),rep("MASTERS",2),"SPECIAL","TRIBAL",NA,"ASSOCS",rep("BAS",2), rep("RESDOC",2), rep("MASTERS",2),rep("RESDOC",2), "ASSOCS","BAS",rep("MASTERS",2),rep("SPECIAL",2)) 
# Level
table <- data %>% 
  select(YEAR,LEVEL) %>%
  group_by(YEAR, LEVEL) %>%
  summarise(COUNT=n()) %>%
  na.omit() %>% # Data was poorly collected from less2 schools at this time
  spread(LEVEL, COUNT) %>%
  na.omit() %>%
  `colnames<-`(c("YEAR", "TWOFOUR","FOUR","LESS2"))
table[1:3,"LESS2"]<-NA # Data was poorly collected from less2 schools at this time
Inst_Level_table<-table
# Control
table<-data %>% 
  select(YEAR,CONTROL) %>%
  group_by(YEAR, CONTROL) %>%
  summarise(COUNT=n()) %>%
  na.omit() %>% # Data was poorly collected from less2 schools at this time
  spread(CONTROL, COUNT) %>%
  `colnames<-`(c("YEAR","PUBLIC","PRIVNPROF","PRIV4PROF"))
table$PRIVNPROF[table$YEAR<1987]<-NA # Problems with private only classifiacation in the 1980s.
table$PRIV4PROF[table$YEAR<1987]<-NA # Problems with private only classifiacation in the 1980s
Inst_Control_table<-table
# Degree-granting status
table<-data %>% 
  select(YEAR,DEGREE) %>%
  group_by(YEAR, DEGREE) %>%
  summarise(COUNT=n()) %>%
  na.omit() %>% # Data was poorly collected from less2 schools at this time
  spread(DEGREE, COUNT) %>%
  `colnames<-`(c("YEAR","DEGREE","NONDEGREE"))
table$DEGREE[table$YEAR==1997]<-NA
table$NONDEGREE[table$YEAR==1997]<-NA
Inst_Degree_table<-table
# Carnegie
table<-data %>% 
  select(YEAR,CARNEGIE) %>%
  group_by(YEAR, CARNEGIE) %>%
  summarise(COUNT=n()) %>%
  na.omit() %>% # Data was poorly collected from less2 schools at this time
  spread(CARNEGIE, COUNT)
table<-table[!table$YEAR==2015,] # Strange reclassification of data.
Inst_Carnegie_table<-table
# Level x Control Crosstabulation
table<-data %>% 
  select(LEVEL,CONTROL) %>%
  na.omit()
Delta_Crosstable<-table



##########################################################################
# IPEDS Complete Data (Employees by Assigned Position (EAP)) 
##########################################################################
setwd("/Users/chadgevans/Research/Projects/Data/Faculty_Trends_etc_data/Complete_Data_Files/EAP_Data")
file_list <- list.files()
datalist<-lapply(file_list, function(x){read.table(x, header=TRUE, sep=",")})
datalist <- lapply(datalist,function(x) {colnames(x) <- toupper(colnames(x));x})
years <- list(2001:2015)
data<-do.call("rbind.fill", mapply(cbind, datalist, YEAR = list(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015), SIMPLIFY = FALSE))
data0211<-data[data$YEAR %in% c(2002:2011),c("YEAR","UNITID","EAPTOT","EAPRECTP")]
data1215<-data[data$YEAR %in% c(2012:2015),c("YEAR","UNITID","EAPFT","EAPPT","EAPCAT")] # Splitting these two intervals is necessary b/c they're structured differently

table<-data0211 %>%
  filter(EAPRECTP %in% c(2102, 2103, 2104, 3102, 3103, 3104)) %>%
  `colnames<-`(c("YEAR","UNITID","EAPTOT","FACULTY"))
table$FACULTY[table$FACULTY==2102]<-"FTTEN"
table$FACULTY[table$FACULTY==2103]<-"FTTRACK"
table$FACULTY[table$FACULTY==2104]<-"FTNTT"
table$FACULTY[table$FACULTY==3102]<-"PTTEN"
table$FACULTY[table$FACULTY==3103]<-"PTTRACK"
table$FACULTY[table$FACULTY==3104]<-"PTNTT"
table <- table %>%
  spread(FACULTY, EAPTOT) %>%
  rowwise() %>% 
  mutate(PT=sum(PTNTT,PTTEN,PTTRACK, na.rm=T)) %>%
  select(YEAR,UNITID,FTNTT,FTTEN,FTTRACK,PT) 
Tenure_table0211<-table

table<-data1215 %>%
  select(YEAR,UNITID,EAPFT,EAPPT,EAPCAT) %>%
  gather(CLASS, VALUE, 5) %>%
  filter(VALUE %in% c(10020,10030,10040))
table$FACULTY[table$VALUE==10020]<-"TENURE" # With faculty status, tenured; "10020" means EAP number or FACSTAT number 20
table$FACULTY[table$VALUE==10030]<-"TRACK" # With faculty status, on tenure track
table$FACULTY[table$VALUE==10040]<-"NONTENURE" # With faculty status not on tenure track/No tenure system, total
table<-table %>%
  select(YEAR,UNITID,EAPFT,EAPPT, FACULTY) %>%
  gather(STATUS, VALUE, -YEAR, -UNITID, -FACULTY) %>%
  unite(FACULTY_STATUS,FACULTY,STATUS) %>%
  spread(FACULTY_STATUS, VALUE) %>%
  rowwise() %>% 
  mutate(PT=sum(NONTENURE_EAPPT,TENURE_EAPPT,TRACK_EAPPT,na.rm=T)) %>%
  select(YEAR,UNITID,NONTENURE_EAPFT,TENURE_EAPFT,TRACK_EAPFT,PT) %>%
  `colnames<-`(c("YEAR","UNITID","FTNTT","FTTEN","FTTRACK","PT"))
Tenure_table1215<-table

Ind_Tenure_data<-rbind(Tenure_table0211,Tenure_table1215)
table<- Ind_Tenure_data %>%
  gather(STATUS, VALUE, -YEAR, -UNITID) %>%
  group_by(YEAR, STATUS) %>%
  summarise(SUM=sum(VALUE, na.rm = T)) %>%
  spread(STATUS,SUM)
table[-1]<-prop.table(as.matrix(table[2:5]),1)
table[-1]<-100*table[-1]
Tenure_Status_table<-table

# Now we merge with institutional level characteristics (as of 2014)
Inst_data<-read_csv(file.path(Data, "CSV_262017-529.csv"))
Inst_data<- Inst_data %>%
  `names<-`(c('UNITID','INST','YEAR','SECTOR','LEVEL','CONTROL','DEGREE',
              'TITLEIV','INSTCAT','CARNEGIE','CARNENROLL','CARNSIZE','OCC',
              'ACADEMIC','CONTPROF','REC','REMEDIAL','HIGH','UNDERGRADS','GRADS','SFRATIO')) %>%
  select(UNITID,LEVEL,CONTROL,DEGREE,CARNEGIE)
Inst_data<-Inst_data[rowSums(is.na(Inst_data))!=4,]
mdata<-inner_join(Inst_data,Ind_Tenure_data, by='UNITID') # keep all the rows of the Ind data and match the Inst-level characterstis with the ind data.
# Level
table<-mdata %>% 
  group_by(LEVEL,YEAR) %>% 
  summarize(NONTENURE=sum(FTNTT,PT, na.rm=T),TENURE=sum(FTTEN,FTTRACK, na.rm=T)) %>%
  mutate(PCTNTT=(NONTENURE/(NONTENURE+TENURE)))
Level_Ten_table<-table
# Control
table<-mdata %>% 
  group_by(CONTROL,YEAR) %>% 
  summarize(NONTENURE=sum(FTNTT,PT, na.rm=T),TENURE=sum(FTTEN,FTTRACK, na.rm=T)) %>%
  mutate(PCTNTT=(NONTENURE/(NONTENURE+TENURE)))
Inst_Control_Ten_table<-table
# Degree-granting x tenure status table
table<-mdata %>% 
  group_by(DEGREE,YEAR) %>% 
  summarize(NONTENURE=sum(FTNTT,PT, na.rm=T),TENURE=sum(FTTEN,FTTRACK, na.rm=T)) %>%
  mutate(PCTNTT=(NONTENURE/(NONTENURE+TENURE)))
Degree_Inst_Ten_table<-table

