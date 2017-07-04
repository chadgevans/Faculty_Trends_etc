#rm(list=ls())

# Directories and Libraries
Build_Directory<-"/Users/chadgevans/Research/PHD/Data_Analysis/Build/Code/"
Analysis_Directory<-"/Users/chadgevans/Research/PHD/Data_Analysis/Analysis/Code/"
Data_Directory<-"/Users/chadgevans/Research/PHD/Data_Analysis/Build/Output/"
source(file.path(Build_Directory, "Libraries.R")) # libraries

# Data
load(file.path(Data_Directory, "Cleaned_HERI.RData")); attach(Cleaned_HERI)
source(file.path(Analysis_Directory, "HERI_Coded.R")) # Code the data

options(digits=3)
options(scipen=999)

df<-df[df$TENURE2=="Non-Tenure",]
attach(df)

tablemaker<-function(ADJUNCT, TYPE){ 
  vars<-df[,c("GENDER","AGE2","MARRIED","PARENT","RACE","CITIZEN","DEGEARN5","ENGLISH")]
  table<-NULL
  for (i in vars){t1<-prop.table(table(i, ADJUNCT),TYPE)
  table<-rbind(table, t1)
  }
  print(table)
}

table<-tablemaker(df$FULLSTAT,2)
tablemaker(df$FULLSTAT,1)

Gen_Faculty<-read.csv("/Users/chadgevans/Research/PHD/Data_Analysis/Build/Input/General_Faculty_Characteristics.csv")
Gen_Faculty[,c(1:2)]

tab<-cbind(table,Gen_Faculty[,2]/100)
tab[c(3:4),3]<-tab[c(3:4),3]*100
names(tab)<-c("Part-time NTT","Full-time NTT", "All Faculty")
view(tab)