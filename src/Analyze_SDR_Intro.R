packages=c("RColorBrewer", "data.table", "dplyr")
lapply(packages, require, character.only = TRUE)

# Directories and Cleaned Data
# Products Directory
Product_Directory<-"/Users/chadgevans/Research/Dissertation/Projects/Products"
Output_Directory<-"/Users/chadgevans/Research/Dissertation/Projects/Build_Dataset/SDR/Output"

load(file.path(Output_Directory, "Coded_SDR_data.RData"))
############################### Analysis ########################################

attach(df)

# How have the proportion of different faculty statuses changed over time?
table<-as.data.frame.matrix(prop.table(table(YEAR, TENSTA2),1))
table<-setDT(table, keep.rownames = TRUE)[]; names(table)[1]<-"YEAR"
par(mar=c(9,5,2,2))
plot(table$YEAR,table$T, ylab="Faculty proportion",xlab="Year",main="Proportion of Faculty in Postsecondary Education",type="l", col="darkblue", ylim=c(0,1))
points(table$YEAR,table$NTT, col="red", type="l")
points(table$YEAR,table$TT, col="cornflowerblue", type="l")
legend(1993,1,c('Tenured Faculty','Tenure-Track Faculty','Non-Tenure Track Faculty'),lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c('darkblue','cornflowerblue','red'), cex=1)
mtext("Evans & Furstenberg",side=1,line=3,adj=0,cex=.75,col="black")
mtext("Source: Survey of Doctorate Recipients, 1993-2013",side=1,line=4,adj=0,cex=.75,col="black")
dev.copy(png,file.path(Product_Directory,"SDR_Faculty_Percentages_Over_Time.png"))
rm(table)

# Replicating the "Percentage of Faculty Types in Postsecondary Education"
table<-as.data.frame.matrix(100*prop.table(table(YEAR, TENFTSTAT),1)[-1,]) # rm row 1 without values in 1993
table<-setDT(table, keep.rownames = TRUE)[]
colnames(table)[1] <- "YEAR"
mypalatte<-brewer.pal(4, "Set2")
par(mar=c(9,5,4,2), bg="oldlace")
plot(table$YEAR,table$FTTEN, ylab="Percent of Faculty",xlab="Year",main="Percentage of Faculty Types in Postsecondary Education",type="l", lwd=2, col=mypalatte[1], ylim=c(0,65))
points(table$YEAR,table$FTTT, col=mypalatte[2], type="l",lwd=2)
points(table$YEAR,table$FTNTT, col=mypalatte[3], type="l",lwd=2)
points(table$YEAR,table$PT, col=mypalatte[4], type="l",lwd=2)
legend(1995,65,c('Full-time Tenured','Full-time Tenure-Track','Full-time Non-Tenure/Track','Part-time Faculty'),lty=rep(1,4), lwd=rep(2.5,4),col=mypalatte, cex=.9, bg="white")
mtext("Evans & Furstenberg",side=1,line=4,adj=0,cex=.7,col="black")
mtext("Source: National Science Foundation, Survey of Doctorate Recipients, 1995-2013",side=1,line=5,adj=0,cex=.7,col="black")
dev.copy(png,file.path(Product_Directory,"SDR_Tenure_Pct_over_Time.png"))
dev.off()
rm(table)

# Growth of those who have careers outside of academia
# SDR does not have information on additional work.  Only principal employer

### Growth of post-docs
PDIXdf<-df[df$POSTSECONDARY=="Yes",]
table<-as.data.frame.matrix(100*prop.table(table(PDIXdf$YEAR, PDIXdf$PDIX),1)[-2,])
table<-setDT(table, keep.rownames = TRUE)[]
colnames(table) <- c("YEAR","NonPostDoc","PostDoc")
mypalatte<-brewer.pal(4, "Set1")
par(mar=c(9,5,4,2), bg="oldlace")
plot(table$YEAR,table$PostDoc, ylab="Percent of PhDs in Higher Education",xlab="Year",main="Percentage of PHDs in Postdocs in Higher Education",type="l", lwd=2, col=mypalatte[1], ylim=c(0,20))
legend(1993,20,c('Postdoc Position'),lty=rep(1,1), lwd=rep(2.5,1),col=mypalatte, cex=1, bg="white")
mtext("Evans & Furstenberg",side=1,line=4,adj=0,cex=.7,col="black")
mtext("Source: National Science Foundation, Survey of Doctorate Recipients, 1993-2013",side=1,line=5,adj=0,cex=.7,col="black")
mtext("Note: This percentage is the portion of PHD recipients working in postsecondary institutions who \n define their job as a temporary position for gaining additional education and training in research.",side=1,line=6.5,adj=0,cex=.7,col="black")
dev.copy(png,file.path(Product_Directory,"SDR_PostDoc_over_Time.png"))
dev.off()
rm(table)

### Growth of retirement-age folks
# Originally, I tried to include "previously retired PJRET" but that variable only available since 2006.
SENIORdf<-df[df$POSTSECONDARY=="Yes",]
table<-as.data.frame.matrix(100*prop.table(table(SENIORdf$YEAR, SENIORdf$SENFTSTAT),1))
table<-table[-1,]
table<-setDT(table, keep.rownames = TRUE)[]
colnames(table)[1] <- "YEAR"
mypalatte<-brewer.pal(4, "Set2")
par(mar=c(9,5,4,2), bg="oldlace")
plot(table$YEAR,table$FTSEN_TT, ylab="Percent of PhDs in Higher Education",xlab="Year",main="Tenure Status of Retirement-Age Faculty (with PhDs)",type="l", lwd=2, col=mypalatte[1], ylim=c(0,10))
points(table$YEAR,table$FTSEN_NTT, col=mypalatte[2], type="l",lwd=2)
points(table$YEAR,table$PTSENIOR, col=mypalatte[3], type="l",lwd=2)
legend(1995,10,c('Full-time Tenured Senior (65+) Faculty','Full-time Non-Tenure/Track Senior (65+) Faculty','Part-time Senior (65+) Faculty'),lty=rep(1,1), lwd=rep(2.5,1),col=mypalatte, cex=.9, bg="white")
mtext("Evans & Furstenberg",side=1,line=4,adj=0,cex=.7,col="black")
mtext("Source: National Science Foundation, Survey of Doctorate Recipients, 1995-2013",side=1,line=5,adj=0,cex=.7,col="black")
mtext("Note: These percentages are the portion of PHD recipients working in postsecondary institutions who \n are retirement age.  There were no senior 'tenure-track' faculty in the sample.",side=1,line=6.5,adj=0,cex=.7,col="black")
dev.copy(png,file.path(Product_Directory,"SDR_Seniors_over_Time.png"))
dev.off()
rm(table)

### Why part-time employment?
PTreasonvars<-c("PJRET","PJSTU","PJFAM","PJOCNA","PJHAJ","PJNOND","PJOT")
ITINdf<-df[df$POSTSECONDARY=="Yes" & df$YEAR==2013,]
a<-ITINdf[, names(ITINdf) %in% PTreasonvars]
b<-sapply(a, function(x){y<-table(x); prop.table(y)})
mypalatte<-brewer.pal(7, "Set2")
par(mar=c(9,5,4,2), bg="oldlace")
barplot3<-barplot(100*b[2,], ylab="Percent", main="Reasons PhDs Worked Part-time in 2013", col=mypalatte, axes=FALSE, axisnames=FALSE)
labels<-c("Semi-retired","Student","Family","Full-time Job \n Unavailable","Multiple Jobs","No Interest \n in Full-time","Other")
text(barplot3, par('usr')[3], labels = labels, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=1)
axis(2)
mtext("Evans & Furstenberg",side=1,line=5,adj=0,cex=.7,col="black")
mtext("Source: National Science Foundation, Survey of Doctorate Recipients, 2006-2013.",side=1,line=6,adj=0,cex=.7,col="black")
dev.copy(png,file.path(Product_Directory,"SDR_PT_Reasons.png"))
dev.off()

### Growth of PT reasons since 2006
PTreasonvars<-c("YEAR","PJRET","PJSTU","PJFAM","PJOCNA","PJHAJ","PJNOND","PJOT")
ITINdf<-df[df$POSTSECONDARY=="Yes",]
a<-ITINdf[, names(ITINdf) %in% PTreasonvars]
t1<-a %>% group_by(YEAR, PJRET) %>% na.omit() %>% summarise (n = n()) %>% mutate(fPJRET = n / sum(n)); t1<-t1[t1[2]=="Y: Yes", c(1,4)]
t2<-a %>% group_by(YEAR, PJSTU) %>% na.omit() %>% summarise (n = n()) %>% mutate(fPJSTU = n / sum(n)); t2<-t2[t2[2]=="Y: Yes", c(1,4)]
t3<-a %>% group_by(YEAR, PJFAM) %>% na.omit() %>% summarise (n = n()) %>% mutate(fPJFAM = n / sum(n)); t3<-t3[t3[2]=="Y: Yes", c(1,4)]
t4<-a %>% group_by(YEAR, PJOCNA) %>% na.omit() %>% summarise (n = n()) %>% mutate(fPJOCNA = n / sum(n)); t4<-t4[t4[2]=="Y: Yes", c(1,4)]
t5<-a %>% group_by(YEAR, PJHAJ) %>% na.omit() %>% summarise (n = n()) %>% mutate(fPJHAJ = n / sum(n)); t5<-t5[t5[2]=="Y: Yes", c(1,4)]
t6<-a %>% group_by(YEAR, PJNOND) %>% na.omit() %>% summarise (n = n()) %>% mutate(fPJNOND = n / sum(n)); t6<-t6[t6[2]=="Y: Yes", c(1,4)]
t7<-a %>% group_by(YEAR, PJOT) %>% na.omit() %>% summarise (n = n()) %>% mutate(fPJOT = n / sum(n)); t7<-t7[t7[2]=="Y: Yes", c(1,4)]
table<-list(t1,t2,t3,t4,t5,t6,t7) %>% Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="YEAR"), .)
table[-1]<-100*table[-1]

mypalatte<-brewer.pal(7, "Set2")
par(mar=c(9,5,4,2), bg="oldlace")
plot(table$YEAR,table$fPJRET, ylab="Percent of Part-time PhDs",xlab="Year",main="Reasons Why Part-time Faculty (with PhDs) Hold Part-time Work",type="l", lwd=2, col=mypalatte[1], ylim=c(0,75))
points(table$YEAR,table$fPJSTU, col=mypalatte[2], type="l",lwd=2)
points(table$YEAR,table$fPJFAM, col=mypalatte[3], type="l",lwd=2)
points(table$YEAR,table$fPJOCNA, col=mypalatte[4], type="l",lwd=2)
points(table$YEAR,table$fPJHAJ, col=mypalatte[5], type="l",lwd=2)
points(table$YEAR,table$fPJNOND, col=mypalatte[6], type="l",lwd=2)
points(table$YEAR,table$fPJOT, col=mypalatte[7], type="l",lwd=2)
legend(2006,75,c('Semi-retired','Student','Family','Full-time job unavailable','Multiple jobs','No interest in Full-time','Other'),lty=rep(1,7), lwd=rep(2.5,7),col=mypalatte, cex=1, bg="white")
mtext("Evans & Furstenberg",side=1,line=4,adj=0,cex=.7,col="black")
mtext("Source: National Science Foundation, Survey of Doctorate Recipients, 2006-2013",side=1,line=5,adj=0,cex=.7,col="black")
dev.copy(png,file.path(Product_Directory,"SDR_PT_reasons_over_Time.png"))
dev.off()
rm(table)



### Other Demographic features
### Employment conditions
### Number of courses taught and instructional support

###################################### Benefits ############################################
i<-JOBINS
prop.table(table(i, TENSTA2))



Y<-"TENSTA"
Z<-c("JOBINS","JOBVAC","JOBPENS")
for(i in Z){

  c<-data.frame(YEAR, i, TENSTA2)
  
  c<-ftable(data.frame(YEAR, i, TENSTA2))

dt <- data.frame(age=rchisq(20,10),group=sample(1:2,20,rep=T))
ddply(c,~YEAR,summarise,TENSTA2=count(TENSTA2),
      
      foo<-ddply(df, .(x1,x2), function(i) prop.table(table(i$y1)))
      
prop.table(table(YEAR, i, TENSTA2), margin = 2) 
  
  table<-data.frame(do.call(rbind,lapply(SDR_datasets, function(x){if(i %in% colnames(x)){
    results<-with(x, prop.table(table(x[,i], TENSTA2)[2:3,1:3],2))
  }
  })))

}
  table<-table[c(2,4,6),]
  rownames(table)<-c(2010,2013,1997)
  table<-t(table[order(rownames(table1), decreasing = FALSE),])
  barplot(table, main=substitute(paste('Percent of Faculty with ', a), list(a=i)),
          xlab="Year", ylab="Percent",col=c("red","darkblue","cornflowerblue"),
          legend = rownames(table1), beside=TRUE,args.legend = list(x = "bottomright"))
}
rm(table)


### Growth of itinerant faculty
table<-as.data.frame.matrix(na.omit(100*prop.table(table(ITINdf$YEAR,ITINdf$PJHAJ),1))) # no data from before 2006
table<-setDT(table, keep.rownames = TRUE)[]
colnames(table) <- c("YEAR","NONITINERANT","ITINERANT")
par(mar=c(9,5,4,2), bg="oldlace")
plot(table$YEAR,table$ITINERANT, ylab="Percent of PhDs in Higher Education",xlab="Year",main="Percent of Part-time Faculty (with PhDs) Holding Itinerant Work",type="l", lwd=2, col=mypalatte[1], ylim=c(0,25))
legend(2006,25,c('Itinerant Faculty'),lty=rep(1,1), lwd=rep(2.5,1),col=mypalatte, cex=1, bg="white")
mtext("Evans & Furstenberg",side=1,line=4,adj=0,cex=.7,col="black")
mtext("Source: National Science Foundation, Survey of Doctorate Recipients, 2006-2013",side=1,line=5,adj=0,cex=.7,col="black")
mtext("Note: This is the portion of part-time faculty, with PhDs, who work part-time because they hold multiple jobs. \n Some of these faculty hold another job in academia and some in the private sector.",side=1,line=6.5,adj=0,cex=.7,col="black")
dev.copy(png,file.path(Product_Directory,"SDR_Itinerant_over_Time.png"))
dev.off()
rm(table)

