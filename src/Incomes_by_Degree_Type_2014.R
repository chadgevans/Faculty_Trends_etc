load("/Users/chadgevans/Research/PHD/Data_Analysis/Build/Output/Cleaned_data.RData")
data=Cleaned_data

data$EDUC <- factor(rep(NA, nrow(data)), levels=c("Less than a BA", "BA","Masters/Professional","PhD"))   
data$EDUC[data$SCHL %in% c(01:20)] <- "Less than a BA"
data$EDUC[data$SCHL==21] <- "BA"
data$EDUC[data$SCHL %in% c(22,23)] <- "Masters/Professional"
data$EDUC[data$SCHL==24] <- "PhD"

data$EMPLOYED=NA
data$EMPLOYED[data$ESR %in% c(1,2,4,5)]<-"Employed"
data$EMPLOYED[data$ESR==3]<-"Unemployed"

#random sample  for plotting
idata <- data %>% filter(PINCP>1000)
min(idata$PINCP)
max(idata$PINCP)
median(idata$PINCP)
IQR(idata$PINCP)

median(idata$PINCP[idata$EDUC=="PhD"])

set.seed(1)
sample=idata[sample(nrow(idata), 5000), ]
pdf("/Users/chadgevans/Research/PHD/Data_Analysis/Analysis/Output/Incomes_by_Degree_Type_2014.pdf")
boxplot(sample$PINCP~sample$EDUC, main="Boxplots of Incomes by Degree Type", ylab="Annual Income",col=c("yellow2","green4","blue2","red"), outline=F)
mtext("Source: American Community Survey",side=1,line=3,adj=1,cex=.8)
mtext("Chad Evans",side=1,line=3,adj=0,cex=.8)
dev.off()
#boxplot(idata$PINCP~idata$EDUC, main="Boxplot of PhD Incomes", ylab="Annual Income",col="green")


#random sample  for plotting
set.seed(1)
aboveThousand <- data %>% filter(PINCP>1000) # only people who earn more than $1000
freq <- 1000 #How many subsets are we creating
allStatNBA <- NULL
allStatBA <- NULL
allStatMAPD <- NULL
allStatPHD <- NULL

for(i in 1:freq){
  tempSample <-  aboveThousand[sample(nrow(aboveThousand), 1000), ]
  tempSampleNBA <- tempSample[tempSample$EDUC=="Less than a BA",]
  sampleStatNBA <- summarise(tempSampleNBA, MedianIncome=median(PINCP))
  allStatNBA <- rbind(allStatNBA, sampleStatNBA)
  tempSampleBA <- tempSample[tempSample$EDUC=="BA",]
  sampleStatBA <- summarise(tempSampleBA, MedianIncome=median(PINCP))
  allStatBA <- rbind(allStatBA, sampleStatBA)
  tempSampleMAPD <- tempSample[tempSample$EDUC=="Masters/Professional",]
  sampleStatMAPD <- summarise(tempSampleMAPD, MedianIncome=median(PINCP))
  allStatMAPD <- rbind(allStatMAPD, sampleStatMAPD)
  tempSamplePHD <- tempSample[tempSample$EDUC=="PhD",]
  sampleStatPHD <- summarise(tempSamplePHD, MedianIncome=median(PINCP))
  allStatPHD <- rbind(allStatPHD, sampleStatPHD)
}
incdata=rbind(allStatNBA, allStatBA, allStatMAPD,allStatPHD)
incdata2=cbind(incdata,c(rep("< BA",freq),rep("BA",freq),rep("Masters/Professional",freq),rep("PhD",freq)))
names(incdata2)=c("PINCP","EDUC")
boxplot(incdata2$PINCP~incdata2$EDUC, main="Boxplot of Incomes", 
        ylab="Annual Income",col=c("yellow2","green4","blue2","red"))
mtext("Source: American Community Survey",side=1,line=3,adj=1,cex=.8)
mtext("Chad Evans",side=1,line=3,adj=0,cex=.8)

