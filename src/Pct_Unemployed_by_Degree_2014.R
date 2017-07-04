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
table <- table(data$EMPLOYED,data$EDUC)
ptable<-prop.table(table, 2)

# Bar Plot of Percent Unemployed by Degree Type in 2014
pdf("/Users/chadgevans/Research/PHD/Data_Analysis/Analysis/Output/Barplot_Pct_Unemployed_by_Degree_2014.pdf")
barplot(100*ptable[2,], ylab="Percent",main="Percent Unemployed by Degree Type in 2014",
        col = c("yellow2","green4","blue2","red"),cex.names=0.85)
title(xlab = "Degree", line = 2.2)
mtext("Chad Evans",side=1,line=3,adj=0,cex=.8)
mtext("Source: American Community Survey",side=1,line=3,adj=1,cex=.8)
dev.off()