#file<-read.csv("/Users/chadgevans/Dropbox/Adjunct/Data/IPEDS/IPEDS_Analytics_DCP_87_10_CSV/delta_public_00_10.csv")
#data<-file[file$academicyear==2010,]
data<-read.csv("/Users/chadgevans/Research/PHD/Data_Analysis/Build/Input/IPEDS/sal2014_is.csv")

data<-read.csv("/Users/chadgevans/Research/PHD/Data_Analysis/Build/Input/IPEDS/eap2014.csv")

data$ARANK<-as.factor(data$ARANK)

data$FACSTAT<-as.factor(data$FACSTAT)
levels(data$FACSTAT)<-c("All staff","With faculty status, total","With faculty status, tenured","With faculty status, on tenure track","With faculty status not on tenure track/No tenure system, total","With faculty status not on tenure track/No tenure system, multi-year contract","With faculty status not on tenure track/No tenure system, annual contract","With faculty status not on tenure track/No tenure system, less-than-annual contract","Without faculty status","Faculty/tenure status not applicable, nondegree-granting institutions")

data$ARANK<-as.factor(data$ARANK)
levels(data$ARANK)<-c("Professors","Assoc. Professors","Asst. professors","Instructors","Lecturers","No academic rank","All instructional staff total")
data$ARANK2<-data$ARANK; levels(data$ARANK2)<-c("Professors","Assoc. Professors","Asst. Professors",rep("Adjuncts",4))
prop.table(table(data$ARANK2))

library(RColorBrewer)
colours <- brewer.pal(4, "Greens") 
pdf("/Users/chadgevans/Research/PHD/Data_Analysis/Analysis/Output/Tenured_Faculty_in_Academia.pdf")
par(mar=c(7.1,4.1,4.1,2.1))
mp <- barplot(prop.table(table(data$ARANK2)), axes = FALSE, axisnames = FALSE, main="Academic Rank of Faculty in Higher Education in 2014", ylab="Proportion", col=colours)
text(mp, par('usr')[1]- 0.02, labels = levels(data$ARANK2), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
mtext("Source: IPEDS Faculty Survey",side=1,line=5,adj=1,cex=.8, col="Black")
mtext("Chad Evans",side=1,line=5,adj=0,cex=.8, col="Black")
axis(2)
dev.off()


x <- barplot(prop.table(table(data$ARANK2)), xaxt="n")
labels <- levels(data$ARANK2)
text(cex=1, x=x-.25, y=-1.25, labels, xpd=TRUE, srt=45, pos=2)