#rm(list=ls())
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

stateTotalPHD  <- data %>% filter(EDUC=="PhD") %>% group_by(ST)%>% summarise(count = n())
#Do the join before percentage computation to make sure you have a count for each state including 0s
jobLessPHD  <- data %>% filter(EDUC=="PhD", EMPLOYED=="Unemployed") %>% group_by(ST)%>% summarise(count = n())
names(jobLessPHD)=c("ST","jobLesscount")
df=merge(stateTotalPHD,jobLessPHD, by="ST",all.x=T)
ds=data.frame(df$ST,100*df$jobLesscount/df$count)
names(ds)=c("GEOID","PCT")
ds$GEOID=sprintf("%02d",c(ds$GEOID))
ds$GEOID=as.factor(ds$GEOID)
ds$PCT[is.na(ds$PCT)] <- 0

# for theme_map
devtools::source_gist("33baa3a79c5cfef0f6df")
datdir <- "/Users/chadgevans/Research/PHD/Data_Analysis/Build/Input/US_States_Shapefile/2014/" # Set working directory
USA.shp <- readOGR(dsn = datdir, layer = "cb_2014_us_state_500k")

# convert it to Albers equal area
us_aea.shp <- spTransform(USA.shp, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea.shp@data$id <- rownames(us_aea.shp@data)
alaska <- us_aea.shp[us_aea.shp$STATEFP=="02",]
alaska <- elide(alaska, rotate=-35)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea.shp)
hawaii <- us_aea.shp[us_aea.shp$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5200000, -1400000))
proj4string(hawaii) <- proj4string(us_aea.shp)

us_aea.shp <- us_aea.shp[!us_aea.shp$STATEFP %in% c("02", "15", "72"),]
us_aea.shp <- rbind(us_aea.shp, alaska, hawaii)
object.size(as(us_aea.shp, "SpatialPolygons"))

us_aea2.shp <- thinnedSpatialPoly(us_aea.shp, tolerance=0.05, minarea=0.001)
object.size(as(us_aea2.shp, "SpatialPolygons"))

us_aea3.shp=merge(us_aea2.shp,ds, by="GEOID",all=F)
object.size(as(us_aea3.shp, "SpatialPolygons"))

us_aea4.shp <- spTransform(us_aea3.shp, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
object.size(as(us_aea4.shp, "SpatialPolygons"))

# plot(us_aea4.shp, main="States with Highest Unemployment Rates for PHD's")

#colours <- brewer.pal(5, "Reds") 
colours <- c("#1A9641","#A6D96A","#FFFFBF","#FDAE61","#D7191C")
brks<-classIntervals(round(us_aea4.shp$PCT,1), n=5, style="quantile")
plot(brks, pal=colours)
brks<- brks$brks

pdf("/Users/chadgevans/Research/PHD/Data_Analysis/Analysis/Output/PhD_Unemployment_US_Map.pdf")
plot(us_aea4.shp, col=colours[findInterval(us_aea4.shp$PCT, brks, all.inside=TRUE)], axes=F,main="States with Highest Unemployment Rates for PhD's")
box()
legend("bottomleft", legend=leglabs(brks), fill=colours, bty="n")
mtext("Source: American Community Survey",side=1,line=3,adj=1,cex=.8)
mtext("Chad Evans",side=1,line=3,adj=0,cex=.8)
dev.off()

## a north arrow: SpatialPolygonsRescale(layout.north.arrow(1), offset= c(505100,160000), scale = 6000,plot.grid=F)
## a legend: legend(x=548500, y=164800, legend=leglabs(brks), fill=colours, bty="n")
## a scale bar: SpatialPolygonsRescale(layout.scale.bar(), offset= c(503800,154800), scale= 10000, fill=c("transparent", "black"), plot.grid= F)


