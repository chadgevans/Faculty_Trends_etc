rm(list=ls())

options(scipen=5)
packages=c("plyr", "ggplot2","rgeos", "maptools","rgdal")
lapply(packages, require, character.only = TRUE)

# Directories and Cleaned Data
Product_Directory<-"/Users/chadgevans/Dissertation/Projects/Products"
Data_Directory<-"/Users/chadgevans/Dissertation/Projects/Build_Dataset/IPEDS/Build_Code/Map_data"
USA_Shapefile_directory <- "/Users/chadgevans/Dissertation/Projects/Build_Dataset/US_States_Shapefile/2014" # Set working directory

USA.shp <- readOGR(dsn = USA_Shapefile_directory, layer = "cb_2014_us_state_500k")
#proj4string(USA.shp) <- CRS("+proj=longlat")
us_aea.shp <- spTransform(USA.shp, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea.shp@data$id <- rownames(us_aea.shp@data)

alaska <- us_aea.shp[us_aea.shp$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea.shp)

hawaii <- us_aea.shp[us_aea.shp$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea.shp)

us_aea.shp <- us_aea.shp[!us_aea.shp$STATEFP %in% c("02", "15", "72"),]
us_aea.shp <- rbind(us_aea.shp, alaska, hawaii)
map <- fortify(us_aea.shp, region="GEOID") # fortify changes to dataframe for ggplot

plot(us_aea.shp, ylim=c(35, 45), xlim=c(-70.4, -90.35), main="Working Map")


CUSAdata <- as(CUSA.shp, "data.frame")
names(ds)[1]<-"STUSPS"
CUSA.shp@data <- data.frame(as(CUSA.shp, "data.frame"), ds[match(CUSA.shp@data[, "STUSPS"], ds[, "STUSPS"]),])
