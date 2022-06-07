library(RgoogleMaps)
library(rgdal)
library(knitr)
library(leaflet)
library(lubridate)
library(plotKML)
library(swfscMisc)
library(raster)


Data <- read.csv("Data/lizards_Mi_data_Appendix_S1.csv",header=T)
#create  spatial points
sp1 <- SpatialPoints(matrix(c(Data$Longitude,Data$Latitude), ncol=2), proj4string = CRS("+proj=longlat +datum=WGS84"))
#transform to meters lat and lon
sp1Transformed <- spTransform(sp1, CRS(as.character(r_veg@crs)))


library(sf)
library(plotrix)
cords = coordinates(sp1Transformed)

#get the individuals with most and least rock availability
low_veg  = which(abs(Data$fveg90-min(Data$fveg90))<0.0001)
high_veg  = which(abs(Data$fveg90-max(Data$fveg90))<0.001)
Data$fveg90[high_veg]

#load RGB maps  - too large to upload to github
Mishmar2 = brick("~/Downloads/IX-12-11646_0052_transparent_mosaic_group1.tif")
Mishmar1 = brick("~/Downloads/IX-12-11646_0051_transparent_mosaic_group1.tif")
Zeelim1 = brick("~/Dropbox/eclipse workspace/lab/Gavin/IX-12-11646_0067_transparent_mosaic_group1.tif")

tiff(file="Figure 2.tiff", width=2000, height=2000, res=600, compression="lzw")
par(mfrow=c(2,2), cex=1.1)
par(mar=c(0,0.5,1,0.5))
i=low_veg
plotRGB(Mishmar2, ext = extent(c(cords[i,1]-90, cords[i,1]+90, cords[i,2]-90, cords[i,2]+90)), margins=T)
draw.circle(cords[i,1], cords[i,2],90)
mtext(paste0("Vegetation=", round(Data$fveg90[i]*100, 2),"%"), side=3, line=0, cex=0.7)

i=high_veg
par(mar=c(0,0.5,1,0.5))
plotRGB(Mishmar1, ext = extent(c(cords[i,1]-90, cords[i,1]+90, cords[i,2]-90, cords[i,2]+90)), margins=T)
draw.circle(cords[i,1], cords[i,2],90)
mtext(paste0("Vegetation=", round(Data$fveg90[i]*100, 2),"%"), side=3, line=0, cex=0.7)

low_rock  = which(abs(Data$frock10-min(Data$frock10))<0.01)
low_rock = low_rock[1]
i=low_rock
par(mar=c(0,0.5,1,0.5))
plotRGB(Mishmar1, ext = extent(c(cords[i,1]-10, cords[i,1]+10, cords[i,2]-10, cords[i,2]+10)), margins=T)
draw.circle(cords[i,1], cords[i,2],10)
mtext(paste0("Rock=", round(Data$frock10[i]*100, 2),"%"), side=3, line=0, cex=0.7)

i=46#high_rock[1]
Data[i,]
par(mar=c(0,0.5,1,0.5))
plotRGB(Mishmar2, ext = extent(c(cords[i,1]-10, cords[i,1]+10, cords[i,2]-10, cords[i,2]+10)), margins=T)
draw.circle(cords[i,1], cords[i,2],10)
mtext(paste0("Rock=", round(Data$frock10[i]*100, 2),"%"), side=3, line=0, cex=0.7)

dev.off()
