library(RgoogleMaps)
library(rgdal)
library(knitr)
library(leaflet)
library(lubridate)
library(plotKML)
library(swfscMisc)
library(raster)

#set the temp folder or the raster package - the default is the system partition 
rasterOptions(tmpdir="~/r_temp")

#load the vegetation and rock files - both were validated and corrected around 100 m from each lizard
r_veg=raster("Data/Fixed_maps/output_veg.tif")
r_veg_boulder = raster("Data/Fixed_maps/output_rockveg.tif")

Data = read.csv("Data/lizards_Mi_data_Appendix_S1.csv", header=T)

#create  spatial points
sp1 <- SpatialPoints(matrix(c(Data$Longitude,Data$Latitude), ncol=2), proj4string = CRS("+proj=longlat +datum=WGS84"))
#transform to meters lat and lon
sp1Transformed <- spTransform(sp1, CRS(as.character(r_veg@crs)))

library(sf)
cords = coordinates(sp1Transformed)

id <- Data$ID
wanted_radius <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
veg_data <- data.frame(ID=id)
for(r in wanted_radius){
    veg_data[,paste0("fveg",as.character(r))] <- NA
    veg_data[,paste0("frock",as.character(r))] <- NA
}

library(plotrix)
for (i in 1:length(sp1Transformed)){
    #get a map around the lizard - a box of 200*200 m2 with the lizard in the middle
    rc_veg <- crop(r_veg, extent(c(cords[i,1]-100, cords[i,1]+100, cords[i,2]-100, cords[i,2]+100)))
    rc_vegrock <- crop(r_veg_boulder, extent(c(cords[i,1]-100, cords[i,1]+100, cords[i,2]-100, cords[i,2]+100)))
    
    #make sure the maps align perfectly
    rc_veg = resample(rc_veg, rc_vegrock, method="ngb")
    
    #make sure vegrock has all the veg pixels from the NDVI as well
    rc_vegrock[rc_veg==1]=1
    
    #calculate the rock cover 
    rc_rock = rc_vegrock - rc_veg
    
    #calculate rock and veg cover for each of the radiuses
    for (r in wanted_radius){
      veg_percentage = extract(rc_veg, cbind(cords[i,1], cords[i,2]), buffer=r, fun=mean, na.rm=F)
      rock_percentage = extract(rc_rock, cbind(cords[i,1], cords[i,2]), buffer=r, fun=mean, na.rm=F)
      draw.circle(cords[i,1], cords[i,2],r)
      print(paste("individual", veg_data$ID[i], "Has", veg_percentage, rock_percentage, "in radius", r))
      veg_data[i,paste0("fveg",as.character(r))] <- veg_percentage
      veg_data[i,paste0("frock",as.character(r))] <- rock_percentage
    }
}

all_data = cbind(Data[,1:8], veg_data)
library(lubridate)
write.csv(all_data, file = paste("data_with_veg_and_rock_cover",today(), "csv", sep="." ), row.names = F)
