library(RgoogleMaps)
library(rgdal)
library(knitr)
library(leaflet)
library(lubridate)
library(plotKML)
library(swfscMisc)
library(raster)
# setwd("C:/Users/gavin/Desktop/PhD/Chapter 1 - Vegetation and lizard's performance/Paper/Fixed maps")
setwd("Fixed_maps")


rasterOptions(tmpdir="/data2")
r_veg=list()
r_veg_boulder = list()
for (i in 1:3){
  r_veg_boulder[[i]] = raster(paste0("For_R_fixed_veg_and_rock_yesno_Tzeelim_",i,".tiff"))
  r_veg[[i]] = raster(paste0("../ndvi_veg_yesno_zeelim",i,".tif"))
}

#zeelim_veg <- mosaic(r_veg[[1]], r_veg[[2]], r_veg[[3]], fun=max )

r_veg[[i+1]] = raster("../ndvi_veg_yesno_mishmar1.tif")
for (j in 1:2){
  r_veg_boulder[[i+j]] = raster(paste0("For_R_Fixed_veg_and_rock_yesno_Mishmar_",j,".tiff"))
}

#fixing NA values - comment if no need
rasterOptions(tmpdir="/data2")
# for (x in 1:(i+j)){
#     rs = r_veg[[x]]
#     rs[is.na(rs[])] <- 0
#     name = ifelse (x<=3, paste0("For_R_fixed_veg_and_rock_yesno_Tzeelim_",x,".tiff"), paste0("For_R_Fixed_veg_and_rock_yesno_Mishmar_",x-3,".tiff"))
#     print(name)
#     writeRaster(rs, name, format="GTiff", overwrite=TRUE)
#     r_veg[[x]] = rs
# }


Data = read.csv("../Full Databse 20.01.2022.csv", header=T)
#create  spatial points
sp1 <- SpatialPoints(matrix(c(Data$Longitude,Data$Latitude), ncol=2), proj4string = CRS("+proj=longlat +datum=WGS84"))
#transform to meters lat and lon
sp1Transformed <- spTransform(sp1, CRS(as.character(r_veg[[2]]@crs)))


library(sf)


cords = coordinates(sp1Transformed)

is_inside = function(x, y, r){
  r_ymin = ymin(r)
  r_xmin = xmin(r)
  r_ymax = ymax(r)
  r_xmax = xmax(r)
#  browser()
  if (r_xmin < x & x < r_xmax & r_ymin < y & y < r_ymax)  
    return(TRUE)
  else
    return(FALSE)
}
#example for testing if the function works: is_inside(x=723859.9, y=3471127, r=r_veg[[2]])
is_inside(x=723859.9, y=3471127, r=r_veg[[3]])
#example for testing if the function works: extent(r_veg[[2]])
extent(r_veg[[2]])

find_raster_with_coordinate = function (x, y, r_list, size_of_list){
  found=FALSE
  i=1
  while (i<=size_of_list & (!found)){
    print (i)
    found = is_inside(x,y,r_list[[i]])
    if (found){
      return(i)
    } else {
      i=i+1
    }
  }
  return(NA)
}

find_raster_with_coordinate(x=720065.7, y=3573899, r=r_veg, size_of_list = 4)


id <- Data$ID
wanted_radius <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
veg_data <- data.frame(ID=id)
for(r in wanted_radius){
    veg_data[,paste0("fveg",as.character(r))] <- NA
    veg_data[,paste0("frock",as.character(r))] <- NA
    veg_data[,paste0("fvegrock",as.character(r))] <- NA
}
library(plotrix)
for (i in 1:length(sp1Transformed)){
  if ((!is.na(r_veg_current)) & (!is.na(r_vegrock_current))){
    r_veg_current = find_raster_with_coordinate(cords[i,1], cords[i,2], r_veg, length(r_veg))
    r_vegrock_current = find_raster_with_coordinate(cords[i,1], cords[i,2], r_veg_boulder, length(r_veg_boulder))
    rc_veg <- crop(r_veg[[r_veg_current]], extent(c(cords[i,1]-100, cords[i,1]+100, cords[i,2]-100, cords[i,2]+100)))
    rc_vegrock <- crop(r_veg_boulder[[r_vegrock_current]], extent(c(cords[i,1]-100, cords[i,1]+100, cords[i,2]-100, cords[i,2]+100)))
    rc_veg = resample(rc_veg, rc_vegrock, method="ngb")
    rc_vegrock[rc_veg==1]=1
    rc_rock = rc_vegrock - rc_veg
    plot(rc_rock)
    for (r in wanted_radius){
      veg_percentage = extract(rc_veg, cbind(cords[i,1], cords[i,2]), buffer=r, fun=mean)
      vegrock_percentage = extract(rc_vegrock, cbind(cords[i,1], cords[i,2]), buffer=r, fun=mean)
      rock_percentage = extract(rc_rock, cbind(cords[i,1], cords[i,2]), buffer=r, fun=mean)
      draw.circle(cords[i,1], cords[i,2],r)
      print(paste("individual", veg_data$ID[i], "Has", veg_percentage, rock_percentage, vegrock_percentage, "In radius", r))
      veg_data[i,paste0("fveg",as.character(r))] <- veg_percentage
      veg_data[i,paste0("frock",as.character(r))] <- rock_percentage
      veg_data[i,paste0("fvegrock",as.character(r))] <- vegrock_percentage
    }
  } else {
    print (paste("coordinate", cords[i,1], cords[i,2], "not in the raster"))
  }
}


library(lubridate)
write.csv(veg_data, file = paste("Rock_and_Veg_Cover_Results",today(), "csv", sep="." ))
#############################################################################################################

##r_veg_current <- find_raster_with_coordinate(cords[Data$ID=="GS47",1], cords[Data$ID=="GS47",2], r_veg, size_of_list = 2)
##plot(r_veg[[r_veg_current]])
##points(cords[Data$ID=="GS09",1], cords[Data$ID=="GS09",2], pch=19, col="blue")
###plot(r_veg[[1]], col=c("NA", "green"), add=T)




# veg_percentage_10m = extract(r_veg[[r_veg_current]], cbind(cords[i,1], cords[i,2]), buffer=10, fun=mean)
# veg_percentage_50m = extract(r_veg[[r_veg_current]], cbind(cords[i,1], cords[i,2]), buffer=50, fun=mean)
# veg_percentage_100m = extract(r_veg[[r_veg_current]], cbind(cords[i,1], cords[i,2]), buffer=100, fun=mean)
# #save in data frame

#Data[i,]$file = r_veg[[r_veg_current]]@file@name
# Data[i,]$fveg10m = veg_percentage_10m
# Data[i,]$fveg50m = veg_percentage_50m
# Data[i,]$fveg100m = veg_percentage_100m
#rgb="D:/OfirL2/Desktop/PhD/Chapter 2 - Vegetation Cover and Life-History of Desert Lizards/Nahal Mishmar - 1.tif"
#rgb_file = "/home/ofir/eclipse workspace/lab/Gavin/Nahal Mishmar - 1.tif"
# rgb_files_zeelim = c("/home/ofir/eclipse workspace/lab/Gavin/IX-12-11646_0067_transparent_mosaic_group1.tif",
#                       "/home/ofir/eclipse workspace/lab/Gavin/IX-12-11646_0072_transparent_mosaic_group1.tif",
#                       "/home/ofir/eclipse workspace/lab/Gavin/IX-12-11646_0073_transparent_mosaic_group1.tif")

# for (i in 1:3){
#   r <- stack(rgb_files_zeelim[i])
#   R = r[[1]]; G = r[[2]]; B= r[[3]]
#   r_tgi = (G-0.39*R-0.61*B)/max(R, max(G, B))
#   writeRaster(r_tgi, file=paste0("tgi_zeelim_",i,".tif"))
# }

# for (i in 2:2){
#    r_tgi = raster(paste0("tgi_zeelim_",i,".tif"))
#    r_veg = r_tgi
#    r_veg[r_tgi>0.04]=1
#    r_veg[r_tgi<0.04]=0
#    writeRaster(r_veg, file=paste0("veg_yesno_zeelim",i,".tif"), overwrite=T)
# }
# rgb = "/home/ofir/eclipse workspace/lab/Gavin/Nahal Mishmar - 2.tif"
# r2 <- stack(rgb)
# m1 <- merge(r2, r)
# plotRGB(r2)
# plotRGB(r1, add=T)
#load points:
#Data <- read.csv("D:/OfirL2/Desktop/PhD/Chapter 2 - Vegetation Cover and Life-History of Desert Lizards/Nahal Mishmar Coordinates for Analysis.csv")
#Data = read.csv("/home/ofir/eclipse workspace/lab/Gavin/Nahal Mishmar Coordinates for Analysis.csv")
#Data = read.csv("/home/ofir/eclipse workspace/lab/Gavin/T'zeelim Coordinates for Analysis.csv")
#Data = read.csv("/home/ofir/eclipse workspace/lab/Gavin/Coordinates for Analysis 06.02.20.csv")
#Data = read.csv("/home/ofir/eclipse workspace/lab/Gavin/MyData2_Zeelim.csv")
radius = 1000 # 100 meters distance
# scalebar(100,xy = c(cords[i,1], cords[i,2]), label = "100 m", col="white", lwd =6, cex=3)
#circle <- st_buffer(cords[i,], 100)
