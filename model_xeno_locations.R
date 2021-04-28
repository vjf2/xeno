#Simple model of xeno locations on dorsal fin

library(raster)

ir <- raster("IR_dorsal transparent.JPG")

load("tpoints.RData")

tp <- coordinates(tpoints)

#need to convert tpoints axes to image axes

#tpoints x 0:912, y 608:0
#image x 0:480, y 0:284

rescale_x <- function(x, oldmax, newmax) {((x-0)/(oldmax - 0) * newmax)} 

rescale_y <- function(x, oldmax, newmax) {newmax - ((x-0)/(oldmax - 0) * newmax) } 

x2 <- rescale_x(tp[,1], 912, 480)
y2 <- rescale_y(tp[,2], 608, 284)

windows()
plot(ir)
points(x2, y2)
points(x2[260:262], y2[260:262], col=c("red", "blue", "black"), pch=16) #anchor points on thermal image

#Put rescaled points back into spatial points object
scaled_points <- SpatialPoints(coords=cbind(x2, y2))

#get values from the raster of xeno points
scale<-extract(ir, scaled_points)

##Match to key, interpolate key values
load("key.RData")
range(key$scalevalues)
length(2:253)
realtemp <- seq(26.4, 35.6, length.out=252)

bigkey <- data.frame(pixelvalue=2:253, realtemp)

temp_coord <- data.frame(x2,
                         y2,
                         scale)

temp_coord$realtemp <- bigkey$realtemp[match(round(temp_coord$scale,0), 
                                             bigkey$pixelvalue)]

##################################################
#Remove background of image
#Resize grid cells to a biologically relevant size
##################################################

#Can't perfectly block out all of the background 
# without blocking out some of the fin, but this
# should be close enough for now

ir2 <- ir
ir2[ir2[]>=248] <- NA #remove all pixel values about 248

windows()
par(mfrow=c(1,2))
plot(ir, colNA="white")
plot(ir2, colNA="white")
points(x2, y2)


#crop the fin to just the dorsal fin area
temp_coord[260:262,]

just_fin <- crop(ir2, extent(10, 320, 70, 290))

windows()
plot(just_fin)

#Merge pixels so that each pixel represents an are of about 1cm2
#Tolley et al 1995 (doi:10.2307/1382611) has some dorsal fin measurements
#Dorsal fin surface area ranged from 347-999 cm2, so we'll just aim
# for something within that range for now

#current number of non-NA pixels
current_pixels <- ncell(just_fin)-freq(just_fin, value=NA)

sqrt(current_pixels/600) # factor to adjust cell size

just_fin <- aggregate(just_fin, fact=7) #resize

windows()
plot(just_fin)
points(x2, y2)
points(x2[260:262], y2[260:262], col=c("red", "blue", "black"), pch=16) #anchor points

#For now just shift Xeno points off of fin slightly to the left

scale <- extract(just_fin, scaled_points)

temp_coord <- data.frame(x2,
                         y2,
                         scale)

temp_coord$realtemp <- bigkey$realtemp[match(round(temp_coord$scale,0), 
                                             bigkey$pixelvalue)]

#shift points outside of fin one pixel to the left to get on fin 
temp_coord$x2[is.na(temp_coord$realtemp)] <- temp_coord$x2[is.na(temp_coord$realtemp)]-7

points(temp_coord$x2, temp_coord$y2, pch=16)

#Get values for all cells
allcells <- data.frame(cellnumber = 1:length(just_fin@data@values), 
                       pixelvalue = just_fin@data@values)

allcells <- allcells[!is.na(allcells$pixelvalue),] #only non-NA cells

coords <- xyFromCell(just_fin, cell = allcells$cellnumber)

allcells$dist2tip <- pointDistance(coords, temp_coord[260, c("x2", "y2")], lonlat=FALSE)

#get count of points on each pixel
counts = table(cellFromXY(just_fin,temp_coord[,c("x2", "y2")]))

allcells$countXeno <- counts[match(allcells$cellnumber, names(counts))]

#Cells which have a count of 57 are the anchor points
allcells$countXeno[which(allcells$countXeno==57)] <- 0

allcells$countXeno[is.na(allcells$countXeno)] <- 0 

allcells$realtemp <- bigkey$realtemp[match(round(allcells$pixelvalue,0), 
                                           bigkey$pixelvalue)]

#######
#Model
#######

#Use a poisson model to estimate counts of Xeno per grid cell of fin based on temp and distance to tip

mod <- glm(countXeno ~ realtemp + dist2tip, data=allcells, family="poisson")

summary(mod)  