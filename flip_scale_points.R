library(raster)

ir <- raster("IR_dorsal transparent.png")

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
points(x2[260:262], y2[260:262], col="red", pch=16) #anchor points on thermal image
