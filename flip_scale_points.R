library(raster)

ir <- raster("IR_dorsal transparent.png")

load("tpoints.RData")
tp <- coordinates(tpoints)

#need to convert tpoints axes to image axes

#tpoints x 912:0, y 608:0
#image x 0:480, y 0:284

rescale <- function(x, oldmax, newmax) {newmax - ((x-0)/(oldmax - 0) * newmax) } 

x2 <- rescale(tp[,1], 912, 480)
y2 <- rescale(tp[,2], 608, 284)

plot(ir)
points(x2, y2)
