##How to find net cdf location based on lat long

pts <- read.csv("Xeno data (updated 5_30_21).csv")

#example file 

library(ncdf4)

##Function to fill in NA with nearest value
f1 <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}


ncin <- nc_open("D:/SatelliteImagery/SST CoralWatch/monthly/2020/ct5km_sst-mean_v3.1_202001.nc")

lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
sst <- ncvar_get(ncin,"sea_surface_temperature")

nc_close(ncin)

#Get cell coordinates for corresponding lat long

for(i in 1:nrow(pts)){
  
  pts$Latcell[i] = which.min(abs(pts$Latitude[i] - lat))
  pts$Longcell[i] = which.min(abs(pts$Longitude[i] - lon))
  
}

#First, check is there's an available temp or report NA
pts$TempAvailable <- apply(pts[, c("Longcell", "Latcell")], 1, 
                    function(x) {sst[x[1], x[2]]} )

##Nest, source F1 function below to fill in NA cells with the nearest value

#generate list of files
b <- list.files(path="D:/SatelliteImagery/SST CoralWatch/monthly/2020", 
                pattern=".nc$", recursive=TRUE, full.names = TRUE)

mytmps <- list()

for (i in 1:length(b)){
  
  ncin <- nc_open(b[i])
  
  alltmps <- ncvar_get(ncin, "sea_surface_temperature")
  
  myfill <- f1(alltmps)
  
  mytmps[[i]] <- apply(pts[, c("Longcell", "Latcell")], 1, 
                       function(x) {myfill[x[1], x[2]]} )
  
  nc_close(ncin)
  
}

final <- as.data.frame(do.call("cbind", mytmps))
names(final) <- month.name

final[, "mean_sst"] <- rowMeans(final)
final[, "min_sst"] <- apply(final[,1:12], 1, min)
final[, "max_sst"] <- apply(final[,1:12], 1, max)
final[, "sd_sst"] <- apply(final[,1:12], 1, sd)

final <- cbind(pts, final)

write.csv(final, "xeno_locations_sst_20210601.csv")



