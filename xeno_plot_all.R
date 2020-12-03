#Check photos for errors and align all xeno

xeno <- read.csv("Xeno_attachment.csv", row.names = 1)

#Add columns
xeno$dolphin_id <- as.numeric(as.factor(xeno$image.name))

xeno$color<-ifelse(xeno$point.type=="fin", "black", "red")
xeno$finlab<-ifelse(xeno$point..==1 &
                      xeno$point.type=="fin", "tip", NA)

xeno$finlab<-ifelse(xeno$point..==2 &
                      xeno$point.type=="fin", "back", xeno$finlab)

xeno$finlab<-ifelse(xeno$point..==3 &
                      xeno$point.type=="fin", "front", xeno$finlab)

xeno$finlab<-ifelse(xeno$point.type=="xeno", "xeno", xeno$finlab)

#check points and remove ones that need to be recoded
# i =1
# dev.new()
# d1<-xeno[xeno$dolphin_id==i,]
# plot(d1$x, d1$y, type="n",
#      xlim=rev(range(d1$x)),
#      ylim=rev(range(d1$y)), 
#      main=i)
# text(d1$x, d1$y, label=d1$finlab, col=d1$color)
# 
# i = i + 1

#recode or check these photos

bads<-c(3, 10, 13, 16, 25,
        31, 35, 37, 42, 
        7,8,9,32,38,44,46) 

xeno <- xeno[-which(xeno$dolphin_id %in% bads),]

#22 is a good one to match to

library(vec2dtransf)

#Pull out points by location
front<-xeno[xeno$finlab=="front",]
back<-xeno[xeno$finlab=="back",]
tip<-xeno[xeno$finlab=="tip",]

#Test transformation using example from 
#http://geotux.tuxfamily.org/index.php/en/geo-blogs/item/302-affine-and-similarity-transformations-in-r

d2f<-front[which(front$dolphin_id==22),]
d2b<-back[which(back$dolphin_id==22),]
d2t<-tip[which(tip$dolphin_id==22),]

transformed <- list()

for(i in 1:length(unique(xeno$dolphin_id))){

  d<-unique(xeno$dolphin_id)[i]  
    
  d1f<-front[which(front$dolphin_id==d),]
  d1b<-back[which(back$dolphin_id==d),]
  d1t<-tip[which(tip$dolphin_id==d),]
  
  
  cpoints<-matrix(c(d1f$x, d1f$y, d2f$x, d2f$y, 
                    d1b$x, d1b$y, d2b$x, d2b$y, 
                    d1t$x, d1t$y, d2t$x, d2t$y), nrow=3, byrow = TRUE)
  
  cpoints<-data.frame(cpoints)
  
  aft<-AffineTransformation(cpoints)
  calculateParameters(aft)
  getParameters(aft)
  
  d1<-xeno[xeno$dolphin_id==d,]
  
  p2<-SpatialPointsDataFrame(coords=d1[,c("x", "y")], 
                             data=d1[,c("finlab", "color", "dolphin_id")])
  
  newLines = applyTransformation(aft, p2)
  
  transformed[[i]] <- newLines

}

tpoints<-do.call("rbind", transformed)

dev.new()
plot(tpoints@coords, type="n",
     xlim=rev(tpoints@bbox[1,]),
     ylim=rev(tpoints@bbox[2,]), 
     main="All Xeno")
text(tpoints@coords[,1], tpoints@coords[,2], label=tpoints$finlab, col=tpoints$color)
