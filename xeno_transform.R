#Do an affine transformation to align fin points to make xeno graph

library(vec2dtransf)

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

#Pull out points by location
front<-xeno[xeno$finlab=="front",]
back<-xeno[xeno$finlab=="back",]
tip<-xeno[xeno$finlab=="tip",]

#Test transformation using example from 
#http://geotux.tuxfamily.org/index.php/en/geo-blogs/item/302-affine-and-similarity-transformations-in-r
d1f<-front[which(front$dolphin_id==1),]
d1b<-back[which(back$dolphin_id==1),]
d1t<-tip[which(tip$dolphin_id==1),]
d2f<-front[which(front$dolphin_id==2),]
d2b<-back[which(back$dolphin_id==2),]
d2t<-tip[which(tip$dolphin_id==2),]

cpoints<-matrix(c(d1f$x, d1f$y, d2f$x, d2f$y, 
           d1b$x, d1b$y, d2b$x, d2b$y, 
           d1t$x, d1t$y, d2t$x, d2t$y), nrow=3, byrow = TRUE)

cpoints<-data.frame(cpoints)

aft<-AffineTransformation(cpoints)
calculateParameters(aft)
getParameters(aft)

d1<-xeno[xeno$dolphin_id==1,]
d2<-xeno[xeno$dolphin_id==2,]

p2<-SpatialPointsDataFrame(d1[,c("x", "y")], data=d1[,c("finlab", "color")])

newLines = applyTransformation(aft, p2)

#Plot originals and transformed
windows()
par(mfrow=c(1,3))

plot(d1$x, d1$y, type="n",
     xlim=rev(range(d1$x)),
     ylim=rev(range(d1$y)),
     main="Dolphin 1")
text(d1$x, d1$y, label=d1$finlab, col=d1$color)

plot(d2$x, d2$y, type="n",
     xlim=rev(range(d2$x)),
     ylim=rev(range(d2$y)),
     main="Dolphin 2")
text(d2$x, d2$y, label=d2$finlab, col=d2$color, font=3)

plot(d2$x, d2$y, type="n",
     xlim=rev(c(550,620)),
     ylim=rev(c(245,300)),
     main="Aligned")
text(d2$x, d2$y, label=d2$finlab, col=d2$color)

text(newLines, label=newLines$finlab, col=newLines$color, 
     font=3)












