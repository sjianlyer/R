## loading packages ##################
if (!"raster" %in% installed.packages()) install.packages("raster")
if (!"rgdal" %in% installed.packages()) install.packages("rgdal")
if (!"plotrix" %in% installed.packages()) install.packages("plotrix")
library(raster)
library(plotrix)
setwd("D:\\S_Work\\FYY2\\")

## open and read raster ###############
r1<-raster("N_MStdDem0_5Z/N_MStdDem0_5Z.img")
r2<-raster("N_MRangeDem0_5Z/N_MRangeDem0_5Z.img")
r3<-raster("N_MStdSlope0_5Z/N_MStdSlope0_5Z.img")
r4<-raster("N_MRangeSlope0_5Z/N_MRangeSlope0_5Z.img")
r5<-raster("N_MStdProCur0_5Z/N_MStdProCur0_5Z.img")
r6<-raster("N_MRugosity0_5Z/N_MRugosity0_5Z.img")
r7<-raster("CTCI0_5Z\\ctci0_5Z.img")
r8<-raster("N_MStdSR0_5Z/N_MStdSR0_5Z.img")

plotTopo<-function(imageData, paraNo){
  # just draw the plot region, without axes, labels, and box
  plot(c(72,136),c(17,54),axes=F,type="n",xlab="",ylab="")
  
  # draw the image data
  image(imageData, col=terrain.colors(256), add=T)
  
  # X axis
  axis(1, labels=F, tick=F)
  segments(c(80, 130), 17, c(80, 130), 18, lwd=2, xpd=F)
  text(c(80.5,130.5), 19.5, c(expression("80"^"o"),expression("130"^"o")), cex=2) 

  # Y axis
  axis(2, labels=F, tick=F)
  segments(72, c(25, 47), 73, c(25, 47), lwd=2, xpd=F)
  text(76, c(25,47), c(expression("25"^"o"), expression("47"^"o")), cex=2)
  
  # other labels
  text(74, 51, paraNo, cex=3, adj=0)
  
  box(lwd=3)
}
# Legend
northarrow3<-function(xshift,yshift,size){
  x<-c(0.35, 0.39, 0.39, 0.60, 0.65, 0.65, 0.61, 0.61, 0.39, 0.35, 0.35)*size+xshift
  y<-c(0.75, 0.75, 0.92, 0.75, 0.75, 1.00, 1.00, 0.83, 1.00, 1.00, 0.75)*size+yshift
  
  xl<-c(0.5, 0.3, 0.5, 0.5)*size+xshift
  yl<-c(0.24, 0.01, 0.7, 0.24)*size+yshift
  xr<-c(0.5, 0.5, 0.7, 0.5)*size+xshift
  yr<-c(0.24, 0.7, 0.01, 0.24)*size+yshift
  xr2<-c(0.5, 0.5, 0.692, 0.5)*size+xshift
  yr2<-c(0.245, 0.69, 0.02, 0.245)*size+yshift
  
  return(SpatialPolygons(list(Polygons(list(Polygon(cbind(x,y))), ID="text"),
                              Polygons(list(Polygon(cbind(xl, yl))),
                                       ID="north"),
                              Polygons(list(Polygon(cbind(xr, yr)),
                                            Polygon(cbind(xr2, yr2),hole=TRUE)),
                                       ID="north2")),1:3))
}

setwd("D:\\S_Work\\FYY\\Group_Plots\\20140729\\")
tiff(filename="Figure 2 of paper 1 for submitting 20140729.tiff", width=12, height=20, unit='in', res=500, compression="lzw")

parBAK<-par(no.readonly=T)
par(mfrow=c(4,2), mai=c(0.2,0.2,0.2,0.2), omi=c(1.5,0.1,0.1,0.1), xaxs="i", yaxs="i", family="serif", font=1)

plotTopo(r1, "(a) SE")
plotTopo(r2, "(b) RE")
plotTopo(r3, "(c) SS")
plotTopo(r4, "(d) RS")
plotTopo(r5, "(e) SC")
plotTopo(r6, "(f) RU")
plotTopo(r7, "(g) CTCI")
plotTopo(r8, "(h) SSR")

par(xpd=NA, font=2)
text(70.25, 12, "Topographic complexity", cex=4)
gradient.rect(36.5,6,104,8,col=terrain.colors(256))
text(c(32.5,108),8.5,c("0","1"),cex=2, font=1)
text(c(32.5,108),5.5,c("low","high"),cex=2, font=1)
plot(northarrow3(120,4.5,11),col=1,pbg="white",add=T)
par(xpd=F)

par(parBAK)
dev.off()