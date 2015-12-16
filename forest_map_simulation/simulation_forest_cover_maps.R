#Script for simulating random forest cover maps with defined covariance structure
#Paul Magdon
# 16/12/2015
#V0.1
###########################################################################

library(raster)
library(RandomFields)

forest.map<-function(pixel,model,proportion){
  #create locations
  xpixel<-pixel
  ypixel<-pixel
  from <- 0
  to <- 1
  x.seq <- seq(from, to, length=pixel) 
  y.seq <- seq(from, to, length=pixel)
  r=GridTopology(c(0,0),c(1,1),c(pixel,pixel))
  #Create RandomField
  simu<- RFsimulate(model, x=x.seq, y=y.seq)
  map<-raster(simu)
  thresh<-qnorm(proportion,mean=mean(values(map)),sd=sd(values(map)))
  bin<-map<thresh
  return(bin)
}

#Create a forest map 

RFoptions(seed=NA)


#Define the Co-Variance model for spatial auto-correlation
#To get a list of all implemented models type:
#RFgetModelNames(type="positive definite")

#Here we use a Cauchy Co-Variance Model: low alpha -> high fragmentation 
model <- RMgencauchy(alpha=0.5,beta=0.3) 

#Define the side length of the map in pixel number. We assume a quadratic map
pixel<-400

#Define the proportion of forest in the map
prop=0.5

#Create the binary forest map
forest<-forest.map(pixel, model, prop)


plot(forest)
