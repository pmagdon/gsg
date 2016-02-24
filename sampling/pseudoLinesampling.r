#######################################################################
#
# Linesampling using a large amount of points on a transect of 
# adjustable length in random direction instead of continous line
#
# Mats Mahnken , 24.02.2016
#######################################################################

library("sp")
library("geosphere")

linesampling = function(x,y,dist,nr)
{
  # StartingPoint
  p=c(x,y)
  
  #Input number of points on line
  pl=1000
  
  #Bearing
  b=sample(0:180, 1)
  b180=b+180
  
  #initialize list liclust
  liclust = list()
  
  #for loop to create points along the line in both directions
  #from the center point of the cluster with (dist/(pl/2)) distance between each other;
  #note: no point in center point of cluster
  for (i in seq(1,(pl/2),1))
  {
    c1=destPoint(p,b,((dist/(pl/2))*i))
    liclust[[i]] = c1
  
    j=(pl/2)+i
    c2=destPoint(p,b180,((dist/(pl/2))*i))
    liclust[[j]] = c2
  }
  
  #merge destPoints from list to SpatialPoints
  clust = do.call("rbind",liclust)
  spcluster=SpatialPoints(clust)
  
  #add cluster number to spatial points
  spdf <- SpatialPointsDataFrame(spcluster, data.frame(Cluster =  c(rep(nr,pl))))
  
  # Assign projection (longlat +datum=WGS84)
  proj4string(spdf) <- CRS("+init=epsg:4326") 
  
  return(spdf)
}