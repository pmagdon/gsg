library("sp")
library("geosphere")

clustersampling = function(x,y,dist,nr)
{
# StartingPoint
p=c(x,y)

# Bearing
b=c(seq(0,360, 45))

# Distance

distdiag=sqrt(dist^2+dist^2)

c1=destPoint(p,b[1],dist)
c2=destPoint(p,b[3],dist)
c3=destPoint(p,b[5],dist)
c4=destPoint(p,b[7],dist)
c5=destPoint(p,b[2],distdiag)
c6=destPoint(p,b[4],distdiag)
c7=destPoint(p,b[6],distdiag)
c8=destPoint(p,b[8],distdiag)

clust=rbind(c1,c2,c3,c4,c5,c6,c7,c8)

spcluster=SpatialPoints(clust)

spdf <- SpatialPointsDataFrame(spcluster, data.frame(Cluster =  c(rep(nr,8))))

# Assign projection (longlat +datum=WGS84)
proj4string(spdf) <- CRS("+init=epsg:4326") 


return(spdf)
}