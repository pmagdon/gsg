#############################################################################
#                            GLOBAL SAMPLING GRID  (GSG)                    #
#############################################################################
#by Lutz Fehrmann and Nils Noelke

#----------------------------------------------------------------------------
library("sp")
library("rgeos")
library("rgdal")
#-----------------------------------------------------------------------------
setClass("GSG",
         representation(
           points="SpatialPoints")
         )


generateGSG = function(distance, landpoly.geo) 
{
  
#Extract BBOX from Polygon
aoi = c(bbox(landpoly.geo)[2], bbox(landpoly.geo)[4], bbox(landpoly.geo)[1], bbox(landpoly.geo)[3])
 

#---------------------------------------------------------------------------
# Calculate grid coordinates 
# The grid is systematic (equidistant points along cicles of latitude)
# on a spheroid (WGS84/Pseudo-Mercator, epsg:3857)
# Calculate the angle of an d-arc in degree
# Set distance between points along latitudes in km

deg <- (distance/(pi * 6378.137)) * 180

# Create a vector of longitue along equator
longitude <- sort(c(seq(0, -180, -deg), seq(deg, 180, deg)))

# Create a vector of latitudes in aoi
latitudes <- longitude[longitude >= aoi[1] & longitude <= aoi[2]]

# Calculate a matrix of longitudes for each circle of latitude
longitudes <- outer(longitude, latitudes, function(x,y) x/cos(pi/180 * y))

# Compile coordinates 
coordinates<-cbind(as.vector(t(longitudes)), latitudes)
colnames(coordinates)<-c("X","Y")

# Subset coordinates in aoi
coordinates<-coordinates[coordinates[,1] >= aoi[3] & coordinates[,1] <= aoi[4],]

# To SpatialPointDataFrame
GSGsp <- SpatialPointsDataFrame(cbind(coordinates[,1], coordinates[,2]), 
                                data = data.frame(1:nrow(coordinates)))
# Assign projection (longlat +datum=WGS84)
proj4string(GSGsp) <- CRS("+init=epsg:4326") 

# Subset gridpoints falling on land (or in a specific country)
landpoly.pts=gIntersection(GSGsp,landpoly.geo)

return(new('GSG', 
           points=landpoly.pts)
       )
}
