########################################
## Import module
## Usage:
## loadBoundary(data.shp,,) for own shapefile
## loadBoundary(,"DEU","0")  for GADM
########################################
library(maptools)

loadBoundary = function (shapefile,country_code,adm_level){

  if(missing(shapefile)){
   # Load boundary from GADM
   gadm_url = load(url(paste("http://biogeo.ucdavis.edu/data/gadm2/R/",country_code,"_adm",adm_level,".RData", sep="")))
   landpoly = gadm
   boundary.geo = spTransform(landpoly, CRS("+init=epsg:4326"))
   return (boundary.geo)
  }
  else
  {
    # Load boundary from own Shapefile
   shapes = readShapeSpatial(shapefile)
   # Assign projection (longlat +datum=WGS84)
   proj4string(shapes) <- CRS("+init=epsg:4326") 
   
   return (shapes)
  }
}
