#-----------------------------------------------------------------
# Example ForestCover Germany (cluster sampling)
#
# Info:
# Waldflaeche > 30.8 % | SE95 ±0.4 (Quelle: 3. BWI 2012)		
#
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Check if required packages are installed otherwise they wil be installed

packages <- c("raster", "plyr", "sp", "geosphere", "rgeos", "rgdal","maptools")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


#-----------------------------------------------------------------
#Include external R files and libraries

source("gsg_grid/create_GSG.r")
source("sampling/clustersampling.r")
source("io/import.r")

library(raster)
library(plyr)
#------------------------------------------------------------------
# Define variables

df=data.frame()
r=1

country_code="DEU"
adm_level="0"

pdist.list=c(seq(from=400, to=500, by=100))# in m
gdist.list=c(seq(from=50, to=60, by=10)) # in km

#Number of Runs
nr.runs=length(pdist.list)*length(gdist.list)

#Create a progress-bar
progress.bar <- create_progress_bar("text")
#------------------------------------------------------------------

#------------------------------------------------------------------
# Function for calculating forest cover including edge correction
# Number of points per cluster is included
calcforestcover<-function(data)
{
  sums <- aggregate(data$forestmap_germany, by=list(Cluster=data$Cluster), FUN=sum, na.rm=TRUE)
  rows <- data.frame(table(data$Cluster))
  Ratio <-sums[,2]/rows[,2]
  
  fc = sum(Ratio)/length(Ratio)
  se = sqrt(var(Ratio)/length(Ratio))
  
  myList = list("forestcover" = fc, "standarderror" = se)
  
  return(myList)
}
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# Import raster
# Forest map extracted from Corine 2012 Raster (100m resolution)
str_name = "case_studies/germany/forestmap_germany.tif" 
imported_raster=raster(str_name)

# Load boundary from GADM
boundary.geo <- loadBoundary(,"DEU","0")
#-------------------------------------------------------------------

progress.bar$init(nr.runs)

# Loop: Different combinations of GridDistance and Pointdistance
for (d in gdist.list) 
{
  # Genreate GSG
  GSG=generateGSG(distance=d,landpoly.geo=boundary.geo)
  
  
  for (l in pdist.list) 
  {
  
  # Extract Coordinates
  coords=as.data.frame(coordinates(GSG@points))

  # Apply ClusterSampling
  li_cluster = list() 
  
     for (i in 1:nrow(coords)) 
     {
     li_cluster[[i]] = clustersampling(coords[i,1], coords[i,2],l,i)
     }
  

  # Merge SpatialPoints from list
  SP=do.call("rbind", li_cluster)
  
  # Remove points outside the boundary
  SPclean=intersect(SP,boundary.geo)

  # Add grid values to points and estimate the forest cover for each cluster
  x=extract(imported_raster, SPclean, sp=TRUE)
  
  # Calculate forest cover in %)
  fcover=calcforestcover(x)
  
  # Writing the output of the loop into dataframe
  df[r,1]=d
  df[r,2]=l
  df[r,3]=fcover$forestcover
  df[r,4]=fcover$standarderror
  
  # counts each run
  r=r+1
  
  progress.bar$step()
  }
}


colnames(df)<-c("GridDistance","PointDistance","ForestCover","StandardError")

#---------------------------------------------------------------------







