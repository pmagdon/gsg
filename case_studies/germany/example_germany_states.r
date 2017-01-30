#-----------------------------------------------------------------
# Example ForestCover Germany by State (cluster sampling)
#
# Info:
# Waldflaeche > 30.8 % | SE95 ±0.4 (Quelle: 3. BWI 2012)  	
#
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Check if required packages are installed otherwise they wil be installed

packages <- c("raster", "plyr", "sp", "geosphere", "rgeos", "rgdal")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Set Working Directory to Source File Location
setwd("")

#-----------------------------------------------------------------
#Include external R files and libraries

source("createGSG.r")
source("clustersampling_4c.r")

library(raster)
library(plyr)
#------------------------------------------------------------------
# Define variables

df=data.frame()
r=1

country_code="DEU"
adm_level="1"

# define GSG size and cluster point distance
pdist.list=c(seq(from=25, to=250, by=25))# in m
gdist.list=c(seq(from=4, to=4, by=4)) # in km ---- not larger than 16km ----

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
  Ratio <-sums[,2]/4 #rows[,2]
  
  fc = sum(Ratio)/length(Ratio)
  se = sqrt(var(Ratio)/length(Ratio))
  
  myList = list("forestcover" = fc, "standarderror" = se)
  
  return(myList)
}
#-------------------------------------------------------------------

#-------------------------------------------------------------------
# Import raster
# Forest map extracted from Corine 2012 Raster (100m resolution)
str_name = "forestmap_germany.tif" 
imported_raster=raster(str_name)

# Load boundary from GADM
gadm_url = load(url(paste("http://biogeo.ucdavis.edu/data/gadm2.8/rds/",country_code,"_adm",adm_level,".rds", sep="")))
landpoly = gadm
boundary.geo <- spTransform(landpoly, CRS("+init=epsg:4326"))
#-------------------------------------------------------------------

progress.bar$init(nr.runs)

# Loop: Different combinations of GridDistance and PointDistance per State
for (ID_1 in boundary.geo$ID_1)
{
  landpoly.geo <- boundary.geo[which(boundary.geo$ID_1==ID_1),]
  
  for (d in gdist.list) 
  {
    for (l in pdist.list) 
    {
      # Generate GSG
      GSG=generateGSG(distance=d,landpoly.geo=landpoly.geo)
      
      # Extract Coordinates
      coords=as.data.frame(coordinates(GSG@points))
      
      # Apply clustersampling_4c
      li_cluster = list() 
      
      for (i in 1:nrow(coords)) 
      {
        li_cluster[[i]] = clustersampling(coords[i,1], coords[i,2],l,i)
      }
      
      # Merge SpatialPoints from list
      SP=do.call("rbind", li_cluster)
      
      # Remove points outside the boundary
      SPclean=intersect(SP,landpoly.geo)
      
      # Add grid values to points and estimate the forest cover for each cluster
      x=extract(imported_raster, SPclean, sp=TRUE)
      
      # Calculate forest cover in %
      fcover=calcforestcover(x)
      
      # Writing the output of the loop into dataframe
      df[r,1]=landpoly.geo$NAME_1
      df[r,2]=d
      df[r,3]=l
      df[r,4]=fcover$forestcover
      df[r,5]=fcover$standarderror
      df[r,6]=(fcover$standarderror)/(fcover$forestcover)
      
      # counts each run
      r=r+1
    }
  }
progress.bar$step()
}

colnames(df)<-c("Bundesland","GridDistance","PointDistance","ForestCover","StandardError","relStandardError")

#---------------------------------------------------------------------