library("sp")
library("geosphere")

linesampling = function(x,y,dist)
{
# StartingPoint
p=c(x,y)

#Bearing
b=sample(0:180, 1)
b180=b+180

c1=destPoint(p,b,dist)
c2=destPoint(p,b180,dist)

c1c2 = rbind(c1,c2)
L1=Line(c1c2)


return(L1)
}