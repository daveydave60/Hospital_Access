#install.packages("gridExtra")

library(ggplot2)  # allows for fortify() func as well as other stuff
library(ggmap)  # allows for googles geocode() function
library(rgeos)  # needs to be installed before maptools for some reason
library(geosphere)  # used to find distance between two points with distm()
library(maptools)  # used for fortify() function
library(rgdal)   #allows for readOGR() functionality
library(dplyr)   #allows for left_join() func
library(stringr)   #allows for str_sub() func
library(scales)
library(gridExtra)   # allows for multiple graphs on one pane
library(maps)
#library(choroplethr)

#Only Run this read.csv once as it takes a while
HospList <- read.csv("./Assets/Hospitals.csv")

### user inputs ###
State <- 'MO'   #2-digit state abbrev ALL CAPS
CircleRadiusMiles <- 25
### End user inputs ###

StateLong <- tolower(state.name[state.abb == State])    #full state name ALL LOWERCASE
HospList_State <- HospList[HospList$STATE == State & HospList$STATUS == 'OPEN' & HospList$TYPE %in% c('GENERAL ACUTE CARE', 'CRITICAL ACCESS'),]

#func to create a circle for geom_path, corrected to not look like ellipse in latlon
plotCircle <- function(LonDec, LatDec, diameter) {#Corrected function
  #LatDec = latitude in decimal degrees of the center of the circle
  #LonDec = longitude in decimal degrees
  #diameter = diameter of the circle in miles
  radius <- diameter/2
  ER <- 3959 #Mean Earth radius in miles. Change this to 6371 and you will have your function working in km.
  AngDeg <- seq(1:360) #angles in degrees 
  Lat1Rad <- LatDec*(pi/180)#Latitude of the center of the circle in radians
  Lon1Rad <- LonDec*(pi/180)#Longitude of the center of the circle in radians
  AngRad <- AngDeg*(pi/180)#angles in radians
  Lat2Rad <-asin(sin(Lat1Rad)*cos(radius/ER)+cos(Lat1Rad)*sin(radius/ER)*cos(AngRad)) #Latitude of each point of the circle in radians
  Lon2Rad <- Lon1Rad+atan2(sin(AngRad)*sin(radius/ER)*cos(Lat1Rad),cos(radius/ER)-sin(Lat1Rad)*sin(Lat2Rad))#Longitude of each point of the circle in radians
  Lat2Deg <- Lat2Rad*(180/pi)#Latitude of each point of the circle convert to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  Lon2Deg <- Lon2Rad*(180/pi)#Longitude of each point of the circle convert to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  #polygon(Lon2Deg,Lat2Deg,lty=2)
  circlesDF <- data.frame(Lon2Deg, Lat2Deg)
  return(circlesDF)
}
###### end func

states <- map_data("county", StateLong, namefield="county")
states$coloring <- ifelse(states$subregion=="andrew","#cccccc","#ffffff")

p <- ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = states$coloring, color = 'black') +
  geom_point(data = HospList_State, aes(x = LONGITUDE, y = LATITUDE))
#  geom_polygon(data = pcirc, aes(Lon2Deg, Lat2Deg), color = NA, fill = 'red', alpha = 0.1)

#HospCircs <- NA #ggplot()
for (i in 1:NROW(HospList_State)){
 nam <- paste0('pcirc_',i)
 assign(nam, plotCircle(HospList_State$LONGITUDE[i], HospList_State$LATITUDE[i], CircleRadiusMiles*2))
 loop_input <- paste0("geom_polygon(data = ", nam, ", aes(Lon2Deg, Lat2Deg), color = NA, fill = 'red', alpha = 0.1)")
 p <- p + eval(parse(text = loop_input))
}

p <- p + coord_fixed(1.3)
print(p)
