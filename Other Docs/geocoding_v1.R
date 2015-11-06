# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#                                                 GeoCoding
# --------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

library(geosphere)
library(RJSONIO)
# API to return details about the address
googleGeocodeUrl <- 'http://maps.google.com/maps/api/geocode/xml?sensor=false&address='

# --------------------------------------------------------------------------------------------------------------
#                         Function to call the API and retrieve lat and long from response  
# --------------------------------------------------------------------------------------------------------------

getDocNodeVal=function(doc, path)
{
  sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}

gGeoCode=function(str)
{
  library(XML)
  u=paste(googleGeocodeUrl,str)
  doc = xmlTreeParse(u, useInternal=TRUE)
  str=gsub(' ','%20',str)
  lat=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lat")
  lng=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lng")
  list(lat = lat, lng = lng)
}

LAT_LONG_1 <- gGeoCode("majestic, Bangalore")
LAT_LONG_2 <- gGeoCode("manyata tech park, Bangalore")


# --------------------------------------------------------------------------------------------------------------
#             Function that contain the algorithm to calculate the displacement between two co-ordinates
# --------------------------------------------------------------------------------------------------------------

distm(as.numeric(paste(LAT_LONG_1)),as.numeric(paste(LAT_LONG_2)),fun=distHaversine)

                                          #   OR   #  

distHaversine(as.numeric(paste(LAT_LONG_1)), as.numeric(paste(LAT_LONG_2)), r=6378137)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
# ------------------- END --------------------- GeoCoding --------------------------- END ----------------------
# --------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
