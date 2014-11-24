#GeoNames FUN! 
 
#load the package
#install.packages("geonames)
#go to http://www.geonames.org/ and create an account, then verify through email. 
#then go to http://www.GeoNames.org/manageaccount and make sure that it is set up. 
 
 
library(geonames)
 
zip_weather <- function(postalcode){
  options(warn=-1)
  zip = GNfindNearbyPostalCodes(postalcode=postalcode,country='US', maxRows=1)
  weather = GNfindNearByWeather(zip$lat,zip$lng)
  tempF = as.numeric(weather$temperature) * 9/5 + 32
  line1 = paste(weather$datetime, "GMT")
  line2 = paste("Weather for", weather$stationName)
  line3 = paste("temp = ",tempF, "deg. F with", weather$clouds, ", wind at", weather$windSpeed, "knots")
  return(c(line1, line2, line3))
  options(warn=0)
}
zip_weather(98155)

zip_earthquakes <- function(postalcode){
}

zip <- GNfindNearbyPostalCodes(postalcode="98155",country='US', maxRows=1)

GNwikipediaSearch("london")


GNfindNearbyStreets(zip$lat, zip$lng)

GNgtopo30(zip$lat, zip$lng)
GNneighbourhood(zip$lat, zip$lng)
GNsrtm3(zip$lat, zip$lng)

#bounding box 
#would be nice to have a function that bounds a zip code or whatevs
GNwikipediaBoundingBox(north=44.1,south=-9.9,east=-22.4,west=55.2)
GNearthquakes()


#neat geo coding
url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}
  
geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
  lat <- x$results[[1]]$geometry$location$lat
  lng <- x$results[[1]]$geometry$location$lng
  location_type <- x$results[[1]]$geometry$location_type
  formatted_address <- x$results[[1]]$formatted_address
  return(c(lat, lng, location_type, formatted_address))
  } else {
  return(c(NA,NA,NA, NA))
  }
}
address <- geoCode("The White House, Washington, DC")














