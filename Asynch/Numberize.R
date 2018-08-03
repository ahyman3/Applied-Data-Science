Numberize <- function(col){
  col <- gsub(",", "", col)
  col <- gsub(" ", "", col)
  col <- as.numeric(col)
  return(col)
}

MakeGeoUrl <- function(address){
  root <- "http://maps.googleapis.com/maps/api/geocode/"
  address <- paste(root, "json?address=", address, "&sensor=FALSE", sep = "")
  return(URLencode(address))
}

Addr2latlng <- function(address){
  urlAddress <- MakeGeoUrl(address)
  apiResult <- getURL(urlAddress)
  geoStruct <- fromJSON(apiResult, simplify = FALSE)
  lat <- NA
  lng <- NA
  try(lat <- geoStruct$results[[1]]$geometry$location$lat)
  try(lng <- geoStruct$results[[1]]$geometry$location$lng)
  return(c(lat, lng))
}