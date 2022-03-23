#' @title GPX file to sf object
#' @description Given a GPX file return an sf points object
#' @param path a file path to a gpx file
#' @return an sf points object with all gpx data attchaed
#' @export
#' @importFrom XML htmlTreeParse xpathSApply xmlValue xmlAttrs
#' @importFrom  sf st_as_sf
#'
gpx_to_pts = function(path){
  
  proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  options(digits=10)
  
  pfile <- htmlTreeParse(file = path,
                         error = function(...) {},
                         useInternalNodes = T)
  
  elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
  times      <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
  coords     <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])
  times = t = as.POSIXct(gsub(".000Z", " ", gsub("T", " ", times)))
  
  geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = proj)
  geodf
}