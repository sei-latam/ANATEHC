#' Title
#'
#' @param user 
#' @param variable 
#' @param station.cod 
#' @param use.map 
#' @param area 
#' @param View.plot 
#' @param buffer 
#'
#' @return
#' @return
#' @export
#'
#' @examples
#' #shasghagsjagja
extracDATA <- function(user = "SEI", variable = "Precipitacion diaria", station.cod = "21010320", 
                       use.map = TRUE, area = NA, View.plot = TRUE, buffer = TRUE, 
                       dist.buffer = 800){
  
  if(user == "SEI"){
    warning("You must have access of SEI Google account")
    variable <- tolower(variable)
    varlist <- var.listSEI()
    
    if(is.element(variable, tolower(varlist[ ,1]))){
      id.folder <- which(variable == tolower(varlist[ ,1]))
      var.stations <- drive_ls(paste0("~/IDEAM_CLIMA/VARIABLES/", varlist[id.folder ,2]))
      
      if(use.map){
        cne.stations <- read_sheet(ss = "1nSD-oHGYT29kkz2hdx-7DP2bJTDtMvIetWBCkfFE7Ks", sheet = 1)
        point.cne.stations <- sf::st_as_sf(cne.stations, coords = c("longitud","latitud"))
        TRANS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
        point.cne.stations <- sf::st_set_crs(point.cne.stations, TRANS) 
        
        if(buffer){
          area.temp = sf::st_transform(area, 3857)
          buffer.area <- sf::st_buffer(area.temp, dist = dist.buffer)
          point.cne.stations.temp <- sf::st_transform(point.cne.stations, 3857)
          id.station<- sf::st_intersects(buffer.area, point.cne.stations.temp)
          station.area <- point.cne.stations[unlist(id.station),]
          buffer.area <- sf::st_transform(buffer.area, TRANS)
          
          if(View.plot){
            tmap::tmap_mode("view")
            tmap::tm_basemap(leaflet::providers$Stamen.Terrain)+
              tmap::tm_shape(area$geometry, name = "Study area")+
              tmap::tm_borders(col= "cyan", lwd=1.5)+
              tmap::tm_shape(station.area, name = "Stations")+
              tmap::tm_dots(size = 0.1, legend.show = F, col = "darkorange", border.col = "darkorange")
          }
          
        } else {
          area.temp = sf::st_transform(area, 3857)
          point.cne.stations.temp <- sf::st_transform(point.cne.stations, 3857)
          id.station<- sf::st_intersects(area.temp, point.cne.stations.temp)
          station.area <- point.cne.stations[id.station,]
        }
      }
      
    }                          
  } else {
    stop("You can use other functions analysis from ANATECH package")
  }
}

