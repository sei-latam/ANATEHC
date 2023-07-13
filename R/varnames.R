#' Title
#'
#' @param check 
#'
#' @return
#' @export
#'
#' @examples
#' 
var.listSEI <- function(check = TRUE){
  extent.name <- c("Precipitacion diaria", "Caudal diario", "Temperatura diaria", "Velocidad media del viento", "Evaporacion diaria")
  short.name <- c("PT_4", "QL_1", "TS_1", "VD_1", "EV_4")
  units.var <- c("mm d-1", "mcs", "grado C", "m s-1", "mm d-1")
  DF.var <- data.frame(var.name = extent.name, short.name = short.name, unit = units.var)
  return(DF.var)    
}

