#' Fuel type
#'
#' Assign a fuel type and flammability to foret stands according to species and age
#'
#' @param land A \code{landscape} data frame with forest stand records in rows 
#'
#' @return A data frame with fuel type and flammability of forest stands
#' 
#' @export
#' 
#' @examples
#'
#' data(landscape)
#' fuel.type(landscape[runif(10,1,nrow(landscape)),])
#' 

fuel.type <- function(land){
  
  ## Fuel type according species and age
  fuel <- data.frame(cell.id=land$cell.id, frz=land$frz)
  fuel$type[land$spp %in% c("BOJ", "ERS", "NonFor", "OTH.FEU.S")] <- "low"
  fuel$type[land$spp %in% c("PET", "OTH.FEU.N")] <- "med" 
  fuel$type[land$spp %in% c("EPN", "SAB", "OTH.RES.S", "OTH.RES.N") & land$age<=40] <- "med"
  fuel$type[land$spp %in% c("EPN", "SAB", "OTH.RES.S", "OTH.RES.N") & land$age>40] <- "high"
  fuel$type <- as.factor(fuel$type)
  
  ## Assign to each cell, the fuel modifier
  fuel <- left_join(fuel, fuel.types.modif, by="type")
  
  return(fuel)
}