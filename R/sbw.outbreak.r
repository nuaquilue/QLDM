#' Srpuce budworm outbreak
#'
#' Simulates spruce budworm mortality according to a given probability of mortality after defoliation
#' 
#' @param land A \code{landscape} data frame with forest stand records in rows
#' @param severity The level of outbreak severity ([0,1])
#' 
#' @return A vector with the \code{cell.id} of the locations killed by the outbreak
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' kill.cells = sbw.outbreak(landscape)
#' 

sbw.outbreak<- function(land, severity=1){
  
  cat("  Spruce budworm outbreak", "\n" )
  
  # three climatic levels, determined from SDM: highly favorable (1), moderately favorable (0.5), unfavorable (0)
  sbw.clim  <- ifelse(land$temp> 0.5 & land$temp < 2.8, 1, ifelse(land$temp> -1.5 & land$temp < 4, 0.3, 0))
  # three age levels: old (1), young (0.5), regen (0)
  sbw.age  <- ifelse(land$age>60 & land$tsfire!=0, 1, ifelse(land$age>30 & land$tsfire!=0, 0.8, 0.6))
  # three compositions, defined in the highly favorable zone: fir (0.5), EPN (0.15), others (0)
  sbw.comp <- ifelse(land$spp == "SAB", 0.4, ifelse(land$spp =="EPN", 0.05, 0))  

  ## Proability of mortality
  prob <- sbw.clim * sbw.age * sbw.comp * severity
  land$kill <- prob > runif(length(prob))

  return(land$cell.id[land$kill==1])
  
}
