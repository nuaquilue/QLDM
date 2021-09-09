#' Stand volume
#'
#' Determine volume as a function of stand age and site index
#' 
#' @param land A \code{landscape} data frame with forest stand records in rows
#' 
#' @return A data frame with the volume per species per management unit
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' vol = stand.volume(landscape)
#' 

stand.volume <- function(land){
  
  ## Volum max according to ecological type
  land$vol.max <- substr(land$eco.type, 1, 2)
  land$vol.max <- ifelse(is.na(land$vol.max), "V250",
                      ifelse(land$vol.max=="RE", "V90",
                      ifelse(land$vol.max=="RS", "V150",
                      ifelse(land$vol.max=="FE", "V250", "V250"))))
  
  ## Maturity class
  land$matu[land$age.matu %in% c(40,45,50,60)] <- "M50"
  land$matu[land$age.matu %in% c(65,70,75,80)] <- "M70"
  land$matu[land$age.matu %in% c(85,90,95,100,105,110,120)] <- "M90"
  
  ## Volume per stand maturity class and site index
  land <- land %>% left_join(courbes, by = c("age","vol.max","matu"))
  
  ## Return volume per species per management unit
  return(vol.out=land$vol)
}
 