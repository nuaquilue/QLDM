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

stand.volume <- function(land.mgmt){
  
  ## Volum max according to ecological type
  land.mgmt$vol.max <- substr(land.mgmt$eco.type, 1, 2)
  land.mgmt$vol.max <- ifelse(is.na(land.mgmt$vol.max), "V250",
                       ifelse(land.mgmt$vol.max=="RE", "V90",
                       ifelse(land.mgmt$vol.max=="RS", "V150",
                       ifelse(land.mgmt$vol.max=="FE", "V250", "V250"))))
  
  ## Maturity class
  land.mgmt$matu[land.mgmt$age.matu %in% c(40,45,50,60)] <- "M50"
  land.mgmt$matu[land.mgmt$age.matu %in% c(65,70,75,80)] <- "M70"
  land.mgmt$matu[land.mgmt$age.matu %in% c(85,90,95,100,105,110,120)] <- "M90"
  
  ## Courbes only computed up to age=150
  land.mgmt$age <- pmin(land.mgmt$age, 150)
  
  ## Volume per stand maturity class and site index
  land.mgmt <- land.mgmt %>% left_join(courbes, by = c("age","vol.max","matu"))
  
  ## Return volume per species per management unit
  return(vol.out=land.mgmt$vol)
}
 