#' Buffer migration
#'
#' Determines the presence of species within a buffer around target cells.
#' 
#' @param land A \code{landscape} data frame with forest stand records in rows
#' @param target.cell.ids A vector of \code{cell.id} codes for those cells open for colonization, 
#' and for which the presence of source populations nearby must be assessed
#'
#' @return A data frame with the \code{cell.id} of the target cells and the presence (TRUE) or 
#' absence (FALSE) of a sufficient number of source populations for each 
#' species: PET, BOJ, ERS, SAB, EPN, other, NonFor
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' buffer.mig(landscape, landscape$cell.id[runif(10,1,nrow(landscape))])
#' 

buffer.mig <- function(land, target.cell.ids){
  
  ## Coordinates of the target cells
  target.cell.ids.xy <- land[land$cell.id %in% target.cell.ids, c("cell.id", "x", "y")]
  
  ## Radius ~ max. colonization distance per species, and min number of soruces to enable colonization
  radius.buff <- spp.colonize.persist$rad
  nb.buff <- spp.colonize.persist$nneigh
    
  # Source cells (i.e. potential colonizers) per species. Minimal age of 50 years.
  micro.boj <- land[land$spp=="BOJ" & land$age>=50 & land$tscomp>=50,]
  micro.pet <- land[land$spp=="PET" & land$age>=50 & land$tscomp>=50,]
  micro.ers <- land[land$spp=="ERS" & land$age>=50 & land$tscomp>=50,]
  micro.epn <- land[land$spp=="EPN" & land$age>=50 & land$tscomp>=50,]
  micro.sab <- land[land$spp=="SAB" & land$age>=50 & land$tscomp>=50,]
  
  ### Calculate number of source populations in the neighbohood of each target cell. Colonization distances
  ### are species-specific.
  # PET
  list.cell.buff <- nn2(micro.pet[,c("x","y")], target.cell.ids.xy[,c("x","y")], 
                        k=nb.buff[4] , searchtype='priority')
  nn.dists.pet <- list.cell.buff$nn.dists[, nb.buff[4]] < radius.buff[4]
  
  # BOJ   
  list.cell.buff <- nn2(micro.boj[,c("x","y")], target.cell.ids.xy[,c("x","y")], 
                        k=nb.buff[1], searchtype='priority')
  nn.dists.boj <- list.cell.buff$nn.dists[,nb.buff[1]]< radius.buff[1]

  # ERS
  list.cell.buff <- nn2(micro.ers[,c("x","y")], target.cell.ids.xy[,c("x","y")], 
                        k=nb.buff[3], searchtype='priority')
  nn.dists.ers <- list.cell.buff$nn.dists[,nb.buff[3]]< radius.buff[3]
  
  # SAB   
  list.cell.buff <- nn2(micro.sab[,c("x","y")], target.cell.ids.xy[,c("x","y")], 
                        k=nb.buff[5],  searchtype='priority')
  nn.dists.sab <- list.cell.buff$nn.dists[,nb.buff[5]]< radius.buff[5]
  
  # EPN   
  list.cell.buff <- nn2(micro.epn[,c("x","y")], target.cell.ids.xy[,c("x","y")], 
                        k=nb.buff[2],  searchtype='priority')
  nn.dists.epn <- list.cell.buff$nn.dists[,nb.buff[2]]< radius.buff[2]
  
  # Build a data frame with the presence or absence of a sufficient number of
  # source populations of each species around each target cell. Currently set
  # at one in all scenarios, but could be modified.
  target.df <- data.frame(target.cell.ids.xy, PET=nn.dists.pet, BOJ=nn.dists.boj, ERS=nn.dists.ers, 
                          SAB=nn.dists.sab, EPN=nn.dists.epn, OTH=TRUE, NonFor=TRUE)
  target.df <- target.df[,-c(2,3)]  
  target.df <- reshape::melt(target.df,id=c("cell.id"))
  names(target.df)[-1] <- c("potential.spp", "press.buffer")
  target.df$potential.spp <- as.character(target.df$potential.spp)
  return(target.df)

  #############################   IT DOES THE SAME #############################
    # ## Now for each potential species, look if it can colonize each target cell according to 
    # ## the estimated colonization distance
    # for(ispp in 1:nrow(spp.colonize.persist)){
    #   ## Cells are potential colonizers if are at least 50 years old and time since last species composition change 
    #   ## is at least 50 years.
    #   colonizer <- filter(land, spp %in% spp.colonize.persist$spp[ispp], age>=50, tscomp>=50)
    #   ## First find the closest neighbors of all target cells. Look for enough neighbors to cover the maximum
    #   ## estimated maximum colonization distance (i.e. 75.000 m)
    #   neighs <- nn2(select(colonizer, x, y), target.cell.ids.xy[,-1], searchtype="priority", k=nrow(colonizer))  
    #   ## Now verify if the potential species are close enough of the target cells, the colonization distance is species-specific.
    #   aux <- apply(neighs$nn.dists <= spp.colonize.persist$rad[ispp], 1, sum) >= spp.colonize.persist$nneigh[ispp]
    #   buffer.spp <- rbind(buffer.spp, data.frame(target.cell.ids.xy, 
    #                       potential.spp=spp.colonize.persist$spp[ispp], press.buffer=aux))    
    # }
    # return(buffer.spp)
}

