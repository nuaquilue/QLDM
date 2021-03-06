#' Default model parameters
#'
#' Returns a list with the Quebec Landscape Dynamic Model parameters and the default values
#'
#' @return A list with the following items:
#'  \itemize{
#'    \item{\code{year.ini}: The inital year YYYY of the simulations}
#'    \item{\code{is.fuel.modifier}: A flag to indicate that fuels flammability modifies target burnt area}
#'    \item{\code{is.clima.modifier}: A flag to indicate that SEP (Spread Event day Probability) ratiomodifies target burnt area}
#'    \item{\code{th.small.fire}: A threshold (in pixels) that determines which fires are small vs. large to assign fuel categories. 
#'    When it is -1, all forest types burnt at high intensity}
#'    \item{\code{wflam}: Weight of species flammability in fire spread rate calculation}
#'    \item{\code{wwind}: Weight of wind factor in fire spread rate calculation}
#'    \item{\code{rpb}: Modifier of spread rate to get probability of burn}
#'    \item{\code{pb.upper.th}: When probability of burning >= pb.upper.th the grid cell always burns}
#'    \item{\code{ph.lower.th}: When probability of burning <= pb.lower.ht the grid cell never burns}
#'    \item{\code{target.old.pct}: Default target proportion of old (>= mature) forests to maintain inside management units}
#'    \item{\code{diff.prematurite}: Number of years before maturity for stands salvaged even if not mature}
#'    \item{\code{salvage.rate.event}: Maximal proportion of burnt mature forests that can be salvaged realistically 
#' in a given fire event [0,1]}
#'    \item{\code{salvage.rate.FMU}: Maximum proportion of salvaged burnt wood allowed in the timber supply in each FMU [0,1]}
#'    \item{\code{hor.plan}: Time horizon for timber supply calculations (by defaut, periods of 5 years, 5*24==120)}
#'    \item{\code{a.priori}: Proportion of AAC to harvest (between 0 and 1). Allows the constitution of a buffer 
#' for attenuation of natural disturbance impacts on timber supply fluctuations. Not yet implemented in the current version.}
#'    \item{\code{replanif}:  A flag to indicate that timber supply calculation (i.e. recalculation of AAC level) is done at each time step 
#' to readjust harvest level to consider changes in FMU age structure (caused by fire) (a posteriori approach).
#' If \code{FALSE}, AAC level it is calculated only once, durgin the first time step}
#'    \item{\code{timber.supply}: String to indicate the unit of timber supply calculation, 
#'    either \code{"area.based"} or \code{"volume.based"}}
#'    \item{\code{lutte}: A flag to indicate the systematic forestation of conifer communities that become broadleaves communities following a perturbation}
#'    \item{\code{enable.succ}: A flag to enable natural succession every 40 years (if FLASE, composition remains the same)}
#'    \item{\code{enfeuil}: After clear-cut, for a percentage of conifer species (EPN and SAB), transform to pionner species (PET)}
#'    \item{\code{age.seed}: Below this stand age, seed production is very low, and regeneration failures are more likely}
#'    \item{\code{p.failure}: Probability of regeneration failure in young (< 50 years) burned stands ([0,1])}
#'    \item{\code{suboptimal}: Tolerance for sub optimal conditions ([0,1])}
#'  }
#'    
#' @export
#' 
#' @examples
#' # Change the replanif parameter to not re-calculate sustainable timber supply levels every time step
#' params = default.params()
#' params$replanif = FALSE
#' 

default.params <- function(){
  return(list(
    ## GENERAL parameters:
    year.ini = 2020,
    
    ## WILDFIRE parameters:
    is.fuel.modifier = T,     
    is.clima.modifier = T,    
    th.small.fire = 50,  
    wflam = 0.7, 
    wwind = 0.3, 
    rpb = 0.4,
    pb.upper.th = 1, 
    pb.lower.th = 0, 
    
    ## FOREST MANAGEMENT parameters:
    target.old.pct = 0.2,    
    diff.prematurite = 0,   
    salvage.rate.event = 1,   
    salvage.rate.FMU = 1,    
    hor.plan = 24,           
    ## Replanning options facing natural disturbances
    a.priori = 1,  
    replanif = TRUE,
    timber.supply = "area.based",
    lutte = FALSE, 
    
    ## VEGETATION DYNAMICS parameters:
    enable.succ = TRUE,  
    enfeuil = 0.0,
    age.seed = 40,     
    p.failure = 0,     
    suboptimal = 0.5  
  ))
}