#' Quebec Landscape Dynamic Model
#'
#' Run the landscape dynamic model QLDM that includes the processes of wilfires, clear-cuts,
#' partial-cuts, spruce budworm outbreaks, post-disturbance regeneration, and forest seral succession.
#'
#' @param is.widlfire A flag to indicate that wildfires are a simualted process of the model
#' @param is.sbw A flag to indicate that spruce budworm outbreaks are a simulated process of the model
#' @param is.clearcut A flag to indicate that harvesting by clear cuts is a simulated process of the model
#' @param is.partialcut A flag to indicate that harvesting by partical cuts is a simulated process of the model
#' @param custom.params List with the model paramaters and default and/or user-defined values 
#' @param rcp Climate projection, either \code{NA} (default), 'rcp45' or 'rcp85' 
#' @param prec.proj Data frame with annual precipitation projections for each time step 
#' @param temp.proj Data frame with mean temperature projections for each time step 
#' @param timber.supply Approach to do timber supply calculation, either \code{area.based} or \code{volume.based}
#' @param pigni Optional data frame with probability of fire igntion per grid cell
#' @param nrun Number of replicates to run the model
#' @param time.step Number of years of each time step
#' @param time.horizon Number of years of the model simulation, it has to be a multiple \code{time.step}
#' @param save.land A flag to save as a RDS file the \code{landscape} data frame at the time step indicated in \code{out.seq}
#' @param out.seq Numeric vector with the time steps the \code{landscape} is saved
#' @param out.path String with the directory path to save the \code{landscape} data frame at each time step indicated in \code{out.seq}
#'
#' @return A list with the following items:
#'  \itemize{
#'    \item{\code{SppByAgeClass}: A data frame of species abundance per age class, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{mgmt.unit}: Code of the forest management unit (FMU).}
#'         \item{\code{spp}: Code of the species.}
#'         \item{\code{age.class}: Code of the age class, \code{C10}, \code{C30}, \code{C50}, \code{C70}, \code{C90}, and \code{OLD}.}
#'         \item{\code{area}: Area in km2.}
#'       }
#'    }
#'    \item{\code{SuitabilityClasses}: A data frame of suitability of potential species per bioclimatic domain, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{bioclim.domain}: Code of the bioclimatic domain.}
#'         \item{\code{spp}: Code of the species.}
#'         \item{\code{poor}: Area of poor environmental suitability.}
#'         \item{\code{med}: Area of intermediate environmental suitability.}
#'         \item{\code{good}: Area of good environmental suitability.}
#'       }
#'    }
#'    \item{\code{SppByFireZone}: A data frame of species abundance per fire zone, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{spp}: Code of the species.}
#'         \item{\code{area}: Area in km2.}
#'       }
#'    }
#'    \item{\code{FuelByFireZone}: A data frame of fuel type per fire zone, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{type}: Code of the fuel type:  \code{low}, \code{med} or \code{high}.}
#'         \item{\code{pct}: Relative abundance of the fuel type in the fire regime zone ([0,1]).}
#'       }
#'    }
#'    \item{\code{Cuts}: A data frame of harvestable area and volume per management unit, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{mgmt.unit}: Code of the forest management unit (FMU).}
#'         \item{\code{tot.inc}: Code of the species.}
#'         \item{\code{even.age}: Code .}
#'         \item{\code{a.mat}: Area in km2.}
#'         \item{\code{a.inc.burnt}: Area in km2.}
#'         \item{\code{a.inc.mat.burnt}: Area in km2.}
#'         \item{\code{a.inc.kill}: Area in km2.}
#'         \item{\code{a.inc.mat.kill}: Area in km2.}
#'         \item{\code{a.reg.fail.ex}: Area in km2.}
#'         \item{\code{a.reg.fail.in}: Area in km2.}
#'         \item{\code{a.salvaged}: Area in km2.}
#'         \item{\code{a.unaff}: Area in km2.}
#'         \item{\code{v.salv}: Volume in m3.}
#'         \item{\code{v.unaff}: Volume in m3.}
#'         \item{\code{a.pcut}: Area partial cut in km2.}
#'         \item{\code{v.pcut}: Volume partial cut in m3.}
#'       }
#'    }
#'    \item{\code{SppCut}: A data frame of area and volum extracted by clear and partial cut per species and management unit, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{mgmt.unit}: Code of the forest management unit (FMU).}
#'         \item{\code{spp}: Code of the species.}
#'         \item{\code{area.ccut}: Area clear cut in km2.}
#'         \item{\code{vol.ccut}: Volume clear cut in m3.}
#'         \item{\code{area.pcut}: Area partial cut in km2.}
#'         \item{\code{vol.pcut}: Volume partial cut in km2.}
#'       }
#'    }
#'    \item{\code{BurntRates}: A data frame of target area to be burnt per fire regime zone 
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{br}: Code of the species.}
#'         \item{\code{brvar}: Code .}
#'         \item{\code{brfuel}: Area in km2.}
#'         \item{\code{brclima}: Area in km2.}
#'         \item{\code{target.area}: Area in km2.}
#'       }
#'    }
#'    \item{\code{FireRegime}: A data frame of number of fires and burnt area per fire regime zone
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{target.area}: Target area to be burnt in km2.}
#'         \item{\code{nfires}: N.}
#'         \item{\code{burnt.area}: Area in km2.}
#'         \item{\code{fire.cycle}: Area in km2.}
#'         \item{\code{indx.combust}: Area in km2.}
#'         \item{\code{indx.combust.burnt}: Area in km2.}
#'       }
#'    }
#'    \item{\code{Fires}: A data frame of wildfires
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{fire.id}: Wildfire identificator.}
#'         \item{\code{wind}: Main wind direction in degrees.}
#'         \item{\code{target.size}: Target size in pixels to be burnt.}
#'         \item{\code{burnt.size}: Burnt size in pixels.}
#'         \item{\code{rem}: Remanent number of pixels to be burnt.}
#'       }
#'    }
#'    \item{\code{BurntFuels}: A data frame of burnt fuel types per fire regime zone
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{type}: Code of the fuel type:  \code{low}, \code{med} or \code{high}.}
#'         \item{\code{area}: Area burnt in km2.}
#'       }
#'    }
#'  }
#'  
#' @export
#' 
#' @examples
#'
#' \dontrun{
#' library(QLDM)
#' data(prec)
#' data(temp)
#' # Run one single 80-year replicate with forest management and default RCP 4.5 climate projections 
#' quebec.ldm(is.clearcut = T, is.partialcut = T, rcp = "rcp45")
#' }
#'


quebec.ldm <- function(is.wildfires = FALSE, is.sbw = FALSE, is.clearcut = FALSE, is.partialcut = FALSE, 
                       custom.params = NA, rcp = NA, prec.proj = NA, temp.proj = NA, timber.supply = "area.based",
                       pigni = NA, nrun = 1, time.step = 5, time.horizon = 80,  
                       save.land = FALSE, out.seq = NA, out.path = NA, ...){

  options(dplyr.summarise.inform=F)
  
  cat("A. Data preparation ...\n")
    
  ## Build the discrete time sequence according to time.step
  ## 0  5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85  # MATHIEU
  ## 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80   # N??RIA
  ## lenght(time.seq) is 16
  time.seq <- seq(time.step, time.horizon, time.step) 
  ## Processes recurrence (in years) 
  fire.step <- cc.step <- pc.step <- time.step
  ## Set the scheduling of the processes
  fire.schedule <- seq(time.step, time.horizon, fire.step)
  cc.schedule <- seq(time.step, time.horizon, cc.step)
  pc.schedule <- seq(time.step, time.horizon, pc.step)
  sbw.schedule <- c(5,35,70)
  if(save.land){
    if(is.na(out.seq)){
      out.seq <- time.seq
    } 
    else{
      if(!all(out.seq %in% time.seq)){
        warning("Not all time steps in the output sequence provided are simulated.", call.=F)
      }
    }
    if(is.na(out.path)) stop("Directory path to save outputs not provided")
  }
  
  ## Compute cell resolution in km2
  km2.pixel <- raster::res(mask)[1] * raster::res(mask)[2] / 10^6
  
  ## Get the list of default parameters and update user-initialized parameters
  params <- default.params()
  if(!is.na(custom.params)){
    # Check class of custom.params
    if((!inherits(customParams, "list"))) {
      stop("'custom.params' must be a named list")
    }
    ## Check that the names of the customized parameters are correct
    if(!all(names(custom.params) %in% names(params)))
      stop("Wrong custom parameters names")
    params <- custom.param
  }
    
  
  ## If provided by the user, load temperature and precipitation predictions for the whole study area
  ## Check that all time steps are included and columns names is ok
  prec.chg = temp.chg = F
  if(!is.na(prec.proj)){
    # Check class of prec.proj
    if((!inherits(prec.proj, "data.frame"))) {
      stop("'prec.proj' must be a named data frame")
    }
    ## Check that the names of the customized parameters are correct
    if(colnames(prec.proj)[1] != "cell.id" | ncol(prec.proj) != (length(time.seq)+1) ) 
      stop("Format of the precipitation projections data frame is not correct.")
    prec.chg = T
  } 
  if(!is.na(temp.proj)){
    # Check class of prec.proj
    if((!inherits(temp.proj, "data.frame"))) {
      stop("'temp.proj' must be a named data frame")
    }
    if(colnames(temp.proj)[1] != "cell.id" | ncol(temp.proj) != (length(time.seq)+1) ) 
      stop("Format of the temperature projections data frame is not correct.")
    temp.chg = T
  }
  ## Load precipitation and temperature projections provided with the package according to the climatic scenario.
  if(is.na(prec.proj) & !is.na(rcp)){
    prec.proj = get(paste0("prec_", rcp))
    prec.chg = T
  }
  if(is.na(temp.proj) & !is.na(rcp)){   
    temp.proj = get(paste0("temp_", rcp))
    temp.chg = T
  }
  
  
  ## Initialize tracking data.frames 
  breaks <- c(0,20,40,60,80,100,999)
  tags <- c("C10","C30", "C50", "C70", "C90", "OLD")
  track.spp.firezone <- data.frame(run=NA, year=NA, frz=NA, spp=NA, area=NA)
  track.spp.age.class <- data.frame(run=NA, year=NA, mgmt.unit=NA, spp=NA, age.class=NA, area=NA)
  track.suit.class <- data.frame(run=NA, year=NA, bioclim.domain=NA, potential.spp=NA, poor=NA, med=NA, good=NA)
  track.fire.regime <- data.frame(run=NA, year=NA, frz=NA,  target.area=NA, nfires=NA, burnt.area=NA, 
                                  fire.cycle=NA, indx.combust=NA, indx.combust.burnt=NA)
  track.fires <- data.frame(run=NA, year=NA, frz=NA, fire.id=NA, wind=NA, target.size=NA, burnt.size=NA)
  track.target <- data.frame(run=NA, year=NA, frz=NA, br=NA, brvar=NA, brfuel=NA, brclima=NA, target.area=NA)
  track.fuel <- data.frame(run=NA, year=NA, frz=NA, type=NA, pct=NA)
  # track.sprd <- data.frame(run=NA, year=NA, frz=NA, fire.id=NA, cell.id=NA, step=NA, flam=NA, wind=NA, sr=NA, pb=NA, burn=NA)
  track.burnt.fuel <- data.frame(run=NA, year=NA, frz=NA, type=NA, area=NA)
  track.cut <- data.frame(run=NA, year=NA, mgmt.unit=NA, tot.inc=NA, even.age=NA, a.mat=NA, a.inc.burnt=NA, 
                           a.inc.mat.burnt=NA, a.inc.kill=NA, a.inc.mat.kill=NA, a.reg.fail.ex=NA, a.reg.fail.in=NA,
                           a.salvaged=NA, a.unaff=NA, v.salv=NA, v.unaff=NA, a.pcut=NA, v.pcut=NA)
  track.spp.cut <- data.frame(run=NA, year=NA, mgmt.unit=NA, spp=NA, area.ccut=NA, vol.ccut=NA,
                              area.pcut=NA, vol.pcut=NA)
  
  ## Start the simulations
  cat("\n") 
  cat(paste("B. Simulations ...\n"))
  
  for(irun in 1:nrun){
    
    ## Main landscape data frame 
    land <- landscape

    ## Upper truncate mean temperature of Black spruce, Yellow birch and Balsam fir
    land[land$spp == "EPN" & land$temp>= 2.4,]$temp <- 2.4
    land[land$spp == "BOJ" & land$temp>= 4.6,]$temp <- 4.6
    land[land$spp == "SAB" & land$temp>= 5.0,]$temp <- 5.0   
    
    ## Matrix to save the sustained yield level at time t = 0, after clear.cut has happeened
    ## It will be used when the option "replanif" is not activated, 
    ## so recalculation of AAC level is only calculated once, during the first period
    ref.harv.level <- table(land$mgmt.unit)*NA
    
    ## Determine the harvest regime for each species:
    ## even aged (1): 95% all conifers, PET, DB
    ## uneven aged (0): 95% deciduous
    ## other (2)
    land <- mutate(land, rndm=runif(nrow(land)))
    land$even <- 2
    land$even[land$spp %in% c("EPN", "PET", "SAB", "OTH.RES.N", "OTH.RES.S", "OTH.FEU.N") & is.na(land$exclus) & land$rndm<=0.95] <- 1
    land$even[land$spp %in% c("EPN", "PET", "SAB", "OTH.RES.N", "OTH.RES.S", "OTH.FEU.N") & is.na(land$exclus) & land$rndm>0.95] <- 0
    land$even[land$spp %in% c("BOJ", "ERS", "OTH.FEU.S") & is.na(land$exclus) & land$rndm>0.95] <- 1
    land$even[land$spp %in% c("BOJ", "ERS", "OTH.FEU.S") & is.na(land$exclus) & land$rndm<=0.95] <- 0    

    ## Compute the baseline fuel at the fire zone level
    fuels <- fuel.type(land)
    baseline.fuel <- group_by(fuels, frz) %>% summarise(x=mean(flam))
    
    ## Record the area to not burnt the following periods if already burnt during the current time step
    mem.feux <- rep(0, length(unique(land$frz)))
    
    ## Record initial distributions:
    ## Species distribution per fire zone
    track.spp.firezone <- rbind(track.spp.firezone, 
      data.frame(run=irun, year=params$year.ini, group_by(land, frz, spp) %>% summarise(area=length(cell.id)*km2.pixel)))
    ## Fuel type distribution per fire zone
    zone.size <- group_by(land, frz) %>% summarise(x=length(frz))
    aux <- group_by(fuels, frz, type) %>% summarise(n=length(frz)) %>% 
           left_join(zone.size, by="frz") %>% mutate(pct=n/x) %>% select(-n, -x)
    track.fuel <- rbind(track.fuel, data.frame(run=irun, year=params$year.ini, aux))
    ## Age classes distribution per species and management unit
    land$age.class <- cut(land$age, breaks=breaks, include.lowest=TRUE, right=TRUE, labels=tags)
    track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=params$year.ini, 
                           filter(land, spp!="NonFor") %>% group_by(mgmt.unit, spp) %>% count(age.class) 
                           %>% mutate(area=n*km2.pixel)) %>% select(-n))
    ## Suitability classes distribution per bioclim.domain 
    suitab <- suitability(land, params) 
    aux <- left_join(suitab, select(land, cell.id, bioclim.domain), by="cell.id") %>%
           group_by(bioclim.domain, potential.spp) %>% 
           summarise(poor=sum(suit.clim==0)*km2.pixel, med=sum(suit.clim==0.5)*km2.pixel, good=sum(suit.clim==1)*km2.pixel) 
    track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=params$year.ini, aux))
    rm(suitab); rm(aux)
    
    ### coupe: vecteur a priori
    if(params$a.priori < 1){
      ## need seuils.a.priori.txt
      a.priori.v <- as.data.frame(cbind(as.character(unique(land$mgmt.unit)),a.priori))
      apri.par.ua <- read.table("outputs/seuils.a.priori.txt", header = TRUE,  sep="\t")
      apri.par.ua2 <- apri.par.ua[apri.par.ua$sc.no==scn.art-2,]
      a.priori.v <- merge(a.priori.v, apri.par.ua2, by.x = "V1", 
            by.y = "list.UA", all.x = TRUE, all.y = T)
      a.priori.v$new <- 1-a.priori.v$baisse.ua
      a.priori.v$new[is.na(a.priori.v$new)] <- 1
      a.priori.v <- a.priori.v[,c(1,6)]
    }

    
    ## Simulation of one time step
    for(t in time.seq){
      
      ## Print replicate and time step
      cat(paste0("Replicate ", irun, "/", nrun,". Time step: ", params$year.ini+t-time.step, "-", t+params$year.ini, "\n"))
      
      ## Update climatic variables at each time step if climate change is activated
      ## Column 1 is cell.index, the following columns are temp (precip) in 
      ## 2020-2025, 2025-2030, 2030-2035, ... etc.
      ## The last column (temp95) then corresponds to the period 2095-2100
      ## The first time step (t=5) we start with climate 2020-2025
      if(temp.chg & t < time.horizon){
        cat("  Update temperature projections\n")
        aux <- temp.proj[,c(1,1+which(time.seq==t))] 
        names(aux) <- c("cell.id", "temp")
        land <- select(land, -temp) %>% left_join(aux, by="cell.id")
      }
      if(prec.chg & t < time.horizon){
        cat("  Update precipitation projections\n")
        aux <- prec.proj[,c(1,1+which(time.seq==t))]
        names(aux) <- c("cell.id", "prec")
        land <- select(land, -prec) %>% left_join(aux, by="cell.id")
      }

      
      ##################################### PROCESSES OF CHANGE #####################################
      ## 1. TIMBER SUPPLY CALCULATION
      ## It is only done during the first period if replanning is FALSE, otherwise, it is calculated each time step
      ## 1.1. AREA BASED
      if((t==time.seq[1] | params$replanif) & timber.supply=="area.based" & is.clearcut & t %in% cc.schedule){
          TS.CC.area <- timber.area(land, params)
          TS.PC.area <- timber.partial.area(land, params, pc.step)  
      }
      if(t==time.seq[1] & is.clearcut & timber.supply=="area.based"){
        ref.harv.level = TS.CC.area
      }
      if(params$a.priori < 1 & is.clearcut & timber.supply=="area.based") {
        if(params$replanif == 1){
          TS.CC.area[,2] <- pmin(ref.harv.level[,2],TS.CC.area[,2]*params$a.priori)
        } 
        else{
          #TS.CC.area[,2] <- ref.harv.level[,2]*a.priori 
          temp <- merge(ref.harv.level, a.priori.v, by.x = "mgmt.unit", 
          by.y = "V1", all.x = TRUE, all.y = T)
          # baisse de 5% par période de 5 ans (1% par an)
          temp$new <-  pmax(temp$new,(1-(0.01*t)))
          TS.CC.area[,2] <- temp$x * temp$new
        }
      }  
      
      ## 1.2. VOLUME BASED - in development
      if((t==time.seq[1] | params$replanif) & timber.supply=="volume.based" & is.clearcut & t %in% cc.schedule){
        TS.CC.vol <- timber.volume(land, params, time.step)
        TS.PC.vol <- timber.partial.volume(land, params, pc.step)
      }
      
      ## 2. FIRE
      burnt.cells <- integer() 
      if(is.wildfires & t %in% fire.schedule){
        fire.out <- wildfires(land, params, baseline.fuel, mem.feux, rcp = rcp,
                              pigni = pigni, km2.pixel = km2.pixel, time.step = time.step)
        burnt.cells <- fire.out[[1]]
        if(length(burnt.cells)>0){
          burnt.fuels <- fuel.type(filter(land, cell.id %in% burnt.cells))
          track.burnt.fuel <- rbind(track.burnt.fuel, data.frame(run=irun, year=t+params$year.ini,
              group_by(burnt.fuels, frz, type) %>% summarise(area=length(flam)*km2.pixel)))
        }
        if(nrow(fire.out[[4]])>0){
          track.target <- rbind(track.target, data.frame(run=irun, year=t+params$year.ini, fire.out[[2]]))
          track.fire.regime <- rbind(track.fire.regime, data.frame(run=irun, year=t+params$year.ini, fire.out[[3]]))
          track.fires <- rbind(track.fires, data.frame(run=irun, year=t+params$year.ini, fire.out[[4]]))
          # track.sprd <- rbind(track.sprd, data.frame(run=irun, year=t+params$year.ini, fire.out[[5]]))
        }
        # Done with fires
        land$tsfire[land$cell.id %in% burnt.cells] <- 0
        mem.feux <- fire.out$mem.feux
      }
        
      ## 3. SBW (under development)
      kill.cells <- integer()
      if(is.sbw & t %in% sbw.schedule){
        kill.cells <- sbw.outbreak(land)
        # Done with outbreak
        land$tssbw[land$cell.id %in% kill.cells] <- 0
      }

      ## 4. SELECTION OF HARVESTED CELLS BASED ON TIMBER SUPPLY
      cc.cells <- pc.cells <- integer()
      ## 4.1. AREA BASED
      if(timber.supply == "area.based" & is.clearcut & t %in% cc.schedule){      
        harv.out <- harvest.area(land, params, TS.CC.area, TS.PC.area, km2.pixel)
        cc.cells <- harv.out[[1]]
        pc.cells <- harv.out[[2]]
        if(nrow(harv.out[[3]])>0)
          track.cut <- rbind(track.cut, data.frame(run=irun, year=t+params$year.ini, harv.out[[3]]))
        if(nrow(harv.out[[4]])>0)
          track.spp.cut <- rbind(track.spp.cut, data.frame(run=irun, year=t+params$year.ini, harv.out[[4]]))
        # Done with clear cuts
        land$tspcut[land$cell.id %in% pc.cells] <- 0
        land$tsccut[land$cell.id %in% cc.cells] <- 0
      }
      
      ## 4.2. VOLUME BASED 
      if(timber.supply == "volume.based" & is.clearcut & t %in% cc.schedule){
        harv.out <- harvest.volume(land, params, TS.CC.vol, TS.PC.vol, km2.pixel)
        cc.cells <- harv.out[[1]]
        pc.cells <- harv.out[[2]]
        if(nrow(harv.out[[3]])>0)
          track.cut <- rbind(track.cut, data.frame(run=irun, year=t+params$year.ini, harv.out[[3]]))
        if(nrow(harv.out[[4]])>0)
          track.spp.cut <- rbind(track.spp.cut, data.frame(run=irun, year=t+params$year.ini, harv.out[[4]]))
        # Done with clear cuts
        land$tspcut[land$cell.id %in% pc.cells] <- 0
        land$tsccut[land$cell.id %in% cc.cells] <- 0
        cc.schedule <- cc.schedule[-1]  
        pc.schedule <- pc.schedule[-1]  
      }
      

      ##################################### VEGETATION DYNAMICS #####################################
      ## First of all, save a vector containing initial forest composition for comparison at the end of the for loop
      initial.forest.comp <- land$spp
      if(params$enable.succ){  
        ## Natural regeneration of forest after disturbance depends on the nature of the disturbance, 
        ## the age of the stand at the time the disturbance occurred, and the environmental suitability
        ## according to climate and soils. Compute it:
        suitab <- suitability(land, params)
      
        ## Regeneration after fire
        if(length(burnt.cells)>0)
          land$spp[land$cell.id %in% burnt.cells] <- 
            forest.transition(land, burnt.cells, suitab, params, type.trans="B")

        ## Regeneration after sbw outbreak
        if(length(kill.cells)>0)
          land$spp[land$cell.id %in% kill.cells] <- 
            forest.transition(land, kill.cells, suitab, params, type.trans="O")

        ##  Regeneration after clear-cutting
        if(length(cc.cells)>0)
          land$spp[land$cell.id %in% cc.cells] <- 
            forest.transition(land, cc.cells, suitab, params, type.trans="C")
      
        ######## maintien forcé de la composition forestière (plantation)
        if(params$lutte){
          territ <- !is.na(land$mgmt.unit) 
          # superficie qui passe de feu à res et l'inverse
          plant.1 <- initial.forest.comp[territ] %in% c("SAB","EPN") & land$spp[territ] %in% c("PET","BOJ","ERS") 
          plant.2 <- initial.forest.comp[territ] %in% c("PET","BOJ","ERS") & land$spp[territ] %in% c("SAB","EPN")
          #print(c(sum(plant.1),sum(plant.2)))
          plant.1a <- plant.1[land$mgmt.unit[territ] == "2751"]
          plant.2a <- plant.2[land$mgmt.unit[territ] == "2751"]
          #print(c(sum(job1b),sum(job2b))) 
          land$spp[initial.forest.comp%in% c("SAB","EPN")] <- initial.forest.comp[initial.forest.comp%in% c("SAB","EPN")]
        }
      
        ## Natural succession of tree spp at every 40 years starting at Tcomp = 70
        chg.comp.cells <- filter(land, (age-age.matu) %in% seq(40,400,40) & tscomp>=70) %>% select(cell.id)
        if(length(unlist(chg.comp.cells))>0)
          land$spp[land$cell.id %in% unlist(chg.comp.cells)] <- 
            forest.transition(land, unlist(chg.comp.cells), suitab, params, type.trans="S")  

        ## Before August 2020
        ## For those cells that change composition, reset Age at X years before maturity
        ## to account for the fact that a major change in species dominance is
        ## generaly due to significant mortality in the overstory
        # land$age[(land$spp != initial.forest.comp) & (land$cell.id %in% unlist(chg.comp.cells))] <- 
        #   land$ageMatu[(land$spp != initial.forest.comp) & (land$cell.id %in% unlist(chg.comp.cells))] - 
        #   sample(seq(10,40,5),1)
      }
      
      ## Now, for each cell that has changed composition (because of natural succession or regeneration
      ## post-distrubance), re-initialize Tcomp
      land$tscomp[land$spp != initial.forest.comp] <- 0
      land$age[land$cell.id %in% burnt.cells] <- 0
      land$age[land$cell.id %in% kill.cells] <- 0
      land$age[land$cell.id %in% cc.cells] <- 0
      land$tspcut[land$cell.id %in% c(cc.cells, kill.cells, burnt.cells)] <- 0 # -(land$age.matu/2)  # negative values!!!
      

      ## Finally, Aging: Increment Time Since Disturbance and Time Last Forest Composition change by time.step 
      land$age <- land$age + time.step
      land$tscomp <- land$tscomp + time.step
      land$tsfire <- land$tsfire + time.step      
      land$tssbw <- land$tssbw + time.step     
      land$tsccut <- land$tsccut + time.step
      land$tspcut <- land$tspcut + time.step
      
      
      
      ##################################### TRACKING AND SPATIAL OUTS #####################################
      ## Species distribution per fire zone
      track.spp.firezone <- rbind(track.spp.firezone, data.frame(run=irun, year=t+params$year.ini, 
                                group_by(land, frz, spp) %>% summarise(area=length(cell.id)*km2.pixel)))
      ## Fuel type distribution per fire zone
      fuels <- fuel.type(land)
      aux <- group_by(fuels, frz, type) %>% summarise(n=length(frz)) %>% 
             left_join(zone.size, by="frz") %>% mutate(pct=n/x) %>% select(-n, -x)
      track.fuel <- rbind(track.fuel, data.frame(run=irun, year=t+params$year.ini, aux))
      ## Age classes distribution per species and management unit      
      land$ageClass <- cut(land$age, breaks=breaks, include.lowest=TRUE, right=TRUE, labels=tags)
      track.spp.age.class <- rbind(track.spp.age.class, data.frame(run=irun, year=t+params$year.ini, 
                             group_by(land, mgmt.unit, spp) %>% count(age.class) %>%  
                             mutate(area=n*km2.pixel)) %>% select(-n))
      ## Suitability classes distribution per bioclim.domain   
      suitab <- suitability(land, params) 
      aux <- left_join(suitab, select(land, cell.id, bioclim.domain), by="cell.id") %>%
              group_by(bioclim.domain, potential.spp) %>% summarise(poor=sum(suit.clim==0)*km2.pixel, 
              med=sum(suit.clim==0.5)*km2.pixel, good=sum(suit.clim==1)*km2.pixel) 
      track.suit.class <- rbind(track.suit.class, data.frame(run=irun, year=t+params$year.ini, aux))
      aux <- group_by(fuel.type(land), frz) %>% summarise(x=mean(flam))
      rm(suitab); rm(aux)
      

      ## If required, save landscape data frame at the time steps required
      if(save.land & t %in% out.seq){
        if(!file.exists(out.path))
          dir.create(file.path(out.path), showWarnings = T) 
        saveRDS(land, file=paste0(out.path, "landscape_", irun, "t", t, ".rds"))
      }
        
    } # t
  } # irun
  
  
  cat("\n", "C. Build outputs...\n")
  # cat("Build outputs list", "\n")
  res <- list(SppByAgeClass = track.spp.age.class[-1,],
              SuitabilityClasses = track.suit.class[-1,],
              SppByFireZone = track.spp.firezone[-1,],
              FuelByFireZone = track.fuel[-1,],
              Cuts = track.cut[-1,],
              SppCut = track.spp.cut[-1,]
              )
  if(is.wildfires){
    track.fire.regime[,8:9] <- round(track.fire.regime[,8:9], 2)     
    track.fires$rem <- track.fires$target.size-track.fires$burnt.size
    res <- c(res,
             list(BurntRates = track.target[-1,],
                  FireRegime = track.fire.regime[-1,],
                  Fires = track.fires[-1,],
                  BurntFuels = track.burnt.fuel[-1,]
                  )
             )
  }

  return(res)  
} 

