
# ===================================================
# = functions for model based MPs in TMB ============
# ===================================================


#' Roxygen commands
#'
#' This is a dummy function whose purpose 
#' it is to hold the useDynLib roxygen tag.
#' This tag will populate the namespace with 
#' compiled c++ functions upon package install.
#'
#' @useDynLib tunaDelay
#'
dummy <- function(){
  return(NULL)
}




#' assessDDmm
#' 
#' @param x a simulation number.
#' @param dset a list with two positions (1:East  2:West) of simulated data for use by management procedures.
#' @param AS the management area for which advice is being provided (1:East 2:West)
#' @param AMs a numeric vector of AM indicator numbers, selecting specific AMs tune to different OMs
#' @return a TAC recommendation arising from \code{x, dset, AS}.
#' @export
#' @examples
#' assessDDmm(1,dset = dset_EW,AS = 1, AMs = 1)
#' sapply(1:10,assessDD,dset = dset_EW, AS = 1, AMs = 1 )
assessDDmm <- function( x, dset, 
                        AMs = c(1,3,5),
                        caps = c(25,4),
                        TACrule = c("mean"),
                        check = TRUE )
{
  # Load the indicesOM list
  data(indicesOM)

  mmFits <- lapply( X = indicesOM[AMs],
                    FUN = fitDD,
                    dset = dset,
                    simNum = x )


  # Apply the HCR, compute area and stock
  # TAC
  mmTACs <- lapply( X = mmFits,
                    FUN = calcHCR )
  mmTACs <- do.call( rbind, mmTACs ) %>%
            mutate( zeroMeanNLL = nll - mean(nll),
                    deltaNLL = (zeroMeanNLL - min(zeroMeanNLL))/2,
                    amWts = exp(-deltaNLL)/sum(exp(-deltaNLL)),
                    wtdTAC_E = amWts * TAC_E,
                    wtdTAC_W = amWts * TAC_W )

  outTable <- mmTACs

  # Add new columns for biomass, and NLL
  outTable$Bnext_E    <- numeric(length(AMs))
  outTable$Bnext_W    <- numeric(length(AMs))

  for( amIdx in 1:length(AMs) )
  {
    outTable$Bnext_E[amIdx] <- mmFits[[amIdx]]$B_s[1]
    outTable$Bnext_W[amIdx] <- mmFits[[amIdx]]$B_s[2]
  }

  if( TACrule == "mean" )
  {
    TAC_E <- mean(mmTACs$TAC_E)
    TAC_W <- mean(mmTACs$TAC_W)
  }

  if( TACrule == "AIC" )
  {
    TAC_E <- sum(mmTACs$wtdTAC_E)
    TAC_W <- sum(mmTACs$wtdTAC_W)
  }

  # Apply the caps
  TAC_E <- min(TAC_E,caps[1])
  TAC_W <- min(TAC_W,caps[2])

  # Scale to kg
  TAC_E <- mean( TAC_E ) * 1e6
  TAC_W <- mean( TAC_W ) * 1e6

  # Check that HCR calcs are running correctly
  if(check)
  {
    if(!file.exists("outTable.csv"))
      write.csv(  outTable, file = "outTable.csv")
    else
      write.csv(  outTable, 
                  file = "outTable.csv", 
                  append = TRUE,
                  col.names = FALSE )
  }

  # Return TAC
  return(c( East = TAC_E, West = TAC_W ))
}
class(assessDDmm)<-"MMMSMP"

# calcHCR()
# Calculates stock and area TAC 
# from the mpOutput, then
calcHCR <- function(  mpOutput = mmTACs[1] )
{
  # First, calculate TAC by stock
  B_s       <- mpOutput$B_s
  msy_s     <- mpOutput$msy
  Fmsy_s    <- mpOutput$Fmsy

  # Fmsy is actually HRs, so just
  # apply it as B*F
  TAC_s     <- B_s * Fmsy_s
  for( sIdx in 1:length(B_s))
    TAC_s[sIdx] <- min( TAC_s[sIdx],msy_s[sIdx])

  # Now compute TAC by area
  B_sa      <- mpOutput$B_sa
  Bprop     <- B_sa 
  for( a in 1:ncol(Bprop))
    Bprop[,a] <- Bprop[,a] / sum(B_sa[,a])
  # Now compute area Fmsy and MSY
  # using a biomass weighting
  msy_a     <- msy_s
  Fmsy_a    <- Fmsy_s
  B_a       <- colSums(B_sa)
  TAC_a     <- TAC_s

  # Loop and compute Fmsy and MSY 
  # as biomass weighted stock values
  for( aIdx in 1:ncol(B_sa))
  {
    Fmsy_a[aIdx]  <- sum( Bprop[,aIdx] * Fmsy_s ) 
    msy_a[aIdx]   <- sum( Bprop[,aIdx] * msy_s )
    TAC_a[aIdx]   <- min( B_a[aIdx] * Fmsy_a[aIdx], msy_a[aIdx])
  }

  # Take the minimum of TAC_s and TAC_a in
  # each area
  TAC_mat <- rbind( TAC_s, TAC_a )
  TAC_EW  <- apply( X = TAC_mat, FUN = min, MARGIN = 2)

  return( data.frame( nll = mpOutput$nll,
                      TAC_E = TAC_EW[1],
                      TAC_W = TAC_EW[2]  ) )
}

# fitDD()
# Non exported function that fits a single
# AM in the multi-model/ensemble MP assessDD()
fitDD <- function(  omIndices = indicesOM[[AMs[1]]],
                    dset      = dset,
                    simNum    = x )
{
  # Get model dimensions
  nT <- dim(dset[[1]]$Cobs)[2]
  nG <- dim(dset[[1]]$Iobs)[2] - 1
  nA <- 2
  nS <- 2
  # Create area IDs vector
  areaNames <- c("East_A","West_A")
  areaIDs   <- c(rep(2,4),rep(1,6))

  # Create an index area key
  idxAreaKey <- Obs_example@MPind[,c("Name","No","Areas")]
  # Assign index types
  idxAreaKey <- unique(idxAreaKey) %>%
                mutate( idxType = 3)
  idxAreaKey[idxAreaKey$Name == "MED_LAR_SUV","idxType"] <- 1
  idxAreaKey[idxAreaKey$Name == "GOM_LAR_SUV","idxType"] <- 2
  
  # Remove RR idx
  RRidx <- which(idxAreaKey$Name == "US_RR_66_114")
  idxAreaKey <- idxAreaKey %>% filter( Name != "US_RR_66_114")

  # Reorder the idxAreaKey, save the order so we
  # can reorder I_gt
  newIdxOrder <- c( "CAN_ACO_SUV", 
                    "FR_AER_SUV2",
                    "GBYP_AER_SUV",
                    "GOM_LAR_SUV",
                    "MED_LAR_SUV",
                    "JPN_LL_NEAtl2",
                    "JPN_LL_West2" )

  # conver to numeric order
  numIdxOrder <- c()
  for( iIdx in 1:length(newIdxOrder))
    numIdxOrder <- c( numIdxOrder, which(idxAreaKey$Name == newIdxOrder[iIdx] ) )

  idxAreaKey <- idxAreaKey[numIdxOrder,]

  # Pull index names
  grs     <- as.character(idxAreaKey$Name)

  # Pull idxType and area ID for the data object
  idxType_g <- idxAreaKey$idxType
  area_g <- areaIDs[idxAreaKey$Areas][1:nG] - 1

  # Initial parameter values for catchability and obs err SD
  lntau_g <- rep(log(0.5),nG)
  initq_g <- rep(5e-2,nG)
  # Inits for "base" indices
  lntau_g[1:7] <- log(c(0.5,0.78,0.88,0.16,0.5,0.5,0.56))
  initq_g[1:7] <- c(5e-8,1e-9,1e-7,5e-5,2e-8,1e-8,1e-8)*1e6

  # Append OM indices
  I_gt <- appendOMIndices(  omIndices, 
                            dset = dset, 
                            simNum = simNum )


  I_gt <- I_gt[-(2 + RRidx),]
  # Reorder
  I_gt[3:9,] <- I_gt[(3:9)[numIdxOrder], ]

  # Append new model switch codes and dim names for omIndices
  idxType_g <- c(1:2,idxType_g)
  area_g    <- c(0,1,area_g)
  grs       <- c("East stock","West stock",grs)
  lntau_g   <- c(rep(log(0.01),2),lntau_g)
  initq_g   <- c(1,1,initq_g)
  nG        <- nG + 2


  # Reformat survey indices and age-composition into one vector
  lnObs_k  <- numeric(0)
  gear_k   <- numeric(0)
  year_k   <- numeric(0)
  iIdx_gt <- array(data=-1,dim=c(nG,nT)) # Index for rel. abun. obs
  k <- 1


  # Fix phases
  phases <- list()
  phases$logith_s       <-  -1
  phases$lnB0_s         <-   1
  phases$lnM_s          <-  -1
  phases$lntau_g        <-   1
  phases$lnq_g          <-   1
  phases$lnFinit_s      <-   1
  phases$rDev_st        <-   1
  phases$logitPropW_s   <-  -1
  phases$qDev_tg        <-  -1
  phases$lnsigmaR_s     <-  -1

  for( t in 1:nT )
  {
    # Loop over surveys
    for( g in 1:nG )
    {
      # Add survey indices
      if( g > 2)
        obs <- I_gt[g,t]*1e-6     # Observation
      else obs <- I_gt[g,t]
      if( obs>0 )
      {
        iIdx_gt[g,t]  <- k-1      # Index of obs (TMB indexing)
        lnObs_k[k]    <- log(obs) # Log observation
        gear_k[k]     <- g-1      # Fleet for obs
        year_k[k]     <- t-1      # Year of obs
        k             <- k+1      # Increment obs
      } # end if
    } # next i    
  } # next t


  # Catch
  C_at <- array( NA,  dim = c(nA,nT),
                      dimnames = list(  area = areaNames,
                                        year = NULL ) )
  C_at["East_A",] <- dset[[1]]$Cobs[simNum,]
  C_at["West_A",] <- dset[[2]]$Cobs[simNum,]

  C_at <- C_at*1e-6

  # average weight
  wbar_at <- array(NA, dim = c(nA,nT) )

  # Now we need to get kage for the DD model - this
  # comes from the inflection point of the weight-at-age
  # curve. This will likely have to be by area,
  # which is gonna suck balls
  nAges   <- dim(dset[[1]]$Mat)[2]

  # Pull length/weight at age
  lenWeightE  <- .makeLenWtAge(dset[[1]], stock = 1)
  lenWeightW  <- .makeLenWtAge(dset[[2]], stock = 2)

  # Now we want to compute the inflection
  # point of each
  eastGrowth <- .compFW( lenWeightE )
  westGrowth <- .compFW( lenWeightW )
  
  # These are all currently by area, they
  # need to changes to by stock somehow
  kage_s  <- c(eastGrowth$kage, westGrowth$kage)
  wk_s    <- c(eastGrowth$wk, westGrowth$wk)
  alpha_s <- c(eastGrowth$alpha, westGrowth$alpha)
  rho_s   <- c(eastGrowth$rho, westGrowth$rho)

  # We want to adjust process errors
  # for brood year
  nT_brood  <- nT + diff(range(kage_s)) 

  # Prior on proportion of each stock in W area:
  # Update later to be a prior based
  # on each AM's conditioning OM
  prPropW_sp <- matrix( c(0.03941496,0.004879433,
                          0.74255900,0.014410685),
                        nrow = 2, ncol = 2,
                        byrow = TRUE )

  data <- list( lnObs_k       = lnObs_k,
                year_k        = year_k,
                gear_k        = gear_k,
                iIdx_gt       = iIdx_gt,
                C_at          = C_at,
                wbar_at       = wbar_at,
                area_g        = area_g,
                kage_s        = kage_s,
                nT_brood      = nT_brood,
                maxkage       = max(kage_s),
                alpha_s       = alpha_s,
                rho_s         = rho_s,
                wk_s          = wk_s,
                prPropW_sp    = prPropW_sp,
                bioPenScale   = 1e3,
                initBioCode_s = c(1,1),
                useWbar_a     = c(0,0),
                baranovIter   = 5,
                baranovStep   = 0.5,
                idxType_g     = idxType_g,
                rType         = 1 )

  pars <- list( logith_s      = c(logit(0.945),logit(0.806)),
                lnB0_s        = log(c(800,125)),
                lnM_s         = rep(-2,2),
                lntau_g       = lntau_g,
                tauW_a        = rep(0.1,nA),
                rDev_st       = array(0, dim = c(nS,(nT-1))),
                qDev_tg       = array(0, dim = c((nT-1),nG)),
                lnsigmaR_s    = rep(log(0.1),2),
                lnsigmaQ_g    = rep(log(0.1),nG),
                #lnFinit_s     = rep(-3,2),
                lnFinit_s     = log(c(0.12,0.12)),
                logitPropW_s  = logit(c(0.039,0.743)),
                lnq_g         = log(initq_g),
                sig2Prior     = c(1,2),
                lnqbar_g      = rep(log(0.5),nG),
                lntauq_g      = rep(log(0.05),nG),
                h_alpha       = c(6,9),
                h_beta        = c(4,1),
                tau2IGa_g     = rep(10,nG),
                tau2IGb_g     = rep(0.01 * 11, nG ) )


  # message("\nFitting tunaDelay AM to Tuna data in ", nA, " areas.\n", sep = "")
  gc()

  phaseList <- .TMBphase( data = data, 
                          parameters = pars, 
                          random = NULL,
                          phases = phases, 
                          base_map = NULL,
                          maxPhase = 1,
                          model_name = "tunaDelay",
                          optimizer = "nlminb",
                          silent = TRUE,
                          maxEval = 1e5,
                          maxIter = 1e5,
                          calcSD = FALSE,
                          parBen = FALSE,
                          intMethod = "None",
                          mcChainLength = 10,
                          mcChains = 1,
                          savePhases = FALSE,
                          calcOSA = FALSE,
                          fitOmSSB = TRUE ) 


  repOpt <- phaseList$repOpt
  repOpt$grs <- grs
  repOpt$wk1_s <- c(eastGrowth$wk1,westGrowth$wk1)

  maxSuccPhz <- phaseList$maxPhaseComplete


  phaseList$repOpt <- NULL

  # Equilibirium calculations
  msyList <- .calcMSY( repOpt )

  propW_s <- repOpt$propW_s
  B_s     <- repOpt$B_st[ ,nT+1]

  B_sa <- matrix( 0, nrow = nS, ncol = nA )
  B_sa[,2] <- propW_s * B_s
  B_sa[,1] <- (1 - propW_s) * B_s


  mpOutput <- list( q_g     = repOpt$q_g,
                    h_s     = repOpt$h_s,
                    B0_s    = repOpt$B0_s,
                    B1_s    = repOpt$B_st[ ,1],
                    B_s     = repOpt$B_st[ ,nT+1],
                    B_sa    = B_sa,
                    rho_s   = repOpt$rho_s,
                    alpha_s = repOpt$alpha_s,
                    msy     = msyList$msy,
                    Bmsy    = msyList$Bmsy,
                    Fmsy    = msyList$Fmsy,
                    nll     = sum(repOpt$nllI_gt[-(1:2),]) )


  return( mpOutput )

}


# Append the OM indices to the dset object
# Called within fitDD()
appendOMIndices <- function( indicesOM, dset, simNum = x )
{
  nT <- dim(dset[[1]]$Cobs)[2]
  nG <- dim(dset[[1]]$Iobs)[2]
  nA <- 2

  # Now let's make arrays of data
  # Indices
  I_gt <- array( NA,  dim = c(nG,nT),
                      dimnames = list(  gear = NULL,
                                        year = NULL) )

  I_gt[1:nG,1:nT] <- dset[[1]]$Iobs[simNum,1:nG,1:nT]

  I_gt[is.na(I_gt)] <- -1

  # Pad the OM indices if nT > ncol indicesOM
  if( nT > ncol(indicesOM) )
  {
    padIdx <- matrix( -1, nrow = 2, ncol = nT )
    padIdx[,1:ncol(indicesOM)] <- indicesOM

    indicesOM <- padIdx
  }
  # Append
  I_gt <- rbind( indicesOM, I_gt )

  return( I_gt )
}



# compFW()
# Computes FW growth parameters and 
# DD age at recruitment from a given
# weight-at-age vector
.compFW  <- function( lnWt = lenWeightE )
{
  # Get weight at age, and ages
  W_a   <- lnWt$W_a
  nAges <- length(W_a)

  # Fit a spline
  wtAgeSpline <- splinefun(x = 1:nAges, y = W_a )
  # Compute inflection point
  inflect <- uniroot( interval = c(1,35),
                      f = wtAgeSpline,
                      deriv = 2 )$root
  # recover kage
  kage    <- floor(inflect)
  wk      <- W_a[kage]
  wk1     <- W_a[kage - 1]
  # Regress W_a+1 ~ W_a
  fwLM    <- lm( W_a[(kage+1):nAges] ~ W_a[kage:(nAges-1)] )

  alpha   <- coef(fwLM)[1]
  rho     <- coef(fwLM)[2]

  outList <- list(  kage = kage,
                    wk = wk,
                    wk1 = wk1,
                    alpha = alpha,
                    rho = rho )

  outList
}

# makeLenWtAge()
# Creates vectors of length and weight
# at age for each area (E/W)
.makeLenWtAge <- function( dset = dset$E, simNum = 1, stock = 1 )
{
  # get VonB pars
  vonK    <- dset$K[simNum]
  Linf    <- dset$Linf[simNum]
  t0      <- dset$t0[simNum]
  nAges   <- dim(dset$Mat)[2]

  # make LenAge
  L_a     <- Linf * (1 - exp(-vonK * (1:nAges - t0) ) )


  # Convert to weight at age
  c1      <- dset$a[ stock + 2 * ( simNum - 1 ) ] 
  c2      <- dset$b[ stock + 2 * ( simNum - 1 ) ] 
  W_a     <- c1 *  L_a ^ c2


  outList <- list(  L_a = L_a,
                    W_a = W_a )
}

# logit
logit <- function(p)
{
  log(p / (1-p))
}


# Custom TMBphase() function for running hierSCAL in phases. 
# Modified from the version in Kasper Kristensen's 
# TMB_contrib_R github repository 
# https://github.com/kaskr/TMB_contrib_R
# Author:Gavin Fay email: gfay42@gmail.com
# 
# Main modification adds a base map list for array based parameters that
# have (a) identified parameters or (b) only some elements
# activated due to missing data (e.g. selectivity pars for
# fleets in specific areas). Doing it this way reduces number
# of loops in the model, speeding up fitting time.
.TMBphase <- function(  data, 
                        parameters, 
                        random = NULL, 
                        phases, 
                        base_map = list(),
                        maxPhase = NULL,
                        model_name = "hierDelayAssess",
                        optimizer = "nlminb",
                        silent = FALSE,
                        calcSD = FALSE,
                        maxEval = 1000,
                        maxIter = 1000,
                        parBen = FALSE,
                        intMethod = "RE",
                        mcChainLength = 100,
                        mcChains = 1,
                        savePhases = TRUE,
                        calcOSA = FALSE,
                        fitOmSSB = FALSE ) 
{
  # function to fill list component with a factor
  # of NAs
  fill_vals <- function(x,vals){ factor( rep( vals, length(x) ) ) }

  # compile the model
  DLL_use <- model_name

  regFfleets <- data$regFfleets  
  
  #loop over phases
  if(!is.null(maxPhase))
    maxPhase <- min( maxPhase, max(unlist(phases) ) )
  else maxPhase <- max(unlist(phases))


  # Make a data.frame that will hold the phase info
  fitReport <- matrix(NA, nrow = maxPhase + 1, ncol = 8 )
  colnames(fitReport) <- c( "phase",
                            "objFun",
                            "maxGrad",
                            "nPar",
                            "convCode",
                            "convMsg",
                            "time",
                            "mcmcTime" )
  fitReport <- as.data.frame(fitReport)

  fitReport$phase <- c(1:maxPhase,"RE")

  phaseReports <- vector(mode = "list", length = maxPhase)

  # generate a list of outputs to return
  # to runHierSCAL, initialise 
  # a success flag at TRUE
  outList <- list( success = TRUE )

  for( phase_cur in 1:maxPhase ) 
  {
    gc()
    # Start timing
    tBegin <- proc.time()[3]

    # work out the map for this phase
    # if the phase for a parameter is greater than the current phase 
    # or a negative value, then map will contain a factor filled with NAs
    map_use <- base_map
    j <- length(map_use)
    for( i in 1:length(parameters) ) 
    {
      parName <- names(parameters)[i]

      if( parName %in% names(phases) )
      {
        if( (phases[[parName]] > phase_cur) | phases[[parName]] < 0 ) 
        { 
          # Check if parName is included in the base_map
          if(parName %in% names(map_use))
            map_use[[parName]] <- fill_vals(parameters[[i]],NA)
          else
          {
            j <- j + 1
            map_use[[j]] <- fill_vals(parameters[[i]],NA)
            names(map_use)[j] <- parName
          }

        }
      } else {
        j <- j + 1
        map_use[[j]] <- fill_vals(parameters[[i]],NA)
        names(map_use)[j] <- parName
      }

    }

    if(fitOmSSB)
    {
      nG <- length(data$area_g)
      qMap <- 1:nG
      sdMap <- 1:nG
      qMap[1:2] <- NA
      sdMap[1:2] <- NA
      map_use$lnq_g <- as.factor(qMap)
      map_use$lntau_g <- as.factor(sdMap)
    }
  
    #remove the random effects if they are not estimated
    random_use <- random[ !random %in% names(map_use) ]
  
    # initialize the parameters at values in previous phase
    params_use <- parameters
    if( phase_cur > 1 ) 
      params_use <- obj$env$parList( opt$par )

    # Fit the model
    obj <- TMB::MakeADFun(  data = data,
                            parameters = params_use,
                            random = NULL,
                            DLL= DLL_use,
                            map= map_use,
                            silent = silent )
    # Run benchmark for parallel accumulation
    if(parBen &  phase_cur == 1)
      outList$phase1Benchmark <- benchmark(obj, cores = 1:(detectCores()-1))

    # Create a control list for the assessment model
    tmbCtrl <- list(  eval.max = maxEval, 
                      iter.max = maxIter  )

    repInit <- obj$report()

    if( phase_cur == 1 )
    {
      checkInitNaN    <- lapply( X = repInit, FUN = .checkNaN )
      checkInitFinite <- lapply( X = repInit, FUN = .checkFinite )
#      if(any(unlist(checkInitNaN)) | any(checkInitFinite))
#        browser(beep(expr=cat("NaN or Inf items in initial rep\n")))
    }
    if(!silent)
      cat("\nStarting optimisation for phase ", phase_cur, "\n\n")
    # Try the optimisation
    opt <-  suppressWarnings(
              try( nlminb (  start     = obj$par,
                             objective = obj$fn,
                             gradient  = obj$gr,
                             control   = tmbCtrl ) )
                            )

    # break if there is an issue
    if( class(opt) == "try-error" )
    {

      cat("\nOptimisation halted due to error\n")

      outList$success                   <- FALSE
      outList$maxPhaseComplete          <- phase_cur - 1

      if( savePhases )
      {
        phaseReports[[phase_cur]]$opt     <- opt
        phaseReports[[phase_cur]]$success <- FALSE
      }

      break
    }

    # Add refitting procedure here.

    # Save max phase complete
    outList$maxPhaseComplete          <- phase_cur

    # Save reports and optimisation
    # output
    if(savePhases)
    {
      phaseReports[[phase_cur]]$pars    <- obj$env$parList(opt$par)
      phaseReports[[phase_cur]]$opt     <- opt
      phaseReports[[phase_cur]]$success <- TRUE
      phaseReports[[phase_cur]]$map     <- map_use
    }

    # Update fitReport
    if(class(opt) != "try-error")
    {
      fitReport[phase_cur,]$objFun      <- obj$fn()
      fitReport[phase_cur,]$maxGrad     <- max(abs(obj$gr()))
      fitReport[phase_cur,]$nPar        <- length(opt$par)
      fitReport[phase_cur,]$convCode    <- opt$convergence
      fitReport[phase_cur,]$convMsg     <- opt$message
    }
    fitReport[phase_cur,]$time           <- (proc.time()[3] - tBegin)/60



    if(!silent)
    {
      cat(  "\nPhase ", phase_cur, " completed with code ",
            opt$convergence, " and following message:\n", sep = "" )
      cat("\n", opt$message, "\n\n", sep = "" )
    }

    # Want to test MCMC performance, so let's MCMC every phase!
    if( intMethod == "phaseMCMC" & class(opt) != "try-error" )
    {
      tBegin      <- proc.time()[3]
      params_use  <- obj$env$parList( opt$par )

      obj <- TMB::MakeADFun(  data = data,
                              parameters = params_use,
                              random = NULL,
                              DLL = DLL_use,
                              map = map_use,
                              silent = silent ) 
      mcmc <- tmbstan(  obj, 
                        init = "last.par.best", 
                        iter = mcChainLength,
                        chains = mcChains )

      phaseReports[[phase_cur]]$mcmc <- mcmc

      mcmcTime <- (proc.time()[3] - tBegin)/60

      fitReport[phase_cur,]$mcmcTime <- mcmcTime

    } # END phaseMCMC

  } # close phase loop

  # Fit the model
  if( intMethod == "RE" & !is.null(random) &  class(opt) != "try-error" )
  { 
    randEffList <- list()

    tBegin      <- proc.time()[3]
    params_use  <- obj$env$parList( opt$par )

    obj <- TMB::MakeADFun(  data = data,
                            parameters = params_use,
                            random = c(random),
                            DLL= DLL_use,
                            map= map_use,
                            silent = silent )  

    repInit <- obj$report()

    tol10 <- 0.01
    
    TMB::newtonOption(obj, tol10 = tol10)
    
    if( parBen )
      randEffList$randEffBenchmark <- benchmark( obj, cores = 1:detectCores() )

    # Try the optimisation
    opt <- try( nlminb (  start     = obj$par,
                          objective = obj$fn,
                          gradient  = obj$gr,
                          control   = tmbCtrl ) )

    randEffList$spHess  <- obj$env$spHess(random = TRUE)
    randEffList$opt     <- opt
    randEffList$par     <- obj$eng$parList( opt$par )

    # Update fitReports
    if(class(opt) != "try-error")
    {
      fitReport[maxPhase + 1,]$objFun      <- obj$fn()
      fitReport[maxPhase + 1,]$maxGrad     <- max(abs(obj$gr()))
      fitReport[maxPhase + 1,]$nPar        <- length(opt$par)
      fitReport[maxPhase + 1,]$convCode    <- opt$convergence
      fitReport[maxPhase + 1,]$convMsg     <- opt$message
    }

    fitReport[maxPhase + 1,]$time          <- (proc.time()[3] - tBegin)/60

    outList$randEffList <- randEffList
  }

  if( intMethod == "MCMC" & class(opt) != "try-error" )
  {
    tBegin      <- proc.time()[3]
    params_use  <- obj$env$parList( opt$par )

    obj <- TMB::MakeADFun(  data = data,
                            parameters = params_use,
                            random = NULL,
                            DLL = DLL_use,
                            map = map_use,
                            silent = silent ) 


    mcmc <- tmbstan(  obj, 
                      init = "last.par.best", 
                      iter = mcChainLength,
                      chains = mcChains )

    outList$mcmc <- mcmc

  }
  
  if(outList$success & calcSD )
    outList$sdrep <- TMB::sdreport(obj)

  osa <- NULL
  if(outList$success & calcOSA )
  {
    osa <- oneStepPredict(obj, observation.name="lnObs_k",
             data.term.indicator="keep",discrete=FALSE)
    osa$year <- data$year_k
    osa$gear <- data$gear_k
  }
  outList$osa <- osa

  if( savePhases )
    outList$phaseReports      <- phaseReports

  if(outList$success)
  {
    outList$repOpt            <- obj$report()
    outList$optPar            <- obj$env$parList( opt$par )
  }
  else {
    outList$optPar         <- params_use
    outList$repOpt         <- repInit
  }

  outList$objfun            <- obj$fn()
  outList$optOutput         <- opt
  outList$map               <- map_use
  outList$maxGrad           <- max(obj$gr())
  outList$fitReport         <- fitReport
  outList$totTime           <- sum(fitReport$time)
  
  return( outList )  

} # END TMBphase()


# Check NAs/NaNs in lists
.checkNA <- function( listEntry )
{
  x <- any(is.na(listEntry))
  x
}

# Check for Infs in lists
.checkFinite <- function( listEntry )
{
  x <- any(!is.finite(listEntry))
  x
}

.checkNaN <- function( listEntry )
{
  x <- any(is.nan(listEntry))
  x
}
