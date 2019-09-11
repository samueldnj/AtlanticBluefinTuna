# ===================================================
# = functions for empirical MPs =====================
# ===================================================

#' empMMMP
#' 
#' @param x a simulation number.
#' @param dset a list with two positions (1:East  2:West) of simulated data for use by management procedures.
#' @param AS the management area for which advice is being provided (1:East 2:West)
#' @param OMs a numeric vector of AM indicator numbers, selecting specific AMs tune to different OMs
#' @param caps a numeric vector of TAC caps to apply
#' @param UCP a character vector indicating whether to use Bmsy or .4B0 as UCP
#' @param TACrule a character vector indicating how to combine multiple TACs
#' @param check logical indicating whether to save MP information
#' @param maxDeltaTACup maximum fractional increase in TAC
#' @param maxDeltaTACdn maximum fractional decrease in TAC
#' @param propW numeric showing the proportion of biomass of each stock in the W area
#' @param mpName character vector of MP name for saving info
#' @return a TAC recommendation arising from \code{x, dset, AS}.
#' @export
#' @examples
#' empMMMP(1,dset = dset_EW,AS = 1, AMs = 1)
#' sapply(1:10,empMMMP,dset = dset_EW, AS = 1, AMs = 1 )
empMMMP <- function(  x, dset, 
                      OMs           = c(1,2,4,7,11),
                      caps          = c(Inf,Inf),
                      UCP           = "Bmsy",
                      TACrule       = c("mean"),
                      wts           = rep(0,5),
                      check         = TRUE,
                      AS            = 1,
                      maxDeltaTACup = 0.2,
                      maxDeltaTACdn = 0.5,
                      propW         = c(.102,.9),
                      mpName        = "baseEmpMP" )
{
  # Area names for saving stuff
  areaNames <- c("East","West")

  # Load OM parameters
  OMpars <- read.csv("OMfits/OMpars.csv") %>%
            filter(OM %in% OMs ) %>%
            mutate( q_GOM = exp(lnq_GOM) * 1e6,
                    q_MED = exp(lnq_MED) * 1e6 )

  MEDidxNo <- Indices[Indices$Name == "MED_LAR_SUV",]$No
  GOMidxNo <- Indices[Indices$Name == "GOM_LAR_SUV",]$No

  # Now smooth MED and GOM - average last
  # four points - maybe use Loess smoother later
  nT        <- dim(dset[[AS]]$Iobs)[3]
  GOMsmooth <- mean(  dset[[AS]]$Iobs[,GOMidxNo,(nT-3):nT],
                      na.rm = T)
  MEDsmooth <- mean(  dset[[AS]]$Iobs[,MEDidxNo,(nT-3):nT],
                      na.rm = T)

  fitTable <- OMpars

  fitTable$MEDsmooth <- MEDsmooth
  fitTable$GOMsmooth <- GOMsmooth
  fitTable$propW_GOM <- propW[2]
  fitTable$propW_MED <- propW[1]


  # Transform larval idx to biomass estimate using
  # catchability parameter
  fitTable <- fitTable %>%
              mutate( B_MED = MEDsmooth / q_MED,
                      B_GOM = GOMsmooth / q_GOM,
                      B_ME  = (1 - propW_MED) * B_MED,
                      B_MW  = (propW_MED) * B_MED,
                      B_GE  = (1 - propW_GOM) * B_GOM,
                      B_GW  = (propW_GOM) * B_GOM,
                      B_E   = B_ME + B_GE,
                      B_W   = B_MW + B_GW,
                      MSY_E = (B_ME * MSY_MED + B_GE * MSY_GOM) / B_E,
                      MSY_W = (B_MW * MSY_MED + B_GW * MSY_GOM) / B_W,
                      Fmsy_E = (B_ME * Fmsy_MED + B_GE * Fmsy_GOM) / B_E,
                      Fmsy_W = (B_MW * Fmsy_MED + B_GW * Fmsy_GOM) / B_W, 
                      Bmsy_E = (B_ME * Bmsy_MED + B_GE * Bmsy_GOM) / B_E,
                      Bmsy_W = (B_MW * Bmsy_MED + B_GW * Bmsy_GOM) / B_W )  



  # Now we have a biomass estimate
  # and some control points to feed to
  # a simple modification of the model
  # based MP's HCR.

  # Apply the HCR, compute area and stock
  # TAC
  mmTACs <- lapply( X         = OMs,
                    FUN       = calcHCR,
                    fitTable  = fitTable,
                    caps      = caps,
                    UCP       = UCP )
  mmTACs <- do.call( rbind, mmTACs )

  if( AS == 1 )
    TACvec <- mmTACs$TAC_E

  if( AS == 2 )
    TACvec <- mmTACs$TAC_W


  # Whittle down to a single TAC per
  # area
  if( TACrule == "mean" )
    TAC <- mean(TACvec)

  if( TACrule == "min" )
    TAC <- min(TACvec)

  if( TACrule == "weighted" )
  {
    TACwts <- exp(wts) / sum(exp(wts))
    TAC    <- sum( (TACwts * TACvec) )
  }
  
  # Scale to kg
  TAC <- TAC * 1e6

  # Smooth TACs from interval to interval:

  # Count length of TAC vector
  nTACs <- length( dset[[1]]$TAC[x,] )
  # Use penultimate TAC, as there is a bug with
  # the last one
  lastTAC <- dset[[AS]]$TAC[x,nTACs-1]
  # Calculate the deltaTAC up and down, just
  # to make following conditionals more readable
  deltaTACup <- 1 + maxDeltaTACup
  deltaTACdn <- 1 - maxDeltaTACdn

  # Apply maxDeltaTAC
  if( TAC/lastTAC > deltaTACup )
    TAC <- deltaTACup * lastTAC 
  if( TAC/lastTAC < deltaTACdn )
    TAC <- deltaTACdn * lastTAC 

  # Check table
  if( check )
  {
    # Combine fitTable and outTable
    outTable            <- left_join( fitTable, mmTACs, by = "OM" )
    outTable$mpName     <- mpName
    outTable$area       <- areaNames[AS]
    outTable$simNum     <- x
    
    outTableFileName <- paste("outTable_sim",x,areaNames[AS],".csv",sep = "")
    outTableFilePath <- file.path("outTables",outTableFileName)

    if(!file.exists(outTableFilePath))
    {
      # outTable$mpNum <- mpNum
      write.table(  outTable, file = outTableFilePath, sep = ",",
                    row.names = FALSE )
    }
    else
    {
      # outTable$mpNum <- mpNum
      write.table(  outTable, sep = ",",
                    file = outTableFilePath, 
                    append = TRUE, col.names = FALSE,
                    row.names = FALSE )
    }
  }

  # Return TAC
  return( TAC )

}
class(empMMMP)<-"MSMP"



# calcHCR()
# Calculates stock and area TAC 
# from the mpOutput, then
calcHCR <- function(  OM       = 1,
                      fitTable = fitTable,
                      caps     = c(Inf,Inf),
                      UCP      = "Bmsy" )
{
  
  # First, calculate TAC by stock
  fitTable  <- fitTable[fitTable$OM == OM,]


  B_s       <- c(fitTable$B_MED, fitTable$B_GOM)
  msy_s     <- c(fitTable$MSY_MED, fitTable$MSY_GOM)
  Fmsy_s  <- c(fitTable$Fmsy_MED, fitTable$Fmsy_GOM)

  # Get Upper control point
  if( UCP == "Bmsy" )
    Bmsy_s    <- c(fitTable$Bmsy_MED, fitTable$Bmsy_GOM)
  if( UCP == ".4B0") 
    Bmsy_s <- 0.4 * c(fitTable$B0_MED, fitTable$B0_GOM)

  # change fmsy_s to ftarget
  Ftarg_s <- rampHCR( B_s = B_s,
                      Fmsy_s = Fmsy_s,
                      Bmsy_s = Bmsy_s )

  # Fmsy is actually HRs, so just
  # apply it as B*F
  TAC_s     <- B_s * (1 - exp(-Ftarg_s))
  for( sIdx in 1:length(B_s) )
    TAC_s[sIdx] <- min( TAC_s[sIdx], msy_s[sIdx] )

  # Now compute TAC by area
  B_a       <- c(fitTable$B_E, fitTable$B_W)
  msy_a     <- c(fitTable$MSY_E, fitTable$MSY_W)
  Fmsy_a    <- c(fitTable$Fmsy_E, fitTable$Fmsy_W)
  Bmsy_a    <- c(fitTable$Bmsy_E, fitTable$Bmsy_W)
  
  # Do calculations for areas instead of stocks
  Ftarg_a <- rampHCR( B_s = B_a,
                      Fmsy_s = Fmsy_a,
                      Bmsy_s = Bmsy_a )

  TAC_a     <- B_a * (1 - exp( - Ftarg_a ))
  for( aIdx in 1:length(B_a) )
    TAC_a[aIdx] <- min( TAC_a[aIdx], msy_a[aIdx] )

  # Take the minimum of TAC_s and TAC_a in
  # each area
  TAC_mat <- rbind( TAC_s, TAC_a )
  TAC_EW  <- apply( X = TAC_mat, FUN = min, MARGIN = 2)

  # Apply the cap
  for( aIdx in 1:ncol(TAC_mat) )
    TAC_EW[aIdx]   <- min( TAC_EW[aIdx], caps[aIdx] )

  return( data.frame( OM      = OM,
                      TAC_E   = TAC_EW[1],
                      TAC_W   = TAC_EW[2]) )
}

# Calculate ramped HCR similar to Albacore
rampHCR <- function(  B_s, 
                      Fmsy_s, 
                      Bmsy_s,
                      LCP = 0.4,
                      UCP = 1.0 )
{
  nS <- length(B_s)

  Ftarg_s <- numeric(length = nS)

  # Current stock status
  D_s <- B_s / Bmsy_s

  # Calculate gradient of ramp
  m_s <- (1 - 0.1)/(UCP - LCP)

  # Apply ramped F HCR
  for( sIdx in 1:nS )
  {
    if( D_s[sIdx] <= LCP )
      Ftarg_s[sIdx] <- 0.1 * Fmsy_s[sIdx]
    if( D_s[sIdx] >= LCP & D_s[sIdx] <= UCP )
      Ftarg_s[sIdx] <- 0.1 * Fmsy_s[sIdx] + 0.9 * Fmsy_s[sIdx] * (D_s[sIdx] - LCP) * m_s
    if( D_s[sIdx] >= UCP )
      Ftarg_s[sIdx] <- Fmsy_s[sIdx]
  } 

  return(Ftarg_s)
}



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
