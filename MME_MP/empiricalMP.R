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
#' @param FM a boolean indicating whether 2/3 M is used in place of Fmsy for HCRs
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
                      AS            = 1,
                      k             = 5,
                      caps          = c(Inf,Inf),
                      FM            = FALSE,
                      UCP           = "Bmsy",
                      TACrule       = "weighted",
                      wts           = rep(0,5),
                      phi           = NULL,
                      trendYrs      = 4,
                      check         = TRUE,
                      maxDeltaTACup = 0.2,
                      maxDeltaTACdn = 0.5,
                      propW         = c(.102,.9),
                      mpName        = "baseEmpMP",
                      plotTAC       = FALSE )
{

  # Area names for saving stuff
  areaNames <- c("East","West")

  # Read-in clustered OMs and weights
  load("../OMs/clust.Rdata")
  # OMs and weights associated with k clusters
  clustk <- clust[[as.character(k)]]

  # Load OM parameters
  OMpars <- read.csv("../OMs/empPars.csv") %>%
            filter(OM %in% clustk$OMs ) %>%
            mutate( q_GOM = exp(lnq_GOM) * 1e6,
                    q_MED = exp(lnq_MED) * 1e6 )
  OMpars <- OMpars[match(clustk$OMs,OMpars$OM), ]

  MEDidxNo <- Indices[Indices$Name == "MED_LAR_SUV",]$No
  GOMidxNo <- Indices[Indices$Name == "GOM_LAR_SUV",]$No

  # Now smooth MED and GOM - average last
  # four points - maybe use Loess smoother later
  nT        <- dim(dset[[AS]]$Iobs)[3]

  GOMsmooth <- mean(  dset[[AS]]$Iobs[x,GOMidxNo,(nT-3):nT],
                      na.rm = T)

  MEDsmooth <- mean(  dset[[AS]]$Iobs[x,MEDidxNo,(nT-3):nT],
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

  maxB <- c( max(fitTable[,c("Bmsy_MED","B0_MED","B_MED")]),
             max(fitTable[,c("Bmsy_GOM","B0_GOM","B_GOM")]) )

  # Apply the HCR, compute area and stock
  # TAC
  TACout <- lapply( X         = clustk$OMs,
                    FUN       = calcHCR,
                    fitTable  = fitTable,
                    FM        = FM,
                    caps      = caps,
                    UCP       = UCP,
                    maxB      = maxB )
  #mmTACs <- do.call( rbind, mmTACs$TAC )

  nCol <- ncol(TACout[[1]]$hcrSeqs[[1]])
  mmTACs <- data.frame( OM=rep(NA,k), TAC_E=rep(NA,k), TAC_W=rep(NA,k) )
  Be <- matrix( data=NA, nrow=k, ncol=nCol )
  Fes <- matrix( data=NA, nrow=k, ncol=nCol )
  Fea <- matrix( data=NA, nrow=k, ncol=nCol )
  Bw <- matrix( data=NA, nrow=k, ncol=nCol )
  Fws <- matrix( data=NA, nrow=k, ncol=nCol )
  Fwa <- matrix( data=NA, nrow=k, ncol=nCol )
  ptse <- list()
  ptsw <- list()

  for( i in 1:k )
  {
    mmTACs[i, ] <- as.numeric(TACout[[i]]$TAC)
    Be[i, ]  <- TACout[[i]]$hcrSeqs[[1]]["B", ]
    Fes[i, ] <- TACout[[i]]$hcrSeqs[[1]]["Fs", ]
    Fea[i, ] <- TACout[[i]]$hcrSeqs[[1]]["Fa", ]
    Bw[i, ]  <- TACout[[i]]$hcrSeqs[[2]]["B", ]
    Fws[i, ] <- TACout[[i]]$hcrSeqs[[2]]["Fs", ]
    Fwa[i, ] <- TACout[[i]]$hcrSeqs[[2]]["Fa", ]
    ptse[[i]] <- TACout[[i]]$hcrPts[[1]]
    ptsw[[i]] <- TACout[[i]]$hcrPts[[2]]
  }
  if( AS == 1 )
    TACvec <- mmTACs$TAC_E
  else
    TACvec <- mmTACs$TAC_W

  # if( x == 1 & dim(dset[[1]]$Cobs)[2] == 56 & TACrule == "trend")
  #   browser()

  # Set default to mean TAC
  wts <- exp(wts)

  if( TACrule == "weighted" )
    wts <- clustk$wts
  if( TACrule == "mean" )
    wts <- rep(1,5)
  if( TACrule == "min" )
  {
    wts <- rep(0,5)
    wts[which.min(TACvec)] <- 1
  }
  if( !is.null(phi) )
  {
    wts <- clustk$wts
    # Calculate trend in spawn index on log scale
    if( AS == 1 )
      trendIdx <- dset[[AS]]$Iobs[x,MEDidxNo,(nT-trendYrs + 1):nT]
    if( AS == 2 )
      trendIdx <- dset[[AS]]$Iobs[x,GOMidxNo,(nT-trendYrs + 1):nT]
    
    trend.df <- data.frame(x = 1:4, y = log(trendIdx))
    trend.lm <- lm( y ~ x, data = trend.df)
    trend.grad <- coef(trend.lm)[2]
    propGrowth <- exp(trend.grad)-1

    # zones are -inf < -2phi < phi < phi < 2phi < int
    wts     <- wts[order(TACvec)]
    TACvec  <- TACvec[order(TACvec)]


    if( TACrule == "weighted")
    {
      if( propGrowth <= -2*phi )
        wts <- wts * c(4,2,1,.5,.25)
      if( propGrowth <= -phi & propGrowth > -2*phi )
        wts <- wts * c(2,4,2,1,.5)
      if( propGrowth <= phi & propGrowth > -phi )
        wts <- wts * c(1,2,4,2,1)
      if( propGrowth <= 2*phi & propGrowth > phi )
        wts <- wts * c(.5,1,2,4,2)
      if( propGrowth > 2*phi)
        wts <- wts * c(.25,.5,1,2,4)
    }
    if( TACrule == "trend" )
    {
      if( propGrowth <= -2*phi )
        wts <- c(1,0,0,0,0)
      if( propGrowth <= -phi & propGrowth > -2*phi )
        wts <- c(1,1,0,0,0)
    }   

    if( TACrule == "pois" )
    {
      if( propGrowth <= -2*phi )
        wts <- dpois(1:5, lambda = 1)
      if( propGrowth <= -phi & propGrowth > -2*phi )
        wts <- dpois(1:5, lambda = 2)
      if( propGrowth <= phi & propGrowth > -phi )
        wts <- dpois(1:5, lambda = 3)
      if( propGrowth <= 2*phi & propGrowth > phi )
        wts <- dpois(1:5, lambda = 4)
      if( propGrowth > 2*phi)
        wts <- dpois(1:5, lambda = 5)
    }

  }

  wts <- wts / sum(wts)
  TAC <- sum( wts * TACvec )

  hcrList <- list( Be=Be, Fes=Fes, Fea=Fea, ptse=ptse,
                   Bw=Bw, Fws=Fws, Fwa=Fwa, ptsw=ptsw )

  save( hcrList, file=paste("HCRs/hcrList",x,"-",ncol(dset[[1]]$Cobs),".Rdata",sep="") )

  #if( AS == 2 )
  #{
  #  TACvec <- mmTACs$TAC_W
  #  TAC <- min(TACvec)
  #}

  
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
      

    outTableFolder <- "./outTables"

    if(!dir.exists(outTableFolder))
      dir.create(outTableFolder)

    outTableFileName <- paste("outTable_sim",x,areaNames[AS],".csv",sep = "")
    outTableFilePath <- file.path(outTableFolder,outTableFileName)

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
                      FM       = 1/3,
                      caps     = c(Inf,Inf),
                      UCP      = "Bmsy",
                      plotHCRs = 0,
                      maxB     = c(1500,150) )
{
  
  # First, calculate TAC by stock
  fitTable  <- fitTable[fitTable$OM == OM,]


  B_s       <- c(fitTable$B_MED, fitTable$B_GOM)
  msy_s     <- c(fitTable$MSY_MED, fitTable$MSY_GOM)

  if( FM )
    Fmsy_s  <- c(fitTable$M_E*FM, fitTable$M_W*FM)
  else
    Fmsy_s  <- c(fitTable$Fmsy_MED, fitTable$Fmsy_GOM)

  # Get Upper control point
  if( UCP == "Bmsy" )
    Bmsy_s    <- c(fitTable$Bmsy_MED, fitTable$Bmsy_GOM)
  if( UCP == ".4B0") 
    Bmsy_s <- c(0.4*fitTable$B0_MED, 0.4*fitTable$B0_GOM)

  # change fmsy_s to ftarget
  Ftarg_s <- rampHCR( B_s = B_s,
                      Fmsy_s = Fmsy_s,
                      Bmsy_s = Bmsy_s )

  # Fmsy is actually HRs, so just
  # apply it as B*F
  TAC_s     <- B_s * (1 - exp(-Ftarg_s))
  for( sIdx in 1:length(B_s) )
  {
    if(caps[sIdx] == "msy")
      TAC_s[sIdx] <- min( TAC_s[sIdx], msy_s[sIdx] )
  }

  # Now compute TAC by area
  B_a       <- c(fitTable$B_E, fitTable$B_W)
  msy_a     <- c(fitTable$MSY_E, fitTable$MSY_W)
  Fmsy_a    <- c(fitTable$Fmsy_E, fitTable$Fmsy_W)
  Bmsy_a    <- c(fitTable$Bmsy_E, fitTable$Bmsy_W)
  
  # Do calculations for areas instead of stocks
  Ftarg_a <- rampHCR( B_s = B_a,
                      Fmsy_s = Fmsy_a,
                      Bmsy_s = Bmsy_a )

  TAC_a     <- B_a * (1 - exp(-Ftarg_a))
  
  for( aIdx in 1:length(B_a) )
  {
    if(caps[aIdx] == "msy")
      TAC_a[aIdx] <- min( TAC_a[aIdx], msy_a[aIdx] )
  }


  # Take the minimum of TAC_s and TAC_a in
  # each area
  TAC_mat <- rbind( TAC_s, TAC_a )
  TAC_EW  <- apply( X = TAC_mat, FUN = min, MARGIN = 2)

  # Apply the cap
  for( aIdx in 1:ncol(TAC_mat) )
    if(is.numeric(caps[aIdx]))
      TAC_EW[aIdx]   <- min( TAC_EW[aIdx], caps[aIdx] )


  if( plotHCRs )
  {
    par( mfcol=c(2,2), mar=c(2,2,1,1), oma=c(2,2,0,0) )
    labs <- c("East","West")
  }

  hcrSeqs <- list()
  hcrPts  <- list()
  nI <- 100

  for( j in 1:2 )
  {
    #Bmax <- 1.1*max( Bmsy_s[j], Bmsy_a[j], B_s, B_a )
    Bmax <- maxB[j]
    Bseq <- seq( from=0, to=1.1*Bmax, length.out=nI )
    Fs <- rampHCR( B_s = Bseq,
                   Fmsy_s = rep(Fmsy_s[j],nI),
                   Bmsy_s = rep(Bmsy_s[j],nI) )
    Fa <- rampHCR( B_s = Bseq,
                   Fmsy_s = rep(Fmsy_a[j],nI),
                   Bmsy_s = rep(Bmsy_a[j],nI) )

    Cs <- Bseq*(1-exp(-Fs))
    Ca <- Bseq*(1-exp(-Fa))

    if( plotHCRs )
    {
      plot( x=range(Bseq), y=c(0,1.1*max(Fa,Fs)), type="n", las=1,
            main=labs[j] )
      plotbg()
      lines( x=Bseq, y=Fs, lwd=1.5 )
      lines( x=Bseq, y=Fa, lwd=1.5, lty=2, col="red" )
      points( x=B_s[j], y=Ftarg_s[j], pch=16, col="white" )
      points( x=B_a[j], y=Ftarg_a[j], pch=16, col="white" )
      points( x=B_s[j], y=Ftarg_s[j], lwd=1.5 )
      points( x=B_a[j], y=Ftarg_a[j], lwd=1.5, col="red" )
      legend( x="bottomright", lty=1:2, legend=c("By stock","By area"), bty="n" )
  
      plot( x=range(Bseq), y=c(0,1.1*max(Ca,Cs)), type="n", las=1 )
      plotbg()
      lines( x=Bseq, y=Cs, lwd=1.5 )
      lines( x=Bseq, y=Ca, lwd=1.5, lty=2, col="red" )
      points( x=B_s[j], y=TAC_s[j], pch=16, col="white" )
      points( x=B_a[j], y=TAC_a[j], pch=16, col="white" )
      points( x=B_s[j], y=TAC_s[j], lwd=1.5 )
      points( x=B_a[j], y=TAC_a[j], lwd=1.5, col="red" )
    }

    hcrSeqs[[j]] <- matrix( data=c(Bseq,Fs,Fa), nrow=3, ncol=nI,
                            dimnames=list(c("B","Fs","Fa")),
                            byrow=1 )
    pts <- c(B_s[j],B_a[j],Ftarg_s[j],Ftarg_a[j])
    names(pts) <- c("Bs","Ba","Fs","Fa")
    hcrPts[[j]] <- pts
  }

  return( list( TAC = data.frame( OM      = OM,
                      TAC_E   = TAC_EW[1],
                      TAC_W   = TAC_EW[2] ),
                hcrSeqs = hcrSeqs,
                hcrPts = hcrPts ) )
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
      Ftarg_s[sIdx] <- 0.1 * Fmsy_s[sIdx] + Fmsy_s[sIdx] * (D_s[sIdx] - LCP) * m_s
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

distMid <- function(x)
  abs(x-48.5)

getCentralOM <- function( mod=4 )
{
  load("mseStatsEmp.Rdata")
  e30 <- mseStats[["east"]][ ,mod,"Br30"]
  w30 <- mseStats[["west"]][ ,mod,"Br30"]

  par(mfrow=c(1,2))
  plot( 1:96, sort(e30) )
  abline( h=1 )
  plot( 1:96, sort(w30) )
  abline( h=1 )

  df <- data.frame( OM=1:96, e=order(e30), w=order(w30) ) %>%
        mutate( z=distMid(e)+distMid(w) ) %>%
        filter( z==min(z) )
  return(df)
}
















