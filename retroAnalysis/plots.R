# plots.R

plotRespSurfaces <- function( surfList,
                              tuningPars = c("multEast","multWest"),
                              rtext = c("Year30",
                                        "All30",
                                        "Each30") )
{
  nSurf <- length(surfList)
  # Now plot - we'll get to solving for
  # optima next.
  par(mfrow = c(nSurf,2), mar = c(2,2,2,2), oma = c(3,4,2,2) )
  # Plot probability of being healthy at end of 30 years
  
  targetPars <- array(NA, dim = c(nSurf,2))
  rownames(targetPars) <- rtext
  colnames(targetPars) <- c('East','West')

  colBreaks <- seq(from = 0, to = 1, length.out = 65)
  cols <- viridisLite::turbo(n = 64, begin = 0, end = 1, direction = -1)


  for( j in 1:nSurf )
  {
    targPars <- surfList[[j]]$targetPars
    targetPars[j,] <- targPars
    surfE <- surfList[[j]]$surfE
    surfE$z[surfE$z > 0.97] <- 0.97

    sampSurfE <- surfList[[j]]$sampSurf_E
    sampSurfW <- surfList[[j]]$sampSurf_W


    plot.surface( sampSurfE, breaks = colBreaks, col = cols, las = 1, type = "C" )
    # plot.surface( surfE, type = "c", add = TRUE )
      points(x = targPars[1], y = targPars[2], col = "grey40",
              pch = 16, cex = 1.5 )

    if( j == 1 )
      mtext( side = 3, text = "East stock" )
    
    surfW <- surfList[[j]]$surfW
    surfW$z[surfW$z > 0.97] <- 0.97

    plot.surface( sampSurfW, breaks = colBreaks, col = cols, las = 1, type = "C" )
      points(x = targPars[1], y = targPars[2], col = "grey40",
              pch = 16, cex = 1.5 )

    if( j == 1 ) 
      mtext( side = 3, text = "West stock" )

    mtext( side = 4, text = rtext[j], line = 5 )
  }

  
  mtext( side = 1, text = tuningPars[1], outer = TRUE )
  mtext( side = 2, text = tuningPars[2], outer = TRUE, line = 2 )

  

  targetPars

}

# Helper function to pull out B/Bmsy values
pullBr <- function( MSE )
{
  Br <- MSE@B_BMSY
  Br
}

# Helper function to pull out F/FMSY values
pullFr <- function( MSE )
{
  Fr <- MSE@F_FMSY
  Fr
}


# plot Br and Fr simulation envelopes for
# a given grid
plotBrFrSimEnvelopes <- function( MSEgrid = F01tuned, 
                                  mpIdx = 2:4,
                                  targetNames = c("Year30","All30","Each30"),
                                  probs = c(0.025, 0.5, 0.975),
                                  trace = 3  )
{
  fYear <- 1965
  nMPs  <- length(MSEgrid[[1]]@MPs)
  vertIdx <- MSEgrid[[1]]@nyears+MPlag+30
  # Pull biomass ratios
  BrList <- lapply(   X = MSEgrid, FUN = pullBr )
  # Combine into an array
  Br.array <- abind(  BrList, along = 0.5 )
  dimnames(Br.array) <- list( OM = 1:48,
                              MP = 1:nMPs,
                              rep = 1:2,
                              stock = 1:2,
                              t = 1:109 )


  # Pull F ratios
  FrList <- lapply(   X = MSEgrid, FUN = pullFr)
  Fr.array <- abind(  FrList, along = 0.5 )
  dimnames(Br.array) <- list( OM = 1:48,
                              MP = 1:nMPs,
                              rep = 1:2,
                              stock = 1:2,
                              t = 1:109 )

  if( trace > 0 )
    traceIdx <- sample(1:48,size = trace )


  MPs <- MSEgrid[[1]]@MPs[mpIdx]

  # Calculate envelopes
  # Biomass
  Br_MPs <- Br.array[,mpIdx,,,,drop=FALSE]
  Br_qmst<- apply(X = Br_MPs, FUN = quantile, MARGIN = c(2,4,5), probs = probs )
  # F
  Fr_MPs <- Fr.array[,mpIdx,,,,drop=FALSE]
  Fr_qmst<- apply(X = Fr_MPs, FUN = quantile, MARGIN = c(2,4,5), probs = probs )

  # Total number of years
  totYears <- MSEgrid[[1]]@nyears + MSEgrid[[1]]@proyears
  yrs <- seq( from = fYear, by = 1, 
              length.out = totYears )

  plotYrs <- c(2018,max(yrs))

  maxBr <- max(Br_qmst, na.rm = T)
  maxFr <- 2


  mpCols <- rep("black",nMPs)
  transCols <- rep("grey65",nMPs)

  par(mfrow = c(4,length(mpIdx)), mar = c(.5,.5,.5,.5), oma = c(5,5,1,2))

  # first row is B/Bmsy for East stock, over MPs
  for( mpId in 1:length(mpIdx) )
  {
    plot( x = plotYrs, y = c(0,maxBr),
          type = "n", axes = FALSE )
      mfg <- par("mfg")
      if( mfg[1] == 1 )
        mtext( side = 3, text = targetNames[mpId], font = 2 )
      if( mfg[2] == 1 )
      {
        axis( side = 2, las = 1 )
        mtext( side = 2, text = "B/Bmsy", font = 2, line = 3 )
      }
      if( mfg[2] == mfg[4] )
        mtext(side = 4, text = "East Br", line = 1.2 )
      
      grid()
      box()
      
      xPoly <- c(yrs,rev(yrs))
      yPoly <- c(Br_qmst[1,mpId,1,],rev(Br_qmst[3,mpId,1,]))
        
      polygon(  x = xPoly, y = yPoly, 
                col = transCols[mpId], border = NA )
      lines( x = yrs, y = Br_qmst[2,mpId,1,], col = mpCols[mpId], lwd = 3)

      if( trace > 0 )
        for( tIdx in traceIdx )
         lines( x = yrs, y = Br_MPs[tIdx,mpId,1,1,], col = mpCols[mpId] )

      grid()
      abline(v = yrs[vertIdx], lty = 2, lwd = 1.2 )
  }
  # Next row is F/Fmsy East stock
  for( mpId in 1:length(mpIdx) )
  {
    plot( x = plotYrs, y = c(0,maxFr),
          type = "n", axes = FALSE )
      mfg <- par("mfg")
      # if( mfg[1] == 1 )
      #   mtext( side = 3, text = MPs[[mpId]][1], font = 2 )
      if( mfg[2] == 1 )
      {
        axis( side = 2, las = 1 )
        mtext( side = 2, text = "F/Fmsy", font = 2, line = 3 )
      }
      if( mfg[2] == mfg[4] )
        mtext(side = 4, text = "East Fr", line = 1.2 )
      
      grid()
      box()
      
      xPoly <- c(yrs,rev(yrs))
      yPoly <- c(Fr_qmst[1,mpId,1,],rev(Fr_qmst[3,mpId,1,]))
        
      polygon(  x = xPoly, y = yPoly, 
                col = transCols[mpId], border = NA )
      lines( x = yrs, y = Fr_qmst[2,mpId,1,], col = mpCols[mpId], lwd = 3)

      if( trace > 0 )
        for( tIdx in traceIdx )
         lines( x = yrs, y = Fr_MPs[tIdx,mpId,1,1,], col = mpCols[mpId] )

      grid()
      abline(v = yrs[vertIdx], lty = 2, lwd = 1.2 )
  }

  # Now West stock Br
  for( mpId in 1:length(mpIdx) )
  {
    plot( x = plotYrs, y = c(0,maxBr),
          type = "n", axes = FALSE )
      mfg <- par("mfg")
      if( mfg[2] == 1 )
      {
        axis( side = 2, las = 1 )
        mtext( side = 2, text = "B/Bmsy", font = 2, line = 3 )
      }
      if( mfg[2] == mfg[4] )
        mtext(side = 4, text = "West Br", line = 1.2 )
      
      grid()
      box()
      
      xPoly <- c(yrs,rev(yrs))
      yPoly <- c(Br_qmst[1,mpId,2,],rev(Br_qmst[3,mpId,2,]))
        
      polygon(  x = xPoly, y = yPoly, 
                col = transCols[mpId], border = NA )
      lines( x = yrs, y = Br_qmst[2,mpId,2,], col = mpCols[mpId], lwd = 3)

      if( trace > 0 )
        for( tIdx in traceIdx )
          lines( x = yrs, y = Br_MPs[tIdx,mpId,1,2,], col = mpCols[mpId] )

      grid()
      abline(v = yrs[vertIdx], lty = 2, lwd = 1.2 )
  }

  # Now West stock Fr
  for( mpId in 1:length(mpIdx) )
  {
    plot( x = plotYrs, y = c(0,maxFr),
          type = "n", axes = FALSE )
      mfg <- par("mfg")
      if( mfg[1] == 3 )
        mtext( side = 3, text = MPs[[mpId]][2], font = 2 )
      if( mfg[2] == 1 )
      {
        axis( side = 2, las = 1 )
        mtext( side = 2, text = "F/Fmsy", font = 2, line = 3 )
      }
      if( mfg[2] == mfg[4] )
        mtext(side = 4, text = "West Fr", line = 1.2 )
      
      axis( side = 1 )
      grid()
      box()
      
      xPoly <- c(yrs,rev(yrs))
      yPoly <- c(Fr_qmst[1,mpId,2,],rev(Fr_qmst[3,mpId,2,]))
        
      polygon(  x = xPoly, y = yPoly, 
                col = transCols[mpId], border = NA )
      lines( x = yrs, y = Fr_qmst[2,mpId,2,], col = mpCols[mpId], lwd = 3)

      if( trace > 0 )
        for( tIdx in traceIdx )
         lines( x = yrs, y = Fr_MPs[tIdx,mpId,1,2,], col = mpCols[mpId] )

      grid()
      abline(v = yrs[vertIdx], lty = 2, lwd = 1.2 )
  }
  
  mtext( side = 1, outer = TRUE, text = "Year", line = 3 )
}
