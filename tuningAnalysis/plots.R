# plots.R

plotRespSurfaces <- function( surfList,
                              tuningPars = c("multEast","multWest"),
                              rtext = c("pHealthy t = 30",
                                        "pHealthy t = 1:30",
                                        "min(pHealthy | t in 1:30)") )
{
  nSurf <- length(surfList)
  # Now plot - we'll get to solving for
  # optima next.
  par(mfrow = c(nSurf,2), mar = c(2,2,2,2), oma = c(3,3,2,2) )
  # Plot probability of being healthy at end of 30 years
  
  targetPars <- array(NA, dim = c(nSurf,2))
  rownames(targetPars) <- rtext
  colnames(targetPars) <- c('East','West')

  for( j in 1:nSurf )
  {
    targPars <- surfList[[j]]$targetPars
    targetPars[j,] <- targPars
    surfE <- surfList[[j]]$surfE
    surfE$z[surfE$z > 0.97] <- 0.97
    plot.surface( surfE )
      points(x = targPars[1], y = targPars[2], col = "grey40",
              pch = 16, cex = 1.5 )

    if( j == 1 )
      mtext( side = 3, text = "East stock" )
    
    surfW <- surfList[[j]]$surfW
    surfW$z[surfW$z > 0.97] <- 0.97

    plot.surface( surfW )
      points(x = targPars[1], y = targPars[2], col = "grey40",
              pch = 16, cex = 1.5 )

    if( j == 1 ) 
      mtext( side = 3, text = "West stock" )

    mtext( side = 4, text = rtext[j], line = 5 )
  }

  
  mtext( side = 1, text = tuningPars[1], outer = TRUE )
  mtext( side = 2, text = tuningPars[2], outer = TRUE )

  

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
plotBrFrSimEnvelopes <- function( MSEgrid, 
                                  mpIdx = 1:3,
                                  probs = c(0.025, 0.5, 0.975)  )
{
  fYear <- 1965
  # Pull biomass ratios
  BrList <- lapply(   X = MSEgrid, FUN = pullBr )
  # Combine into an array
  Br.array <- abind(  BrList, along = 0.5 )
  dimnames(Br.array) <- list( OM = 1:48,
                              MP = 1:50,
                              rep = 1:2,
                              stock = 1:2,
                              t = 1:109 )

  # Pull F ratios
  FrList <- lapply(   X = MSEgrid, FUN = pullFr)
  Fr.array <- abind(  FrList, along = 0.5 )
  dimnames(Br.array) <- list( OM = 1:48,
                              MP = 1:50,
                              rep = 1:2,
                              stock = 1:2,
                              t = 1:109 )

  # Calculate envelopes
  # Biomass
  Br_MPs <- Br.array[,mpIdx,,,,drop=FALSE]
  Br_qms <- apply(X = BR_MPs, FUN = quantile, MARGIN = c(2,4,5), probs = probs )
  # F
  Fr_MPs <- Br.array[,mpIdx,,,,drop=FALSE]
  Fr_qms <- apply(X = BR_MPs, FUN = quantile, MARGIN = c(2,4,5), probs = probs )

  # Total number of years
  totYears <- MSEgrid[[1]]@nyears + MSEgrid[[1]]@proyears
  yrs <- seq( from = fYear, by = 1, 
              length.out = totYears )

  maxBr <- max(Br_qms, na.rm = T)
  maxFr <- max(Fr_qms, na.rm = T)

  mpCols <- RColorBrewer::brewer.pal(n = length(mpIdx), "Dark2")
  transCols <- scales::alpha(mpCols, alpha = 0.5)

  par(mfrow = c(2,2), mar = c(2,2,2,2), oma = c(3,3,1,2))
  plot( x = range(yrs), y = c(0,maxBr),
        type = "n", axes = FALSE )
    for( mpId in 1:length(mpIdx) )
    {
      xPoly <- c(yrs,rev(yrs))
      yPoly <- c(Br_qms[1,mpId,],rev(Br_qms[3,mpId,]))
      
      polygon(  x = xPoly, y = yPoly, 
                )
    }
}
