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

    mtext( side = 4, text = rtext[j], line = 4 )
  }

  
  mtext( side = 1, text = tuningPars[1], outer = TRUE )
  mtext( side = 2, text = tuningPars[2], outer = TRUE )

  

  targetPars

}