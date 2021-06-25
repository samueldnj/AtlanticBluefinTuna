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
  
  for( j in 1:nSurf )
  {
    targPars <- surfList[[j]]$targetPars
    plot.surface( surfList[[j]]$surfE )
      points(x = targPars[1], y = targPars[2], col = "grey40",
              pch = 16, cex = 1.5 )

    if( j == 1 )
      mtext( side = 3, text = "East stock" )
    
    plot.surface( surfList[[j]]$surfW )
      points(x = targPars[1], y = targPars[2], col = "grey40",
              pch = 16, cex = 1.5 )

    if( j == 1 ) 
      mtext( side = 3, text = "West stock" )

    mtext( side = 4, text = rtext[j], line = 4 )
  }

  
  mtext( side = 1, text = tuningPars[1], outer = TRUE )
  mtext( side = 2, text = tuningPars[2], outer = TRUE )

  targetPars <- rbind(pH30targPars, 
                      pYrHtargPars,
                      minpYrHtargPars )

  rownames(targetPars) <- c("pH30","pYrH","minpYrH")

  targetPars

}