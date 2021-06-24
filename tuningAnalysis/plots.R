# plots.R

# Find target parameters in a surface
findTargPars <- function( surface1 = pH30_Esurf,
                          surface2 = pH30_Wsurf,
                          target = .6,
                          tol = 0.001)
{
  # Copy surface1 object and take diff between 1 and 2
  diffSurface <- surface1
  diffSurface$z <- abs(surface1$z - surface2$z)
  # find where they are the same
  sameZindices    <- which(diffSurface$z < tol, arr.ind = TRUE )  

  # Now we want to limit this to 1 y value for every x, so let's
  # do that
  uniqueXind <- unique(sameZindices[,1])
  uniqueZindices <- array(0, dim = c(length(uniqueXind),2) )
  sameZvals       <- array(0,dim = c(length(uniqueXind),2))
  uniqueZindices[,1] <- uniqueXind
  for( rIdx in 1:length(uniqueXind) )
  { 
    xInd <- uniqueXind[rIdx]
    yInd <- which.min(diffSurface$z[xInd,])
    uniqueZindices[rIdx,2] <- yInd
    sameZvals[rIdx,1] <- surface1$z[xInd,yInd]
    sameZvals[rIdx,2] <- surface2$z[xInd,yInd]
  }

  sameZpars       <- cbind(surface1$x[uniqueZindices[,1]],surface1$y[uniqueZindices[,2]])



  # Now, we use the z values from surface 1 along the curve in 
  # uniqueZindices to create a spline, and solve for the x value (E), 
  # which will give us the y value (W)
  splineY <- numeric( length = nrow(uniqueZindices) )
  for(j in 1:nrow(uniqueZindices))
    splineY[j] <- surface1$z[uniqueZindices[j,1],uniqueZindices[j,2]]

  # make response spline
  xSpline <- splinefun(x = sameZpars[,1], y = splineY - target )
  ySpline <- splinefun(x = sameZpars[,2], y = splineY - target )

  # Solve for x value
  xVal <- uniroot( xSpline, interval = range(sameZpars[,1]) )$root
  yVal <- uniroot( ySpline, interval = range(sameZpars[,1]) )$root

  
  targPars <- c(xVal,yVal)
  
  targPars
}

# response surface plots
makeRespSurfaces <- function( grid.df,  
                              tuningPars = c("multEast","multWest"),
                              target = 0.6,
                              tol = 0.001 )
{
  # colBreaks <- seq(from = 0.3, to = 1.0, by = 0.1)

  # Probability of being healthy in year 30
  pH30_Etps <- Tps( x = as.matrix(grid.df[,tuningPars]),
                    Y = grid.df$pH30_E )

  pH30_Esurf <- predictSurface(pH30_Etps)

  pH30_Wtps <- Tps( x = as.matrix(grid.df[,tuningPars]),
                    Y = grid.df$pH30_W )

  pH30_Wsurf <- predictSurface(pH30_Wtps)



  pH30targPars <- findTargPars( surface1 = pH30_Esurf,
                                surface2 = pH30_Wsurf,
                                target = target, tol = tol )



  

  # Probability of being healthy over 30 years
  pYrH_Etps <- Tps( x = as.matrix(grid.df[,tuningPars]),
                    Y = grid.df$pYrHealthy_E )

  pYrH_Esurf <- predictSurface(pYrH_Etps)

  pYrH_Wtps <- Tps( x = as.matrix(grid.df[,tuningPars]),
                    Y = grid.df$pYrHealthy_W )

  pYrH_Wsurf <- predictSurface(pYrH_Wtps)

  browser()

  pYrHtargPars <- findTargPars( surface1 = pYrH_Esurf,
                                surface2 = pYrH_Wsurf,
                                target = target, tol = tol )


  # Min probability that stock is healthy in each
  # of the first 30 years
  minpYrH_Etps <- Tps(  x = as.matrix(grid.df[,tuningPars]),
                        Y = grid.df$minProbYrHealth_E )

  minpYrH_Esurf <- predictSurface(minpYrH_Etps)

  minpYrH_Wtps <- Tps(  x = as.matrix(grid.df[,tuningPars]),
                        Y = grid.df$minProbYrHealth_W )

  minpYrH_Wsurf <- predictSurface(minpYrH_Wtps)


  minpYrHtargPars <- findTargPars(  surface1 = minpYrH_Esurf,
                                    surface2 = minpYrH_Wsurf,
                                    target = target, tol = tol ) 


  # Now plot - we'll get to solving for
  # optima next.
  par(mfrow = c(3,2), mar = c(2,2,2,2), oma = c(3,3,2,2) )
  # Plot probability of being healthy at end of 30 years
  
  
  plot.surface( pH30_Esurf )
    points(x = pH30targPars[1], y = pH30targPars[2], col = "grey40",
            pch = 16, cex = 1.5 )
    mtext( side = 3, text = "East stock" )
  
  plot.surface( pH30_Wsurf )
    points(x = pH30targPars[1], y = pH30targPars[2], col = "grey40",
            pch = 16, cex = 1.5 )
    mtext( side = 3, text = "West stock" )

  mtext( side = 4, text = "pHealthy t = 30", line = 4 )

  # Probability of being healthy over 30 years
  plot.surface( pYrH_Esurf )
    points(x = pYrHtargPars[1], y = pYrHtargPars[2], col = "grey40",
            pch = 16, cex = 1.5 )
  
  plot.surface( pYrH_Wsurf )
    points(x = pYrHtargPars[1], y = pYrHtargPars[2], col = "grey40",
          pch = 16, cex = 1.5 )

  mtext( side = 4, text = "pHealthy t = 1:30", line = 4 )

  # Probability of being healthy over 30 years
  plot.surface( minpYrH_Esurf )
    points( x = minpYrHtargPars[1], y = minpYrHtargPars[2], col = "grey40",
            pch = 16, cex = 1.5 )

  plot.surface( minpYrH_Wsurf )
    points( x = minpYrHtargPars[1], y = minpYrHtargPars[2], col = "grey40",
            pch = 16, cex = 1.5 )

  mtext( side = 4, text = "Min prob healthy t = 1:30", line = 4 )

  mtext( side = 1, text = tuningPars[1], outer = TRUE )
  mtext( side = 2, text = tuningPars[2], outer = TRUE )

  targetPars <- rbind(pH30targPars, 
                      pYrHtargPars,
                      minpYrHtargPars )

  rownames(targetPars) <- c("pH30","pYrH","minpYrH")

  targetPars

}