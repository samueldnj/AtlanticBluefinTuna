# Tools for tuning analysis

loadProject <- function(  projFolder = "testF01_qGrid_allOMs", 
                          OMs = 1:48 )
{
  # Load rda files
  rdas <- paste0("MSEs/",projFolder,"/rdas/MSE_",OMs,".rda" )

  gridMSEs <- lapply(X = rdas, FUN = readRDS )
  
  gridMSEs 
}

# Calculate performance metrics for a grid of
# operating model objects
calcPerfMetrics <- function(  projFolder = "testF01_qGrid_allOMs", 
                              OMs = 1:48 )
{
  # Load rda files
  rdas <- paste0("MSEs/",projFolder,"/rdas/MSE_",OMs,".rda" )

  gridMSEs <- lapply(X = rdas, FUN = readRDS )

  pH30_E <- lapply(X = gridMSEs, FUN = pH30, pp = 1)
  pH30_W <- lapply(X = gridMSEs, FUN = pH30, pp = 2)

  PGK_E <- lapply(X = gridMSEs, FUN = PGK, pp = 1)
  PGK_W <- lapply(X = gridMSEs, FUN = PGK, pp = 2)

  yrHealth_E <- lapply(X = gridMSEs, FUN = tfHealthy_t, pp = 1)
  yrHealth_W <- lapply(X = gridMSEs, FUN = tfHealthy_t, pp = 2)

  Br30_E <- lapply(X = gridMSEs, FUN = Br30, pp = 1)
  Br30_W <- lapply(X = gridMSEs, FUN = Br30, pp = 2)

  AvC30_E <- lapply(X = gridMSEs, FUN = AvC30, pp = 1)
  AvC30_W <- lapply(X = gridMSEs, FUN = AvC30, pp = 2)

  AvgBr_E <- lapply(X = gridMSEs, FUN = AvgBr, pp = 1)
  AvgBr_W <- lapply(X = gridMSEs, FUN = AvgBr, pp = 2)

  perfMetricList <- list( pH30_E = pH30_E,
                          pH30_W = pH30_W,
                          PGK_E = PGK_E,
                          PGK_W = PGK_W,
                          yrHealth_E = yrHealth_E,
                          yrHealth_W = yrHealth_W,
                          Br30_E = Br30_E,
                          Br30_W = Br30_W,
                          AvC30_E = AvC30_E,
                          AvC30_W = AvC30_W,
                          AvgBr_E = AvgBr_E,
                          AvgBr_W = AvgBr_W  )

  saveRDS(perfMetricList, file = file.path("MSEs",projFolder,"PMlist.rds"))

  perfMetricList
} # END calcPerfMetrics()


# 
makeMP.df_F01 <- function( projFolder = "testF01_qGrid_allOMs" )
{

  # Get gridMPs
  gridMPs <- readRDS(file.path("MSEs",projFolder,"gridMPs.rds"))

  # Stack gridMPs into a df
  gridMPs.df <- do.call(rbind,gridMPs) %>% as.data.frame()
  colnames(gridMPs.df) <- c("EastMP","WestMP")

  # Pull out tuning par values
  MPsEast <- gridMPs.df[,1]
  eastMPsplit <- stringr::str_split(MPsEast, pattern = "_q")
  qEast <- unlist(eastMPsplit)[2*(1:length(eastMPsplit))]

  MPsWest <- gridMPs.df[,2]
  westMPsplit <- stringr::str_split(MPsWest, pattern = "_q")
  qWest <- unlist(westMPsplit)[2*(1:length(westMPsplit))]

  gridMPs.df$qEast <- as.numeric(qEast)
  gridMPs.df$qWest <- as.numeric(qWest)


  gridMPs.df
}

makeMP.df_conU <- function( projFolder = "testConstU_allOMs" )
{

  # Get gridMPs
  gridMPs <- readRDS(file.path("MSEs",projFolder,"gridMPs.rds"))

  # Stack gridMPs into a df
  gridMPs.df <- do.call(rbind,gridMPs) %>% as.data.frame()
  colnames(gridMPs.df) <- c("EastMP","WestMP")

  # Pull out tuning par values
  MPsEast <- gridMPs.df[,1]
  eastMPsplit <- stringr::str_split(MPsEast, pattern = "_m")
  mEast <- unlist(eastMPsplit)[2*(1:length(eastMPsplit))]

  MPsWest <- gridMPs.df[,2]
  westMPsplit <- stringr::str_split(MPsWest, pattern = "_m")
  mWest <- unlist(westMPsplit)[2*(1:length(westMPsplit))]

  gridMPs.df$multEast <- as.numeric(mEast)
  gridMPs.df$multWest <- as.numeric(mWest)


  gridMPs.df
}

makeMP.df_BR3 <- function( projFolder = "testConstU_allOMs" )
{

  # Get gridMPs
  gridMPs <- readRDS(file.path("MSEs",projFolder,"gridMPs.rds"))

  # Stack gridMPs into a df
  gridMPs.df <- do.call(rbind,gridMPs) %>% as.data.frame()
  colnames(gridMPs.df) <- c("EastMP","WestMP")

  # Pull out tuning par values
  MPsEast <- gridMPs.df[,1]
  eastMPsplit <- stringr::str_split(MPsEast, pattern = "_a")
  mEast <- unlist(eastMPsplit)[2*(1:length(eastMPsplit))]

  MPsWest <- gridMPs.df[,2]
  westMPsplit <- stringr::str_split(MPsWest, pattern = "_b")
  mWest <- unlist(westMPsplit)[2*(1:length(westMPsplit))]

  gridMPs.df$alpha <- as.numeric(mEast)
  gridMPs.df$beta <- as.numeric(mWest)


  gridMPs.df
}

# Take gridMPs.df object for the particular CMP
# and add the performance metrics we've defined
# to the data.frame. These can then be used to
# generate response surfaces
addPerfMetrics <- function( gridMPs.df, 
                            OMs = 1:48,
                            projFolder = "testConstU_allOMs" )
{

  # First calculate the performance metrics
  PMlist <- calcPerfMetrics(projFolder,OMs = OMs)

  # First Br30?
  Br30_E <- abind(PMlist$Br30_E, along = 0.5)[,-1,]
  Br30_W <- abind(PMlist$Br30_W, along = 0.5)[,-1,]

  gridMPs.df$medBr30_E <- apply(X = Br30_E, FUN = median, MARGIN = 2)
  gridMPs.df$medBr30_W <- apply(X = Br30_W, FUN = median, MARGIN = 2)

  gridMPs.df$lpcBr30_E <- apply(X = Br30_E, FUN = quantile, MARGIN = 2, probs = 0.05)
  gridMPs.df$lpcBr30_W <- apply(X = Br30_W, FUN = quantile, MARGIN = 2, probs = 0.05)


  # Then Average Br
  AvgBr_E <- abind(PMlist$AvgBr_E, along = 0.5)[,-1,]
  AvgBr_W <- abind(PMlist$AvgBr_W, along = 0.5)[,-1,]

  gridMPs.df$medAvgBr_E <- apply(X = AvgBr_E, FUN = median, MARGIN = 2)
  gridMPs.df$medAvgBr_W <- apply(X = AvgBr_W, FUN = median, MARGIN = 2)

  # Then av catch
  AvC30_E <- abind(PMlist$AvC30_E, along = 0.5)[,-1,]
  AvC30_W <- abind(PMlist$AvC30_W, along = 0.5)[,-1,]

  gridMPs.df$medAvC30_E <- apply(X = AvC30_E, FUN = median, MARGIN = 2)
  gridMPs.df$medAvC30_W <- apply(X = AvC30_W, FUN = median, MARGIN = 2)




  # Then prob Healthy in year 30
  H30_E <- abind(PMlist$pH30_E, along = 0.5)[,-1,]
  H30_W <- abind(PMlist$pH30_W, along = 0.5)[,-1,]

  gridMPs.df$pH30_E <- apply(X = H30_E, FUN = mean, MARGIN = 2)
  gridMPs.df$pH30_W <- apply(X = H30_W, FUN = mean, MARGIN = 2)

  # Prob healthy over the next 30 years
  pYrHealthy_E <- abind(PMlist$PGK_E, along = 0.5)[,-1,]/100
  pYrHealthy_W <- abind(PMlist$PGK_W, along = 0.5)[,-1,]/100

  gridMPs.df$pYrHealthy_E <- apply(X = pYrHealthy_E, FUN = median, MARGIN = 2)
  gridMPs.df$pYrHealthy_W <- apply(X = pYrHealthy_W, FUN = median, MARGIN = 2)

  # Now min prob healthy in each of the next 30 years
  yrHealth_E <- abind(PMlist$yrHealth_E, along = 0.5)[,-1,]
  yrHealth_W <- abind(PMlist$yrHealth_W, along = 0.5)[,-1,]

  probYrHealth_E <- apply(X = yrHealth_E, FUN = mean, MARGIN = c(2,3))
  probYrHealth_W <- apply(X = yrHealth_W, FUN = mean, MARGIN = c(2,3))

  gridMPs.df$minProbYrHealth_E <- apply(X = probYrHealth_E, FUN = min, MARGIN = 1)
  gridMPs.df$minProbYrHealth_W <- apply(X = probYrHealth_W, FUN = min, MARGIN = 1)

  write.csv(gridMPs.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))

  return(gridMPs.df)
}


# Find target parameters in a surface
findTargPars <- function( surface1 = pH30_Esurf,
                          surface2 = pH30_Wsurf,
                          target = .6,
                          tol = 0.001)
{
  # Copy surface1 object and take diff between 1 and 2
  surface1$z[surface1$z >= 1] <- NA
  surface2$z[surface2$z >= 1] <- NA
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
  if(range(splineY)[1] == range(splineY)[2])
    return(c(NA,NA))


  if(target <= range(splineY)[2] & target >= range(splineY)[1])
  {
    xSpline <- splinefun(x = sameZpars[,1], y = splineY - target )
    ySpline <- splinefun(x = sameZpars[,2], y = splineY - target )

    # Solve for x value
    if(length(unique(sameZpars[,1])) >= 2)
      xVal <- uniroot( xSpline, interval = range(sameZpars[,1]) )$root
    else xVal <- unique(sameZpars[,1])

    if(length(unique(sameZpars[,2])) >= 2)
      yVal <- uniroot( ySpline, interval = range(sameZpars[,2]) )$root
    else yVal <- unique(sameZpars[,2])
    
    targPars <- c(xVal,yVal)
  } else targPars <- c(NA,NA)
  
  targPars
}

# response surfaces and solving for target
# tuning parameter values
makeRespSurfaces <- function( grid.df = gridPerfMetrics.df,  
                              tuningPars = c("qEast","qWest"),
                              resp = "pH30",
                              target = 0.6,
                              tol = 0.1 )
{
  # Generate response surfaces
  tpsE <- Tps( x = as.matrix(grid.df[,tuningPars]),
                    Y = grid.df[[paste0(resp,"_E")]] )

  surfE <- predictSurface(tpsE)

  tpsW <- Tps( x = as.matrix(grid.df[,tuningPars]),
                    Y = grid.df[[paste0(resp,"_W")]] )

  surfW <- predictSurface(tpsW)

  # Solve for optimal tuning parameters
  targPars <- findTargPars( surface1 = surfE,
                            surface2 = surfW,
                            target = target, tol = tol )
  

  out <- list(  surfE = surfE,
                surfW = surfW,
                targetPars = targPars)

  return(out)
}