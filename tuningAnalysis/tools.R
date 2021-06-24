# Plots for tuning analysis

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

  perfMetricList <- list( pH30_E = pH30_E,
                          pH30_W = pH30_W,
                          PGK_E = PGK_E,
                          PGK_W = PGK_W,
                          yrHealth_E = yrHealth_E,
                          yrHealth_W = yrHealth_W,
                          Br30_E = Br30_E,
                          Br30_W = Br30_W )

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

  gridMPs.df$qEast <- qEast
  gridMPs.df$qWest <- qWest


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