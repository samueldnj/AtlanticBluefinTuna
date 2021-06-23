# Plots for tuning analysis

makeMP.df_F01 <- function( projFolder = "testF01_qGrid_allOMs" )
{
  # First load the performance metrics
  PMlist <- readRDS(file.path("MSEs",projFolder,"PMlist.rds"))

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
  # First load the performance metrics
  PMlist <- readRDS(file.path("MSEs",projFolder,"PMlist.rds"))

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
                            projFolder = "testConstU_allOMs" )
{

  # First load the performance metrics
  PMlist <- readRDS(file.path("MSEs",projFolder,"PMlist.rds"))

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


  return(gridMPs.df)
}