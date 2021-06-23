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

  # Now start adding PMs

  # First Br30?
  Br30_E <- abind(PMlist$Br30_E, along = 0.5)[,-1,]
  Br30_W <- abind(PMlist$Br30_W, along = 0.5)[,-1,]

  browser()

  gridMPs.df$medBr30_E <- apply(X = Br30_E, FUN = median, MARGIN = 2)
  gridMPs.df$medBr30_W <- apply(X = Br30_W, FUN = median, MARGIN = 2)

  browser()

}