# --------------------------------------------------------------------------
# runExampleMSE.R
# 
# Loads the ABTMSE package (local modified version) and 
# runs the TMB DD model based procedure
#
# Author: Samuel D N Johnson
# Date: May 30, 2019
#
# --------------------------------------------------------------------------

# Load the ABTMSE package
source("initTest.R")
source("runCMPs.R")
source("makeGridMPs_constU.R")
source("pewPMs.R")
source("plots.R")
source("tools.R")



OMdvec <- 1:48

# projFolder <- "F01_refineGrid_allTargs"

# perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
#                                     OMs = OMdvec )



# gridMPs.df <- makeMP.df_F01( projFolder =  projFolder)


# gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
#                                       OMs = OMdvec,
#                                       projFolder = projFolder)


# projFolder <- "F01_refineGrid2_pYrH"

# gridMPs.df <- makeMP.df_F01( projFolder =  projFolder)

# gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
#                                       OMs = OMdvec,
#                                       projFolder = projFolder)

# projFolder <- "F01_refineGrid2_minpYr"

# gridMPs.df <- makeMP.df_F01( projFolder =  projFolder)

# gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
#                                       OMs = OMdvec,
#                                       projFolder = projFolder)

targPars <- array(NA, dim = c(3,2))
rownames(targPars) <- c("pH30","pYrH","minProbYrH")
colnames(targPars) <- c("East","West")

# year30
pH30Grid.df <- read.csv("MSEs/F01_refineGrid_allTargs/perfMetrics.csv")
pH30surfaces <- makeRespSurfaces( grid.df = pH30Grid.df,  
                                  tuningPars = c("qEast","qWest"),
                                  resp = "pH30",
                                  target = 0.6,
                                  tol = 0.001 )

# targPars[1,] <- pH30surfaces$
# All30
pYrHGrid.df <- read.csv("MSEs/F01_refineGrid_allTargs/perfMetrics.csv")
pYrHsurfaces <- makeRespSurfaces( grid.df = pYrHGrid.df,  
                                  tuningPars = c("qEast","qWest"),
                                  resp = "pYrHealthy",
                                  target = 0.6,
                                  tol = 0.1 )
# Each30
minpYrHGrid.df <- read.csv("MSEs/F01_refineGrid_allTargs/perfMetrics.csv")
minpYrHsurfaces <- makeRespSurfaces(  grid.df = minpYrHGrid.df,  
                                      tuningPars = c("qEast","qWest"),
                                      resp = "minProbYrHealth",
                                      target = 0.6,
                                      tol = 0.001 )



surfList <- list( pH30 = pH30surfaces,
                  pYrH = pYrHsurfaces,
                  minpYrH = minpYrHsurfaces )


targPars <- plotRespSurfaces( surfList = surfList,
                              tuningPars = c("q (East area)","q (West area)"))

targPars

