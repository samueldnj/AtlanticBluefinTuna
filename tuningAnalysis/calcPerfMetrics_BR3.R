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
source("makeGridMPs_BR.R")
source("pewPMs.R")
source("plots.R")
source("tools.R")




OMdvec <- 1:48

projFolder <- "BR_initGrid_hiCaps2"

perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )



gridMPs.df <- makeMP.df_BR3( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      OMs = OMdvec,
                                      projFolder = projFolder)

projFolder <- "BR_refineGrid2_YearEach30"

perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )


gridMPs.df <- makeMP.df_BR3( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      OMs = OMdvec,
                                      projFolder = projFolder)


targPars <- array(NA, dim = c(3,2))
rownames(targPars) <- c("pH30","pYrH","minProbYrH")
colnames(targPars) <- c("East","West")

pH30Grid.df <- read.csv("MSEs/BR_refineGrid2_YearEach30/perfMetrics.csv")
pH30surfaces <- makeRespSurfaces( grid.df = pH30Grid.df,  
                                  tuningPars = c("alpha","beta"),
                                  resp = "pH30",
                                  target = 0.6,
                                  tol = 0.1 )


pYrHGrid.df <- read.csv("MSEs/BR_initGrid_hiCaps2/perfMetrics.csv")
pYrHsurfaces <- makeRespSurfaces( grid.df = pYrHGrid.df,  
                                  tuningPars = c("alpha","beta"),
                                  resp = "pYrHealthy",
                                  target = 0.6,
                                  tol = 0.1 )

minpYrHGrid.df <- read.csv("MSEs/BR_refineGrid2_YearEach30/perfMetrics.csv")
minpYrHsurfaces <- makeRespSurfaces(  grid.df = minpYrHGrid.df,  
                                      tuningPars = c("alpha","beta"),
                                      resp = "minProbYrHealth",
                                      target = 0.6,
                                      tol = 0.1 )



surfList <- list( pH30 = pH30surfaces,
                  pYrH = pYrHsurfaces,
                  minpYrH = minpYrHsurfaces )


targPars <- plotRespSurfaces( surfList = surfList,
                              tuningPars = c("alpha (East area)","beta (West area)"),
                              rtext = c("pHealthy t = 30",
                                        "pHealthy t = 1:30",
                                        "min(pHealthy | t in 1:30)") )

targPars

