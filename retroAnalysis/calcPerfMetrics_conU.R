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


projFolder <- "tuneLW_Br30"

# perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
#                                     OMs = OMdvec )


gridMPs.df <- makeMP.df_conU( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      OMs = OMdvec,
                                      projFolder = projFolder)

# Save
write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))

targPars <- array(NA, dim = c(1,2))
rownames(targPars) <- c("wtdMedBr30")
colnames(targPars) <- c("East","West")


# projFolder <- "conU_pH30_RefineGrid1"


# gridMPs.df <- makeMP.df_conU( projFolder =  projFolder)

# gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
#                                       OMs = OMdvec,
#                                       projFolder = projFolder)


# projFolder <- "conU_pYrH_RefineGrid2"

# gridMPs.df <- makeMP.df_conU( projFolder =  projFolder)

# gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
#                                       OMs = OMdvec,
#                                       projFolder = projFolder)

Br30Grid.df <- read.csv("MSEs/tuneLW_Br30/perfMetrics.csv")
Br30surfaces <- makeRespSurfaces( grid.df = Br30Grid.df,  
                                  tuningPars = c("multEast","multWest"),
                                  resp = "wtdMedBr30",
                                  target = 1.25,
                                  tol = 0.01 )


surfList <- list( Br30 = Br30surfaces )


targPars <- plotRespSurfaces( surfList = surfList,
                              tuningPars = c("m (East area)","m (West area)"),
                              rtext = c("Br30"))

targPars



