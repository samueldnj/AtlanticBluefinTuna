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

projFolder <- "AH_phaseIn_stoch"

# perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
#                                     OMs = OMdvec )



gridMPs.df <- makeMP.df_F01( projFolder =  projFolder, filename = "mpList.rds")


gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      OMs = OMdvec,
                                      projFolder = projFolder)

write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.scv"))

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

# wtdBr30
wtdBr30Grid.df <- read.csv("MSEs/testF01_qGrid_allOMs/perfMetrics.csv")
wtdBr30surfaces <- makeRespSurfaces( grid.df = wtdBr30Grid.df,  
                                      tuningPars = c("qEast","qWest"),
                                      resp = "wtdMedBr30",
                                      target = 1.25,
                                      tol = 0.01 )

surfList <- list( Br30 = wtdBr30surfaces )


targPars <- plotRespSurfaces( surfList = surfList,
                              tuningPars = c("q (East area)","q (West area)"),
                              rtext = c("wtdBr30") )

targPars

write.csv( targPars, file = file.path("MSEs",projFolder,"targPars.csv"))