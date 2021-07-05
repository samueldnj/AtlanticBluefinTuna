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


projFolder <- "testConU_5to10_allOMs"

perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )


# projFolder <- "conU_minpYrH_RefineGrid2"

gridMPs.df <- makeMP.df_conU( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      OMs = OMdvec,
                                      projFolder = projFolder)

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

# pH30Grid.df <- read.csv("MSEs/testConU_5to10_allOMs/perfMetrics.csv")
# pH30surfaces <- makeRespSurfaces( grid.df = pH30Grid.df,  
#                                   tuningPars = c("multEast","multWest"),
#                                   resp = "pH30",
#                                   target = 0.6,
#                                   tol = 0.01 )

# pYrHGrid.df <- read.csv("MSEs/testConU_5to10_allOMs/perfMetrics.csv")
# pYrHsurfaces <- makeRespSurfaces( grid.df = pYrHGrid.df,  
#                                   tuningPars = c("multEast","multWest"),
#                                   resp = "pYrHealthy",
#                                   target = 0.6,
#                                   tol = .1 )
# minpYrHGrid.df <- read.csv("MSEs/testConU_5to10_allOMs/perfMetrics.csv")
# minpYrHsurfaces <- makeRespSurfaces(  grid.df = minpYrHGrid.df,  
#                                       tuningPars = c("multEast","multWest"),
#                                       resp = "minProbYrHealth",
#                                       target = 0.6,
#                                       tol = 0.01 )

# # targetPars <- rbind(pH30targPars, 
#                       pYrHtargPars,
#                       minpYrHtargPars )

# rownames(targetPars) <- c("pH30","pYrH","minpYrH")

# surfList <- list( pH30 = pH30surfaces,
#                   pYrH = pYrHsurfaces,
#                   minpYrH = minpYrHsurfaces )


# targPars <- plotRespSurfaces( surfList = surfList,
#                               tuningPars = c("m (East area)","m (West area)"))

# targPars




# #z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# # source("tuneEastMmult.R")