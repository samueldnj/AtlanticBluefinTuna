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

projFolder <- "EA_tuneBr30_targ1"

# perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    # OMs = OMdvec )



gridMPs.df <- makeMP.df_EA( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      OMs = OMdvec,
                                      projFolder = projFolder)

# Save
write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))


targPars <- array(NA, dim = c(1,2))
rownames(targPars) <- c("wtdMedBr30")
colnames(targPars) <- c("East","West")

Br30Grid.df <- read.csv("MSEs/EA_tuneBr30_targ/perfMetrics.csv")
Br30surfaces <- makeRespSurfaces( grid.df = Br30Grid.df,  
                                  tuningPars = c("targE","targW"),
                                  resp = "wtdMedBr30",
                                  target = 1.25,
                                  tol = 0.01 )


# # pYrHGrid.df <- read.csv("MSEs/BR_initGrid_hiCaps3/perfMetrics.csv")
# # pYrHsurfaces <- makeRespSurfaces( grid.df = pYrHGrid.df,  
# #                                   tuningPars = c("alpha","beta"),
# #                                   resp = "pYrHealthy",
# #                                   target = 0.6,
# #                                   tol = 0.1 )

# # minpYrHGrid.df <- read.csv("MSEs/BR_refineGrid2_YearEach30/perfMetrics.csv")
# # minpYrHsurfaces <- makeRespSurfaces(  grid.df = minpYrHGrid.df,  
# #                                       tuningPars = c("alpha","beta"),
# #                                       resp = "minProbYrHealth",
# #                                       target = 0.6,
# #                                       tol = 0.1 )



surfList <- list( Br30 = Br30surfaces )


targPars <- plotRespSurfaces( surfList = surfList,
                              tuningPars = c("alpha (East area)","beta (West area)"),
                              rtext = c("wtdBr30") )

targPars

write.csv( targPars, file = file.path("MSEs",projFolder,"targPars.csv"))