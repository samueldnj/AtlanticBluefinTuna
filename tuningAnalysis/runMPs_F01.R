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
source("makeGridMPs_F01.R")

makeGridFzero1( qGrid = seq(from = 0.2, to = 0.8, by = 0.1),
                outFile = "autoF01gridMPs.R")

source("autoF01gridMPs.R")

projFolder <- "testF01_qGrid_allOMs"

# sfInit(parallel=TRUE, cpus = 2 )
# sfLibrary( ABTMSE )
# sfLibrary( TMB )
# sfClusterCall("loadABT")
# sfSource("empiricalMP.R")
# sfSource("MPs.R")
# sfSource("plots.R")
                  

OMdvec <- c(1:48)

gridqMSEs <- lapply(  X = OMdvec, 
                      FUN = runCMPs,
                      MPs = gridMPs,
                      checkMPs = TRUE,
                      reloadABT = FALSE,
                      projFolderName = projFolder )




pH30_E <- lapply(X = gridqMSEs, FUN = pH30, pp = 1)
pH30_W <- lapply(X = gridqMSEs, FUN = pH30, pp = 2)

PGK_E <- lapply(X = gridqMSEs, FUN = PGK, pp = 1)
PGK_W <- lapply(X = gridqMSEs, FUN = PGK, pp = 2)

yrHealth_E <- lapply(X = gridqMSEs, FUN = tfHealthy_t, pp = 1)
yrHealth_W <- lapply(X = gridqMSEs, FUN = tfHealthy_t, pp = 2)

perfMetricList <- list( pH30_E = pH30_E,
                        pH30_W = pH30_W,
                        PGK_E = PGK_E,
                        PGK_W = PGK_W,
                        yrHealth_E = yrHealth_E,
                        yrHealth_W = yrHealth_W )

saveRDS(perfMetricList, file = file.path("MSEs",projFolder,"PMlist.rds"))

#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# source("tuneEastMmult.R")