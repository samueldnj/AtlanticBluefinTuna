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

# sfInit(parallel=TRUE, cpus = 2 )
# sfLibrary( ABTMSE )
# sfLibrary( TMB )
# sfClusterCall("loadABT")
# sfSource("empiricalMP.R")
# sfSource("MPs.R")
# sfSource("plots.R")
                  

OMdvec <- 1

gridHRMSEs <- lapply( X = OMdvec, 
                      FUN = runCMPs,
                      MPs = gridMPs,
                      checkMPs = TRUE,
                      reloadABT = FALSE,
                      projFolderName = "testF01_qGrid_OM1" )

#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# source("tuneEastMmult.R")