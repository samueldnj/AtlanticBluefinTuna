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

makeGridconstU( eMult = seq(from = 1, to = 5, by = 1),
                wMult = seq(from = 7, to = 12, by = 1),
                outFile = "autoConstUgridMPs.R" )

source("autoConstUgridMPs.R")

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
                      projFolderName = "testConstU_multGrid_OM1" )

#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# source("tuneEastMmult.R")