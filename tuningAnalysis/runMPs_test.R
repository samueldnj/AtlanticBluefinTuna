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
source("makeGridMPs.R")

loadABT()

HRs <- seq(from = 0, to = 0.20, length.out = 21)

makeHRGridMPs(HRs = HRs, MPfile = "fixedHRgrid.R")

source("fixedHRgrid.R")

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
                      assessInt = 1,
                      MPs = gridMPs,
                      checkMPs = TRUE,
                      reloadABT = FALSE,
                      projFolderName = "bmsyTest_HRgrid" )

#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# source("tuneEastMmult.R")