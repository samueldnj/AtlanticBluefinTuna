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
source("assessDDMP.R")
source("calcEquilibriumDD.R")
source("MPs.R")

sfInit(parallel=TRUE, cpus = 2)
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("calcEquilibriumDD.R")

options( warn = -1 )

testMPs <- list(  loCap23M.4B0 = c("MP_loCap23M.4B0","MP_loCap23M.4B0") )

OMvec <- paste( "OM_", 1:15,sep = "" )

OMvec <- c( OMvec, "ROM_1d", "ROM_2d", "ROM_3d" )

lapply( X = OMvec, 
        FUN = runCMPs,
        assessInt = 2,
        MPs = testMPs )

