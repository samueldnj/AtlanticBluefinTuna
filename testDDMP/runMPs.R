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

sfInit(parallel=TRUE, cpus= detectCores() - 2)
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("calcEquilibriumDD.R")

options( warn = -1 )

testMPs <- list(  loCap = c("MP_loCap","MP_loCap"),
                  hiCap = c("MP_hiCap","MP_hiCap"),
                  loCap23M = c("MP_loCap23M","MP_loCap23M"),
                  hiCap23M = c("MP_hiCap23M","MP_hiCap23M") )

OMvec <- paste( "OM_", 1:14,sep = "" )

lapply( X = OMvec, 
        FUN = runCMPs,
        assessInt = 2,
        MPs = testMPs )

