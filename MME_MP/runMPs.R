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

shortListMPs <- list( loCap = c("MP_loCap","MP_loCap"),
                      loCap23M = c("MP_loCap23M","MP_loCap23M"),
                      hiCap23M = c("MP_hiCap23M","MP_hiCap23M"),
                      loCap23M.4B0 = c("MP_loCap23M.4B0","MP_loCap23M.4B0") )

testMPs    <- list( MPtest_Mean = c("MPtest_Mean","MPtest_Mean"),
                    MPtest_AIC = c("MPtest_AIC","MPtest_AIC"),
                    MPtest_loCap23M.4B0 = c("MPtest_loCap23M.4B0","MPtest_loCap23M.4B0"))

msyCapMPs    <- list( MP_msyCap = c("MP_msyCap","MP_msyCap"),
                      MP_msyCapF23M = c("MP_msyCapF23M","MP_msyCapF23M"),
                      MP_msyCapF23M.4B0 = c("MP_msyCapF23M.4B0","MP_msyCapF23M.4B0"))



OMvec <- paste( "OM_", 1:15,sep = "" )

ROMvec <- paste( "OM_", 1:31, sep = "" )

OMvec <- c( OMvec, "ROM_1d", "ROM_2d", "ROM_3d" )

lapply( X = ROMvec, 
        FUN = runCMPs,
        assessInt = 2,
        MPs = msyCapMPs )

