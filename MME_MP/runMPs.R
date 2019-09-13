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
source("empiricalMP.R")
source("MPs.R")

sfInit(parallel=TRUE, cpus = detectCores() - 1)
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("empiricalMP.R")
sfSource("MPs.R")

options( warn = -1 )

# shortListMPs <- list( loCap = c("empMP_loCap","empMP_loCap"),
#                       loCap23M = c("empMP_loCap23M","empMP_loCap23M"),
#                       hiCap23M = c("empMP_hiCap23M","empMP_hiCap23M"),
#                       loCap23M.4B0 = c("empMP_loCap23M.4B0","empMP_loCap23M.4B0") )

testMPs    <- list( MPtest_Mean = c("MPtest_Mean","MPtest_Mean"),
                    MPtest_AIC = c("MPtest_AIC","MPtest_AIC"),
                    MPtest_loCap23M.4B0 = c("MPtest_loCap23M.4B0","MPtest_loCap23M.4B0"))

msyCapMPs    <- list( empMP_msyCap = c("empMP_msyCap","empMP_msyCap"),
                      empMP_msyCap.4B0 = c("empMP_msyCap.4B0","empMP_msyCap.4B0") )




OMdvec <- paste( "OM_", 1:15, "d",sep = "" )
OMvec <- paste( "OM_", 1:15, sep = "" )

ROMvec <- paste( "OM_", 1:31, sep = "" )

OMvec <- c( OMvec, "ROM_1d", "ROM_2d", "ROM_3d" )

msyCapMSEs <- lapply( X = OMdvec, 
                      FUN = runCMPs,
                      assessInt = 2,
                      MPs = msyCapMPs )

testMSEs <- lapply( X = OMdvec, 
                    FUN = runCMPtest,
                    assessInt = 2,
                    MPs = msyCapMPs )

