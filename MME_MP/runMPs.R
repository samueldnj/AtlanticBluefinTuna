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
source("plots.R")

sfInit(parallel=TRUE, cpus = 2 )
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("empiricalMP.R")
sfSource("MPs.R")
sfSource("plots.R")

options( warn = -1 )

# shortListMPs <- list( loCap = c("empMP_loCap","empMP_loCap"),
#                       loCap23M = c("empMP_loCap23M","empMP_loCap23M"),
#                       hiCap23M = c("empMP_hiCap23M","empMP_hiCap23M"),
#                       loCap23M.4B0 = c("empMP_loCap23M.4B0","empMP_loCap23M.4B0") )


# msyCapMPs    <- list( empMP_msyCap = c("empMP_msyCap","empMP_msyCap"),
#                       empMP_msyCap.4B0 = c("empMP_msyCap.4B0","empMP_msyCap.4B0") )

otherCapMPs  <- list( empMP_loCap = c("empMP_loCap","empMP_loCap"),
                      empMP_hiCap = c("empMP_hiCap","empMP_hiCap") )


noCapMPs <- list( empMP_noCap     = c("empMP_noCap","empMP_noCap"),
                  empMP_noCapMin  = c("empMP_noCapMin","empMP_noCapMin"))




OMdvec <- paste( "OM_", 1:15, "d",sep = "" )
OMvec <- paste( "OM_", 1:15, sep = "" )

ROMvec <- paste( "OM_", 1:31, sep = "" )

OMvec <- c( OMvec, "ROM_1d", "ROM_2d", "ROM_3d" )

# msyCapMSEs <- lapply( X = OMvec, 
#                       FUN = runCMPs,
#                       assessInt = 2,
#                       MPs = msyCapMPs,
#                       checkMPs = TRUE,
#                       projFolderName = "msyCaps" )


noCapMSEs <- lapply(  X = OMdvec, 
                      FUN = runCMPs,
                      assessInt = 2,
                      MPs = noCapMPs,
                      checkMPs = TRUE,
                      projFolderName = "noCapMPs" )

otherCapMSEs <- lapply( X = OMvec[1], 
                        FUN = runCMPs,
                        assessInt = 2,
                        MPs = otherCapMPs,
                        checkMPs = TRUE,
                        projFolderName = "hiLoCaps" )