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
source("plots.R")

sfInit(parallel=TRUE, cpus = 12)
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("MPs.R")
sfSource("calcEquilibriumDD.R")

options( warn = -1 )

hiCapMPs <- list( MP_hiCap = c("MP_hiCap","MP_hiCap"),
                  MP_hiCap23M = c("MP_hiCap23M","MP_hiCap23M"),
                  MP_hiCap23M.4B0 = c("MP_hiCap23M.4B0","MP_hiCap23M.4B0") )

loCapMps <- list( MP_loCap = c("MP_loCap","MP_loCap"),
                  MP_loCap23M = c("MP_loCap23M","MP_loCap23M"),
                  MP_loCap23M.4B0 = c("MP_loCap23M.4B0","MP_loCap23M.4B0") )

msyCapMPs    <- list( MP_msyCap = c("MP_msyCap","MP_msyCap"),
                      MP_msyCapF23M = c("MP_msyCapF23M","MP_msyCapF23M"),
                      MP_msyCapF23M.4B0 = c("MP_msyCapF23M.4B0","MP_msyCapF23M.4B0"))

aicMPs      <- list(  MP_aic_msyCap = c("MP_aic_msyCap","MP_aic_msyCap"),
                      MP_aic_msyCapF23M = c("MP_aic_msyCapF23M","MP_aic_msyCapF23M"),
                      MP_aic_msyCapF23M.4B0 = c("MP_aic_msyCapF23M.4B0","MP_aic_msyCapF23M.4B0") )

testMP      <- list(  MP_aic_msyCap = c("MP_aic_msyCap","MP_aic_msyCap") )



OMvec <- paste( "OM_", 1:15,sep = "" )
OMdvec <- paste( "OM_", 1:15,"d",sep = "" )

ROMvec <- paste( "OM_", 1:31, sep = "" )

# OMvec <- c( OMvec, "ROM_1d", "ROM_2d", "ROM_3d" )

testMSEs <- lapply( X = OMdvec, 
                    FUN = runCMPs,
                    assessInt = 2,
                    MPs = testMP,
                    checkMPs = TRUE,
                    projFolder = "testMP" )

# msyMSEs <- lapply( X = OMvec, 
#                   FUN = runCMPs,
#                   assessInt = 2,
#                   MPs = msyCapMPs,
#                   checkMPs = TRUE,
#                   projFolder = "msyMPs" )

# hiCapMSEs <- lapply(  X = OMvec, 
#                       FUN = runCMPs,
#                       assessInt = 2,
#                       MPs = hiCapMPs,
#                       checkMPs = TRUE,
#                       projFolder = "hiCaps" )

# loCapMSEs <- lapply(  X = OMvec, 
#                       FUN = runCMPs,
#                       assessInt = 2,
#                       MPs = loCapMPs,
#                       checkMPs = TRUE,
#                       projFolder = "loCaps" )

# aicMSEs <- lapply(  X = OMvec, 
#                     FUN = runCMPs,
#                     assessInt = 2,
#                     MPs = aicMPs,
#                     checkMPs = TRUE,
#                     projFolder = "aicTACs" )


