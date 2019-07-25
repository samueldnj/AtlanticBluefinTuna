# --------------------------------------------------------------------------
# testMP.R
# 
# Loads the ABTMSE package (local modified version) and 
# runs a test version of the TMB DD model based procedure, 
# creating a plot of AM performance
#
# Author: Samuel D N Johnson
# Date: July 16, 2019
#
# --------------------------------------------------------------------------

# Load the ABTMSE package
source("initTest.R")
source("assessDDMP.R")
source("calcEquilibriumDD.R")
source("MPs.R")
source("plots.R")

sfInit(parallel=TRUE, cpus= 2 )
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("calcEquilibriumDD.R")

# Squelch warnings for Rmd output
options( warn = -1 )

if(!dir.exists("outTables"))
  dir.create("outTables")

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

OMvec <- paste( "OM_", 1:15,"d",sep = "" )
OMvec <- c( OMvec, "ROM_1d", "ROM_2d", "ROM_3d" )

ROMvec <- paste( "ROM_", c(18:31), "d", sep = "" )

lapply( X = OMvec, 
        FUN = runCMPs,
        assessInt = 2,
        MPs = msyCapMPs )


# lapply( X = ROMvec, 
#         FUN = runCMPtest,
#         assessInt = 2,
#         MPs = testMPs )



# parallel::parLapply(  cl = ROMclust,
#                       X = ROMvec,
#                       fun = runCMPs,
#                       assessInt = 2,
#                       MPs = testMPs )

# stopCluster(ROMclust)

# sfInit(parallel=TRUE, cpus= 6 )
# sfLibrary( ABTMSE )
# sfLibrary( TMB )
# sfClusterCall("loadABT")
# sfSource("assessDDMP.R")
# sfSource("calcEquilibriumDD.R")

# runCMPtest( OM = "OM_1",
#             MPs = testMPs,
#             assessInt = 2 )



