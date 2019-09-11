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
source("empiricalMP.R")
source("MPs.R")
source("plots.R")

# sfInit(parallel=TRUE, cpus= 2 )
# sfLibrary( ABTMSE )
# sfLibrary( TMB )
# sfClusterCall("loadABT")
# sfSource("assessDDMP.R")
# sfSource("calcEquilibriumDD.R")

# Squelch warnings for Rmd output
options( warn = -1 )

if(!dir.exists("outTables"))
  dir.create("outTables")

testMPs    <- list( empMPtest_Mix  = c("empMPtest_Mean","empMPtest_min"),
                    empMP_loCap     = c("empMP_loCap","empMP_loCap") )


OMvec <- paste( "OM_", 1:15,"d",sep = "" )
# OMvec <- c( OMvec, "ROM_1d", "ROM_2d", "ROM_3d" )

# ROMvec <- paste( "ROM_", c(18:31), "d", sep = "" )

x <- lapply(  X = OMvec, 
              FUN = runCMPtest,
              assessInt = 2,
              MPs = testMPs )





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



