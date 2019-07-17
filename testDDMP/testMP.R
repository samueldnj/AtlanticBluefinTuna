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

sfInit(parallel=TRUE, cpus= 6 )
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("calcEquilibriumDD.R")

# Squelch warnings for Rmd output
options( warn = -1 )

if(!dir.exists("outTables"))
  dir.create("outTables")

testMPs <- list(  test = c("MP_testMean","MP_testMean") )

OMlist <- c("OM_1d","OM_2d", "OM_3d", "OM_4d", "ROM_1d")

# lapply( X = OMlist, 
#         FUN = runCMPtest,
#         assessInt = 2,
#         MPs = testMPs )

runCMPtest( OM = "OM_1",
            MPs = testMPs,
            assessInt = 2 )



