
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

optimEmpMP <- runSPSA_MPoptim(  Niter = 10,
                                c = 0.5,
                                a = 0.01,
                                gamma = 0.101,
                                alpha = 0.602,
                                OMs = c(1,2,4),
                                nCores = 3,
                                seed = 1234,
                                par = TRUE )


save( optimEmpMP, file = "testSPSA.RData" )