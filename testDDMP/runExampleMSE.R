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

# Now, create an example dset and try to run the MP
dsetComb <- list( E = dset_example_East,
                  W = dset_example_West )

# First, run the assessment on the
# dset for 3 OMs
checkTAC <- assessDDmm( x = 1, dset = dsetComb, AMs = c(1,4,7), F23M=1 )


# Now define an MP list and run the MSE on OM_1
testMPs <- list(  baseMM    = c("assessDDmm","assessDDmm"),
                  loCap     = c("MP_loCap","MP_loCap"),
                  hiCap     = c("MP_hiCap","MP_hiCap"),
                  loCap23M  = c("MP_loCap23M","MP_loCap23M"),
                  hiCap23M  = c("MP_hiCap23M","MP_hiCap23M") )

sfInit(parallel=TRUE, cpus=detectCores()-1)
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("calcEquilibriumDD.R")


MSE_OM1d <- new(  'MSE', 
                  OM=OM_1d,
                  Obs=Perfect_Obs,
                  MPs=testMPs,
                  interval=3,
                  IE="Overage_10")

MSE_1 <- new( 'MSE', 
              OM=OM_1,
              Obs=Perfect_Obs,
              MPs=testMPs,
              interval=3,
              IE="Overage_10")


# MSE_2 <- new( 'MSE', 
#               OM=OM_2,
#               Obs=Good_Obs,
#               MPs=testMPs,
#               interval=3,
#               IE="Overage_10")
