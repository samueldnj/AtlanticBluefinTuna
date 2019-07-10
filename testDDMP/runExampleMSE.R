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

# Now, create an example dset and try to run the MP
dsetComb <- list( E = dset_example_East,
                  W = dset_example_West )

# First, run the assessment on the
# dset for 3 OMs
checkTAC <- assessDDmm( x = 1, dset = dsetComb, AMs = c(1,4,7), F23M=1 )


# Now define an MP list and run the MSE on OM_1
testMPs <- list(  #SS = c("UMSY","UMSY"),
                  MM = c("assessDDmm","assessDDmm") )
MSE_ex <- new(  'MSE', 
                OM=OM_example,
                Obs=Perfect_Obs,
                MPs=testMPs,
                interval=3,
                IE="Overage_10")

# MSE_1 <- new( 'MSE', 
#               OM=OM_1,
#               Obs=Perfect_Obs,
#               MPs=testMPs,
#               interval=3,
#               IE="Overage_10")


# MSE_2 <- new( 'MSE', 
#               OM=OM_2,
#               Obs=Good_Obs,
#               MPs=testMPs,
#               interval=3,
#               IE="Overage_10")
