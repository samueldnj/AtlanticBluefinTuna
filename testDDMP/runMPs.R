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

source("MPs.R")

#loCap <- list( MM=c("MP_loCap","MP_loCap") )
#MSE_loCap <- new( Class    = 'MSE', 
#                  OM       = OM_example,
#                  Obs      = Perfect_Obs,
#                  MPs      = loCap,
#                  interval = 3,
#                  IE       = "Overage_10")
#save(MSE_loCap,file="MSEs/MSE_loCap.Rdata")
#
#hiCap <- list( MM=c("MP_hiCap","MP_hiCap") )
#MSE_hiCap <- new( Class    = 'MSE', 
#                  OM       = OM_example,
#                  Obs      = Perfect_Obs,
#                  MPs      = hiCap,
#                  interval = 3,
#                  IE       = "Overage_10")
#save(MSE_hiCap,file="MSEs/MSE_hiCap.Rdata")

#loCap23M <- list( MM=c("MP_loCap23M","MP_loCap23M") )
#MSE_loCap23M <- new( Class    = 'MSE', 
#                    OM       = OM_example,
#                    Obs      = Perfect_Obs,
#                    MPs      = loCap23M,
#                    interval = 3,
#                    IE       = "Overage_10")
#save(MSE_loCap23M,file="MSEs/MSE_loCap23M.Rdata")

hiCap23M <- list( MM=c("MP_hiCap23M","MP_hiCap23M") )
MSE_hiCap23M <- new( Class    = 'MSE', 
                    OM       = OM_example,
                    Obs      = Perfect_Obs,
                    MPs      = hiCap23M,
                    interval = 3,
                    IE       = "Overage_10")
save(MSE_hiCap23M,file="MSEs/MSE_hiCap23M.Rdata")

