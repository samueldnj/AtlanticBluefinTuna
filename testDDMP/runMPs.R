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

sfInit(parallel=TRUE, cpus= detectCores() - 2)
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("calcEquilibriumDD.R")

testMPs <- list(  loCap = c("MP_loCap","MP_loCap"),
                  hiCap = c("MP_hiCap","MP_hiCap"),
                  loCap23M = c("MP_loCap23M","MP_loCap23M"),
                  hiCap23M = c("MP_hiCap23M","MP_hiCap23M") )


# MSE_OM1 <- new( Class     = "MSE",
#                 OM        = OM_1,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM1, file = "MSEs/MSE_OM1.Rdata")

# MSE_OM2 <- new( Class     = "MSE",
#                 OM        = OM_2,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM2, file = "MSEs/MSE_OM2.Rdata")

# MSE_OM3 <- new( Class     = "MSE",
#                 OM        = OM_3,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM3, file = "MSEs/MSE_OM3.Rdata")

# MSE_OM4 <- new( Class     = "MSE",
#                 OM        = OM_4,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM4, file = "MSEs/MSE_OM4.Rdata")

# MSE_OM5 <- new( Class     = "MSE",
#                 OM        = OM_5,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM5, file = "MSEs/MSE_OM5.Rdata")

MSE_OM6 <- new( Class     = "MSE",
                OM        = OM_6,
                Obs       = Perfect_Obs,
                MPs       = testMPs,
                interval  = 3,
                IE        = "Overage_10" )

save(MSE_OM6, file = "MSEs/MSE_OM6.Rdata")

# MSE_OM7 <- new( Class     = "MSE",
#                 OM        = OM_7,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM7, file = "MSEs/MSE_OM7.Rdata")


# MSE_OM8 <- new( Class     = "MSE",
#                 OM        = OM_8,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM8, file = "MSEs/MSE_OM8.Rdata")


# MSE_OM9 <- new( Class     = "MSE",
#                 OM        = OM_9,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM9, file = "MSEs/MSE_OM9.Rdata")

# MSE_OM10 <- new( Class     = "MSE",
#                 OM        = OM_10,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM10, file = "MSEs/MSE_OM10.Rdata")

# MSE_OM11 <- new( Class     = "MSE",
#                 OM        = OM_11,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM11, file = "MSEs/MSE_OM11.Rdata")

# MSE_OM12 <- new( Class     = "MSE",
#                 OM        = OM_12,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM12, file = "MSEs/MSE_OM12.Rdata")

# MSE_OM13 <- new( Class     = "MSE",
#                 OM        = OM_13,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM13, file = "MSEs/MSE_OM13.Rdata")

# MSE_OM14 <- new( Class     = "MSE",
#                 OM        = OM_14,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM14, file = "MSEs/MSE_OM14.Rdata")

# MSE_OM15 <- new( Class     = "MSE",
#                 OM        = OM_15,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM15, file = "MSEs/MSE_OM15.Rdata")