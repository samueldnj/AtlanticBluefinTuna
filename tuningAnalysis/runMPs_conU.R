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
source("runCMPs.R")
source("makeGridMPs_constU.R")
source("plots.R")
source("tools.R")

makeGridconstU( eMult = seq(from = 1, to = 5, by = 1),
                wMult = seq(from = 7, to = 12, by = 1),
                outFile = "autoConstUgridMPs.R" )

source("autoConstUgridMPs.R")

saveRDS(gridMPs, file = file.path("MSEs",projFolder,"gridMPs.rds"))
                  
projFolder <- "testConstU_allOMs"

OMdvec <- 1:48

gridUmultMSEs <- lapply(  X = OMdvec, 
                          FUN = runCMPs,
                          MPs = gridMPs,
                          checkMPs = TRUE,
                          reloadABT = FALSE,
                          projFolderName = projFolder )

pH30_E <- lapply(X = gridUmultMSEs, FUN = pH30, pp = 1)
pH30_W <- lapply(X = gridUmultMSEs, FUN = pH30, pp = 2)

PGK_E <- lapply(X = gridUmultMSEs, FUN = PGK, pp = 1)
PGK_W <- lapply(X = gridUmultMSEs, FUN = PGK, pp = 2)

yrHealth_E <- lapply(X = gridUmultMSEs, FUN = tfHealthy_t, pp = 1)
yrHealth_W <- lapply(X = gridUmultMSEs, FUN = tfHealthy_t, pp = 2)

Br30_E <- lapply(X = gridUmultMSEs, FUN = Br30, pp = 1)
Br30_W <- lapply(X = gridUmultMSEs, FUN = Br30, pp = 2)

perfMetricList <- list( pH30_E = pH30_E,
                        pH30_W = pH30_W,
                        PGK_E = PGK_E,
                        PGK_W = PGK_W,
                        yrHealth_E = yrHealth_E,
                        yrHealth_W = yrHealth_W,
                        Br30_E = Br30_E,
                        Br30_W = Br30_W )

saveRDS(perfMetricList, file = file.path("MSEs",projFolder,"PMlist.rds"))

gridMPs.df <- makeMP.df_conU( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      projFolder = projFolder)

write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))


#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# source("tuneEastMmult.R")