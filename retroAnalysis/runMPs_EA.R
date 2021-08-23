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
source("pewPMs.R")
source("EA_1.R")
source("makeGridMPs_EA.R")
source("tools.R")
source("plots.R")

# makeGridBR( alp = 1.7,
#             bet = 0.9,
#             outFile = "autoBRgrid.R")

makeGridEA_targ(  eTarg = seq(from = 0.8, to = 1.8, by = 0.2),
                  wTarg = seq(from = 0.8, to = 1.8, by = 0.2),
                  eGamma = 1, wGamma = 1,
                  outFile = "autoEAgrid.R")

source("autoEAgrid.R")

projFolder <- "EA_tuneBr30_targ1"

if(!dir.exists(file.path("MSEs",projFolder)))
  dir.create(file.path("MSEs",projFolder))

saveRDS(gridMPs, file = file.path("MSEs",projFolder,"gridMPs.rds"))
                  

OMdvec <- c(1:48)

gridEAMSEs <- lapply(  X = OMdvec, 
                      FUN = runCMPs,
                      MPs = gridMPs,
                      checkMPs = TRUE,
                      reloadABT = FALSE,
                      projFolderName = projFolder )

perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )

saveRDS(perfMetricList, file = file.path("MSEs",projFolder,"PMlist.rds"))

gridMPs.df <- makeMP.df_EA_targ( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      projFolder = projFolder)

write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))

