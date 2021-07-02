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
source("makeGridMPs_BR.R")
source("tools.R")
source("plots.R")

# makeGridBR( alp = 1.7,
#             bet = 0.9,
#             outFile = "autoBRgrid.R")

makeGridBR( alp = seq(from = 7, to = 9, by = 0.05),
            bet = seq(from = 0.8, to = 1.6, by = 0.2),
            eCap = 50,
            wCap = Inf,
            outFile = "autoBRgrid.R")

source("autoBRgrid.R")

projFolder <- "BR_initGrid_hiCaps2"

if(!dir.exists(file.path("MSEs",projFolder)))
  dir.create(file.path("MSEs",projFolder))

saveRDS(gridMPs, file = file.path("MSEs",projFolder,"gridMPs.rds"))
                  

OMdvec <- c(17:32)

gridBRMSEs <- lapply(  X = OMdvec, 
                      FUN = runCMPs,
                      MPs = gridMPs,
                      checkMPs = TRUE,
                      reloadABT = FALSE,
                      projFolderName = projFolder )

perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )

saveRDS(perfMetricList, file = file.path("MSEs",projFolder,"PMlist.rds"))

gridMPs.df <- makeMP.df_conU( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      projFolder = projFolder)

write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))

