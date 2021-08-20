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
source("pewPMs.R")
source("plots.R")
source("tools.R")

makeGridconstU( eMult = seq(from = 6.8, to = 8.8, by = .4),
                wMult = seq(from = 1.85, to = 2.35, by = .1),
                outFile = "autoConstUgridMPs.R" )

source("autoConstUgridMPs.R")


                  
projFolder <- "conU_retune_grid2"

if(!dir.exists(file.path("MSEs",projFolder)))
  dir.create(file.path("MSEs",projFolder))

saveRDS(gridMPs, file = file.path("MSEs",projFolder,"gridMPs.rds"))

OMdvec <- 1:48

gridUmultMSEs <- lapply(  X = OMdvec, 
                          FUN = runCMPs,
                          MPs = gridMPs,
                          checkMPs = TRUE,
                          reloadABT = FALSE,
                          projFolderName = projFolder )


perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )

gridMPs.df <- makeMP.df_conU( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      OMs = OMdvec,
                                      projFolder = projFolder)

write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))


#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# source("tuneEastMmult.R")