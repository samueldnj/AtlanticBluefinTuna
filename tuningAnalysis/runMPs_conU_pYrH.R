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

makeGridconstU( eMult = seq(from = 8.2, to = 8.4, by = 0.05),
                wMult = seq(from = 8.9, to = 9.1, by = 0.05),
                outFile = "autoConU_pYrHgridMPs.R" )

source("autoConU_pYrHgridMPs.R")


                  
projFolder <- "conU_pYrH_RefineGrid3"

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