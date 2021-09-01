# --------------------------------------------------------------------------
# runMPs_conU.R
# 
# Sources function scripts, makes a grid of CMPs and
# applies to the full grid of operating models
#
# Author: Samuel D N Johnson
# Date: June 15, 2021
#
# --------------------------------------------------------------------------

# Load the ABTMSE package
source("initTest.R")
source("runCMPs.R")
source("pewPMs.R")
source("plots.R")
source("tools.R")
source("tunedConUMPs.R")


                  
projFolder <- "conU_tuned"

if(!dir.exists(file.path("MSEs",projFolder)))
  dir.create(file.path("MSEs",projFolder))

saveRDS(gridMPs, file = file.path("MSEs",projFolder,"gridMPs.rds"))

OMdvec <- 1:48

# gridUmultMSEs <- lapply(  X = OMdvec, 
#                           FUN = runCMPs,
#                           MPs = gridMPs,
#                           checkMPs = TRUE,
#                           reloadABT = FALSE,
#                           projFolderName = projFolder )


perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )

gridMPs.df <- makeMP.df_conU( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      OMs = OMdvec,
                                      projFolder = projFolder)

write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))


#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# source("tuneEastMmult.R")