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
source("tools.R")
source("plots.R")

source("phasedCMPs.R")

EA_phaseMPs <- list( # EA
                    EA_noPhz    = c("EA_E","EA_W"),
                    EA_Phz5     = c("EA_E_Phz5","EA_W_Phz5"),
                    EA_Phz10    = c("EA_E_Phz10","EA_W_Phz10") )
                

projFolder <- "EA_phaseIn_stoch"

if(!dir.exists(file.path("MSEs",projFolder)))
  dir.create(file.path("MSEs",projFolder))

saveRDS(EA_phaseMPs, file = file.path("MSEs",projFolder,"mpList.rds"))
                  

OMdvec <- c(1:48)

phzEA_MSEs <- lapply( X = OMdvec, 
                      FUN = runCMPs,
                      MPs = EA_phaseMPs,
                      checkMPs = TRUE,
                      reloadABT = FALSE,
                      det = FALSE,
                      projFolderName = projFolder )

perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )

saveRDS(perfMetricList, file = file.path("MSEs",projFolder,"PMlist.rds"))

gridMPs.df <- makeMP.df_EA( projFolder =  projFolder)

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      projFolder = projFolder)

write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))


