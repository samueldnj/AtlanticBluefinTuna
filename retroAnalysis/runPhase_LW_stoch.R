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

LW_phaseMPs <- list( # LW (conU)
                    conU_noPhz  = c("conU_E", "conU_W"),
                    conU_Phz5   = c("conU_E_Phz5", "conU_W_Phz5"),
                    conU_Phz10  = c("conU_E_Phz10", "conU_W_Phz10") )
                

projFolder <- "LW_phaseIn_stoch"

if(!dir.exists(file.path("MSEs",projFolder)))
  dir.create(file.path("MSEs",projFolder))

saveRDS(LW_phaseMPs, file = file.path("MSEs",projFolder,"mpList.rds"))
                  

OMdvec <- c(1:48)

phzLW_MSEs <- lapply( X = OMdvec, 
                      FUN = runCMPs,
                      MPs = LW_phaseMPs,
                      checkMPs = TRUE,
                      reloadABT = FALSE,
                      det = FALSE,
                      projFolderName = projFolder )

perfMetricList <- calcPerfMetrics(  projFolder = projFolder, 
                                    OMs = OMdvec )

saveRDS(perfMetricList, file = file.path("MSEs",projFolder,"PMlist.rds"))

gridMPs.df <- makeMP.df_conU( projFolder =  projFolder, filename = "mpList.rds")

gridMPs.df$multEast  <- mE
gridMPs.df$multWest  <- mW

gridPerfMetrics.df <- addPerfMetrics( gridMPs.df = gridMPs.df,
                                      projFolder = projFolder)

write.csv(gridPerfMetrics.df, file = file.path("MSEs",projFolder,"perfMetrics.csv"))

