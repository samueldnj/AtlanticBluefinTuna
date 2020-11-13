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
library(parallel)

# Load the ABTMSE package
source("initTest.R")
source("assessDDMP.R")
source("calcEquilibriumDD.R")
source("MPs.R")
source("plots.R")


#for( i in 1:96 )
#{
#i <- 1
#  tempMSE <- new('MSE',OM=get(paste0('OM_',i,'d')),MPs=list(MP_mix = c("MP_noCapF23M","MP_msyCapF23M")),
#                 Obs=Perfect_Obs, Deterministic=TRUE)
#  saveRDS(tempMSE,file=paste0("MSEs/LFR-DelayDiff/MSE_R_",i,".rda"))
#}
#
#for( i in 1:12 )
#{
#  tempMSE <- new('MSE',OM=get(paste0('ROM_',i,'d')),MPs=list(MP_mix = c("MP_noCapF23M","MP_msyCapF23M")),
#                 Obs=Perfect_Obs, Deterministic=TRUE)
#  saveRDS(tempMSE,file=paste0("MSEs/LFR-DelayDiff/MSE_R_",i,".rda"))
#}


#######options( warn = -1 )
#######
testMPs    <- list( MP_0.1 = c("MP_noCapFM1","MP_msyCapF23M"),
                    #MP_0.025 = c("MP_noCapFM025","MP_msyCapF23M"),
                    MP_0.01 = c("MP_noCapFM01","MP_msyCapF23M"),
                    MP_last10 = c("MP_noCapFMlast10","MP_msyCapF23M")
                   )
#######
OMdvec <- 1:7
library(parallel)
omClust <- makeCluster( floor(detectCores()/2) )

# mixCapMSE <- lapply(  X = OMdvec, 
#                       FUN = runCMPs,
#                       assessInt = 2,
#                       MPs = testMPs,
#                       checkMPs = TRUE,
#                       projFolder = "oct22" )

testMSE <- parLapply( cl = omClust,
                      X = OMdvec, 
                      FUN = runCMPs,
                      assessInt = 2,
                      MPs = testMPs,
                      checkMPs = TRUE,
                      projFolder = "samTest" )

stopCluster(ROMclust)


