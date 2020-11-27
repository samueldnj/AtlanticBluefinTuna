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
source("plots.R")

sfInit(parallel=TRUE, cpus = 2)
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("MPs.R")
sfSource("calcEquilibriumDD.R")

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
# testMPs    <- list( MP_0.1 = c("MP_noCapFM1","MP_msyCapF23M"),
#                     #MP_0.025 = c("MP_noCapFM025","MP_msyCapF23M"),
#                     MP_0.01 = c("MP_noCapFM01","MP_msyCapF23M"),
#                     MP_last10 = c("MP_noCapFMlast10","MP_msyCapF23M")
#                    )
#######


# lfrDDMPs <- list( lfrDD_001 = c("MP_noCap_F1.13M_last10","MP_msyCap_F1.09M_last10"),
#                   lfrDD_002 = c("MP_noCap_F1.13M_last10","MP_msyCap_F0.52M_last10"),
#                   lfrDD_003 = c("MP_noCap_F1.13M_last10","MP_msyCap_F0.23M_last10"))

initMPs <- c( DD_NoCap          = c("MP_noCapB0","MP_noCapB0"),
              DD_MSYCap         = c("MP_msyCapB0","MP_msyCapB0"),
              DD_NoCapFM        = c("MP_noCapFMB0","MP_noCapFMB0"),
              DD_MSYCapFM       = c("MP_msyCapFMB0","MP_msyCapFMB0"),
              DD_MixCapFM       = c("MP_noCapFMB0","MP_msyCapFMB0"),
              DD_MixCapFMlast10 = c("MP_noCapFMB0last10","MP_msyCapFMB0last10"),)

OMdvec <- 1:96

# mixCapMSE <- lapply(  X = OMdvec, 
#                       FUN = runCMPs,
#                       assessInt = 2,
#                       MPs = testMPs,
#                       checkMPs = TRUE,
#                       projFolder = "oct22" )

testMSE <- lapply(  X = OMdvec, 
                    FUN = runCMPs,
                    assessInt = 2,
                    MPs = lfrDDMPs,
                    checkMPs = TRUE,
                    projFolder = "lfrDDMPs_eBr30eq1.0" )


