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
source("empiricalMP.R")
source("MPs.R")
source("plots.R")

sfInit(parallel=TRUE, cpus = 2 )
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("empiricalMP.R")
sfSource("MPs.R")
sfSource("plots.R")

options( warn = -1 )

noCapTrendMPs <- list(  #emp_msyCap   = c("emp_msyCap","emp_msyCap"),
                        #emp_msyCapB0 = c("emp_msyCapB0","emp_msyCapB0"),
                        #emp_msyCapF23M = c("emp_msyCapF23M","emp_msyCapF23M")
                        #emp_msyCapF23MB0 = c("emp_msyCapF23MB0","emp_msyCapF23MB0"),
                        emp_noCapTrendTAC = c("emp_noCapTrendTAC","emp_noCapTrendTAC"),
                        emp_noCapTrendWtdTAC = c("emp_noCapTrendWtdTAC","emp_noCapTrendWtdTAC"),
                        emp_noCapTrendPoisTAC = c("emp_noCapTrendPoisTAC","emp_noCapTrendPoisTAC")
                )

noCapMPs <- list( emp_noCap   = c("emp_noCap","emp_noCap"),
                  emp_noCapB0 = c("emp_noCapB0","emp_noCapB0"),
                  emp_noCapF23M = c("emp_noCapF23M","emp_noCapF23M"),
                  emp_noCapF23MB0 = c("emp_noCapF23MB0","emp_noCapF23MB0"),
                  emp_noCapTrendTAC = c("emp_noCapTrendTAC","emp_noCapTrendTAC")
                )

baseMPs <- list(  Emp_msyCap = c("emp_msyCap","emp_msyCap"),
                  Emp_noCap = c("emp_noCap","emp_noCap"),
                  Emp_msyCapB0 = c("emp_msyCapB0","emp_msyCapB0"),
                  Emp_noCapB0 = c("emp_noCapB0","emp_noCapB0"),
                  Emp_msyCapFMB0 = c("emp_msyCapF.3MB0","emp_msyCapF.3MB0"),
                  Emp_noCapFMB0 = c("emp_noCapF.3MB0","emp_noCapF.3MB0"),
                  emp_noCapFMB0_trWtd4x0.1 = c("emp_noCapFMB0_trWtd4x0.1","emp_noCapFMB0_trWtd4x0.1"),
                  emp_noCapFMB0_tr4x0.1 = c("emp_noCapFMB0_tr4x0.1","emp_noCapFMB0_tr4x0.1"))

empMPs <- list( lfrEM_001 = c("emp_noCap_F.3MB0_tr4x0.2","emp_noCap_F1MB0"),
                lfrEM_002 = c("emp_noCap_F.3MB0_tr4x0.2","emp_noCap_F23MB0_trWtd4x0.1"),
                lfrEM_003 = c("emp_noCap_F.3MB0_tr4x0.2","emp_msyCap_F.6MB0") )

fmMSEs <- lapply( X = 68:96, 
                  FUN = runCMPs,
                  assessInt = 2,
                  MPs = empMPs,
                  checkMPs = TRUE,
                  projFolderName = "lfrEM_eBr30eq1.0" )                  

OMdvec <- 1:96

fmMSEs <- lapply( X = OMdvec, 
                  FUN = runCMPs,
                  assessInt = 2,
                  MPs = baseMPs,
                  checkMPs = TRUE,
                  projFolderName = "baseEmpMPs" )

#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

# source("tuneWestMmult.R")
# source("tuneWestTrendPhi.R")
