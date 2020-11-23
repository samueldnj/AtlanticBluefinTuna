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

# sfInit(parallel=TRUE, cpus = 2 )
# sfLibrary( ABTMSE )
# sfLibrary( TMB )
# sfClusterCall("loadABT")
# sfSource("empiricalMP.R")
# sfSource("MPs.R")
# sfSource("plots.R")

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


                  

OMdvec <- paste( "OM_", (1:96),"d",sep = "" )

fmMSEs <- lapply( X = OMdvec, 
                  FUN = runCMPs,
                  assessInt = 2,
                  MPs = noCapTrendMPs,
                  checkMPs = TRUE,
                  projFolderName = "noCapTrendMPs" )

#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")

source("tuneEastMmult.R")
