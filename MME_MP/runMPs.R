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

noCapMPs <- list( #emp_msyCap   = c("emp_msyCap","emp_msyCap"),
                  #emp_msyCapB0 = c("emp_msyCapB0","emp_msyCapB0"),
                  #emp_msyCapFM = c("emp_msyCapFM","emp_msyCapFM")
                  #emp_msyCapFMB0 = c("emp_msyCapFMB0","emp_msyCapFMB0")
                  emp_noCapFMB0 = c("emp_noCapFMB0","emp_noCapFMB0")
                )
                  

OMdvec <- paste( "OM_", (1:96),"d",sep = "" )

fmMSEs <- lapply( X = OMdvec, 
                  FUN = runCMPs,
                  assessInt = 2,
                  MPs = noCapMPs,
                  checkMPs = TRUE,
                  projFolderName = "sep17" )

#z <- runCMPs("OM_87d",noCapMPs,checkMPs=1,projFolderName="sep12")