# tuneEastMmult.R
# Runs a grid search over scalars for F=kM
# and saves results


# Load the ABTMSE package
source("initTest.R")
source("assessDDMP.R")
source("calcEquilibriumDD.R")
source("MPs.R")
source("plots.R")

saveDir <- "eastNLLgrid"
gridScript <- paste(saveDir,".R",sep = "")
OMdvec <- 1:96

westMP <- "MP_msyCapF23M"

# Make MPs
source("makeGridMPs.R")
nllWts <- c(0,0.01,0.025,0.05,0.1,.5,1.0,"last10")

MPnames <- makeGridMPs( M = 1,
                        eastCap = Inf,
                        westCap = Inf,
                        nllMults = nllWts,
                        MPfile = gridScript )

source(gridScript)

sfInit(parallel=TRUE, cpus = 2)
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("MPs.R")
sfSource(gridScript)
sfSource("calcEquilibriumDD.R")



nMPs <- length(MPnames)
testMPs <- vector(mode = "list", length = nMPs )
names(testMPs) <- MPnames

for( k in 1:length(MPnames) )
{
  testMPs[[MPnames[k]]] <- c(MPnames[k],westMP)
}

testMSE <- lapply(  X = OMdvec, 
                    FUN = runCMPs,
                    assessInt = 2,
                    MPs = testMPs,
                    checkMPs = TRUE,
                    projFolder = saveDir )


