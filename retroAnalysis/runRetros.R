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
source("makeGridMPs_BR.R")
source("tools.R")
source("plots.R")
source("BR3.R")
source("constU.R")
source("EA_1.R")
source("Fzero1.R")

library("corrplot")

# Load data sets
dsetE <- readRDS("dsetE.rds")
dsetW <- readRDS("dsetW.rds")

# Let's see what happens
yrs <- seq(from = 1965, by = 1, length.out = 109 )
retroYrs <- 46:56
# Array for retrospective TACs, a is area, y is yr
retroTACs_may <- array(0, dim = c(4,2,length(retroYrs)))

for(yIdx in 1:length(retroYrs))
{
  yr <- retroYrs[yIdx]
  # BR
  retroTACs_may[1,1,yIdx] <- BR_E3s(  x = 1, 
                                      dset = dsetE, 
                                      lastyr = yr,
                                      alp=3.24,
                                      TACcap = 45,
                                      pullD = FALSE )

  retroTACs_may[1,2,yIdx] <- BR_W3s(  x = 1, 
                                      dset = dsetW, 
                                      lastyr = yr,
                                      bet=0.77,
                                      gam=10.0,
                                      TACcap = 2.35,
                                      pullD = FALSE )

  # conU
  retroTACs_may[2,1,yIdx] <- ConstU_E(  x=1,
                                        dset=dsetE,
                                        IndexE=2,
                                        yrs4mean=3,
                                        lastyr = yr,
                                        target_yr=55,
                                        deltaE_up=0.5,
                                        deltaE_down=0.5,
                                        multiplierE=6.13)

  retroTACs_may[2,2,yIdx] <- ConstU_W(  x=1,
                                        dset=dsetW,
                                        IndexW=3,
                                        IndexE=2,
                                        yrs4mean=3,
                                        lastyr=yr,
                                        target_yr=55,
                                        deltaW_down=0.5,
                                        deltaW_up=0.5,
                                        multiplierW=1.46)

  # EA_1
  retroTACs_may[3,1,yIdx] <- EA_1_E(  x = 1,
                                      dset = dsetE,
                                      lastyr = yr,
                                      Targ=1.32,
                                      Deltaup=0.15,
                                      Gamma = 1,
                                      Deltadown=0.15,
                                      yrs4mean=3)

  # EA_1
  retroTACs_may[3,2,yIdx] <- EA_1_W(  x = 1,
                                      dset = dsetW,
                                      lastyr = yr,
                                      Targ=1.5,
                                      Deltaup=0.15,
                                      Gamma = 1,
                                      Deltadown=0.15,
                                      yrs4mean=3)

  # AH_E
  retroTACs_may[4,1,yIdx] <- Fzero1E( x=1, 
                                      dset = dsetE, 
                                      yrsmth = 3, 
                                      lim = c(.1,.4,1), 
                                      ny = yr,
                                      alp = c(0.75, 0.6, 0.5),
                                      IndexID_y = 1, 
                                      IndexID_m = 12, 
                                      IndexID_o = 2, 
                                      nyears=55,
                                      q = 0.385,
                                      IndexID_bio = 2)

  # AH_W
  retroTACs_may[4,2,yIdx] <- Fzero1W( x = 1, 
                                      dset = dsetW, 
                                      yrsmth = 3, 
                                      lim = c(.1,.4,1), 
                                      alp = c(0.75, 0.6, 0.5),
                                      ny = yr,
                                      IndexID_y = 13, 
                                      IndexID_m = 12, 
                                      IndexID_o = 14, 
                                      nyears=55,
                                      q = 0.306, 
                                      IndexID_bio = 14)
}

# Plot
graphics.off()
pdf(file = "figs/retroTACs.pdf",
        height = 11, width = 8.5)        
plotRetros(dsetE,dsetW,retroTACs_may)
dev.off()

nCMPs <- dim(retroTACs_may)[1]
eastCatMat <- matrix(0, nrow = dim(retroTACs_may)[3], ncol = nCMPs + 1 )
westCatMat <- matrix(0, nrow = dim(retroTACs_may)[3], ncol = nCMPs + 1 )

eastCatMat[,1] <- dsetE$Cobs[1,retroYrs]
eastCatMat[,1 + 1:nCMPs] <- t(retroTACs_may[,1,])

westCatMat[,1] <- dsetW$Cobs[1,retroYrs]
westCatMat[,1 + 1:nCMPs] <- t(retroTACs_may[,2,])

eastCatMat <- eastCatMat / 1e6
westCatMat <- westCatMat / 1e6

colnames( eastCatMat) <- c("Catch", "BR","LW","EA","AH")
colnames( westCatMat) <- c("Catch", "BR","LW","EA","AH")

corEast <- cor(eastCatMat, use = "pairwise.complete.obs")
corWest <- cor(westCatMat, use = "pairwise.complete.obs")

graphics.off()
pdf(file = "figs/eastCorPlot.pdf", width = 8, height = 9)
corrplot.mixed(corEast, lower = 'square', upper = "number")
mtext(side = 3, text=  "East Catch and CMP TACs")
dev.off()

graphics.off()
pdf(file = "figs/westCorPlot.pdf", width = 8, height = 9)
corrplot.mixed(corWest, lower = 'square', upper = "number")
mtext(side = 3, text=  "West Catch and CMP TACs")
dev.off()
