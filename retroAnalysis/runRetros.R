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


# Load data sets
dsetE <- readRDS("dsetE.rds")
dsetW <- readRDS("dsetW.rds")

# Let's see what happens
yrs <- seq(from = 1965, by = 1, length.out = 109 )
retroYrs <- 46:56
# Array for retrospective TACs, a is area, y is yr
retroTACs_may <- array(0, dim = c(3,2,length(retroYrs)))

for(yIdx in 1:length(retroYrs))
{
  yr <- retroYrs[yIdx]
  # BR
  retroTACs_may[1,1,yIdx] <- BR_E3s(  x = 1, 
                                      dset = dsetE, 
                                      lastyr = yr,
                                      alp=1.69,
                                      TACcap = 45,
                                      pullD = FALSE )

  retroTACs_may[1,2,yIdx] <- BR_W3s(  x = 1, 
                                      dset = dsetW, 
                                      lastyr = yr,
                                      bet=0.95,
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
                                        multiplierE=7.35)

  retroTACs_may[2,2,yIdx] <- ConstU_W(  x=1,
                                        dset=dsetW,
                                        IndexW=3,
                                        IndexE=2,
                                        yrs4mean=3,
                                        lastyr=yr,
                                        target_yr=55,
                                        deltaW_down=0.5,
                                        deltaW_up=0.5,
                                        multiplierW=7.90)

  # EA_1
  retroTACs_may[3,1,yIdx] <- EA_1_E(  x = 1,
                                      dset = dsetE,
                                      lastyr = yr,
                                      Targ=1.32,
                                      Deltaup=0.15,
                                      Deltadown=0.15,
                                      yrs4mean=3)

  # EA_1
  retroTACs_may[3,2,yIdx] <- EA_1_W(  x = 1,
                                      dset = dsetW,
                                      lastyr = yr,
                                      Targ=2.0,
                                      Deltaup=0.15,
                                      Deltadown=0.15,
                                      yrs4mean=3)
}


par(mfrow = c(2,1), mar = c(1,.1,1,.1), oma = c(4,4,2,2) )

mCols <- RColorBrewer::brewer.pal(n = 3, "Dark2")

plot( x = range(yrs[retroYrs]), 
      y = c(0,max(dsetE$Cobs[1,retroYrs],retroTACs_may[,1,]))/1e3,
      type = "n", axes = FALSE )
  axis(side = 2, las = 1)
  grid()
  box()
  lines(  x = yrs[1:57],
          y = dsetE$Cobs[1,1:57]/1e3,
          lty = 1, lwd = 3 )
  for( m in 1:3 )
    points( x = yrs[retroYrs],
            y = retroTACs_may[m,1,]/1e3,
            pch = 16, col = mCols[m], cex = 1.2 )

  legend(x = "topleft", col = mCols, pch = 16, cex = 1.2,
          legend = c("BR","LW","EA"), bty = "n")
  mtext( side = 3, text = "East Area")

plot( x = range(yrs[retroYrs]), 
      y = c(0,max(dsetW$Cobs[1,retroYrs],retroTACs_may[,2,]))/1e3,
      type = "n", axes = FALSE )
  axis( side = 1 )
  axis(side = 2, las = 1)
  grid()
  box()
  lines(  x = yrs[1:57],
          y = dsetW$Cobs[1,1:57]/1e3,
          lty = 1, lwd = 3 )
  for( m in 1:3 )
    points( x = yrs[retroYrs],
            y = retroTACs_may[m,2,]/1e3,
            pch = 16, col = mCols[m], cex = 1.2 )

  mtext(side =3, text = "West Area")

  mtext(side = 2, outer = TRUE, text = "Catch (t)", line = 3 )
  mtext(side = 1, outer = TRUE, text = "Year", line = 3 )

