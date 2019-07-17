# --------------------------------------------------------------------------
# plots.R
# 
# plots for testing the DD MP
#
# Author: Samuel D N Johnson
# Date: July 16, 2019
#
# --------------------------------------------------------------------------



plot_AMfits <- function(  simNum      = 1,
                          MSEobj      = MSEtest_OM2d,
                          tables      = checkTables,
                          MPnum       = 2,
                          interval    = 2  )
{
  # First, pull SSB from the MSEobj
  SSB <- MSEobj@SSB[MPnum,simNum,,]/1e6

  # Now we have a matrix of biomasses with
  # stock in the rows (E/W) and time in the columns


  # Now, get the right checkTable
  checkTable <- tables[[simNum]]

  # Count AMs
  AMs   <- unique(checkTable$AM)
  nAMs  <- length(AMs)

  # Count years
  yrs   <- 1965:2070
  tMP   <- 2017

  MPyrs <- seq( from  = tMP, to = max(yrs) - interval,
                by = interval )

  yrCol <- rep( MPyrs, rep(5,length(MPyrs)) )

  checkTable$yr   <- rep( MPyrs, rep(5,length(MPyrs)) )

  AMcols <- RColorBrewer::brewer.pal(n = length(AMs), "Dark2")
  checkTable$col  <- AMcols

  AMlabels <- paste( "AM", as.character(AMs), sep = "_" )

  # Set up the E/W plot
  par( mfrow = c(2,1), mar =c(2,1,2,1), oma = c(4,4.5,2,1) )

  plot( x = range(yrs), y = c(0,max(SSB[1,],checkTable$Bnext_E,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1 )
    grid()
    abline( v = tMP, col = "grey40", lty = 2, lwd = 3 )
    lines( x = yrs, y = SSB[1,], col = "red", lwd = 3 )
    points( x = checkTable$yr + 1, y = checkTable$Bnext_E,
            pch = 16, col = checkTable$col )
    mtext( side = 3, font = 2, text = "East Stock")

  plot( x = range(yrs), y = c(0,max(SSB[2,],checkTable$Bnext_W,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1 )
    grid()
    abline( v = tMP, col = "grey40", lty = 2, lwd = 3 )
    lines( x = yrs, y = SSB[2,], col = "red", lwd = 3 )
    points( x = checkTable$yr + 1, y = checkTable$Bnext_W,
            pch = 16, col = checkTable$col )
    mtext( side = 3, font = 2, text = "West Stock")
    legend( x = "topleft", legend = AMlabels,
            pch = 16, col = AMcols, bty = "n" )

  mtext(  side = 2, text = "Spawning Biomass (kt)", 
          outer = T, line = 2.5, cex = 2 )
  mtext( side = 1, text = "Year", outer = TRUE,
          cex = 2, line = 2)
}

# Plot a 2 panel plot of the current HCR used in
# the MP
plotHCR <- function( Ftarg = 0.16, cap = 2.5 )
{
  # Calculate a max B for the x axis
  maxB <- 1.5 * cap / Ftarg
  # Create a vector of x vals
  Bseq <- seq(from = 0, to = maxB, length.out = 100 )

  # Create a catch sequence
  C           <- Ftarg * Bseq
  C[C >= cap] <- cap 

  # Now F sequence
  F           <- rep(Ftarg,100)
  F[C >= cap] <- (cap / Bseq)[C >= cap]

  par(  mfrow = c(2,1), 
        mar = c(2,1,1,1),
        oma = c(3,3,1,1) )
  plot( x = Bseq, y = F, type = "l",
        ylim = c(0, 1.5 * Ftarg),
        xlab = "", ylab = "", las = 1 )
    grid()
    mtext( side = 2, text = "Exploitation Rate (/yr)",
            line = 3)
    abline( h = cap / Ftarg, lty = 2, lwd = 2 )

  plot( x = Bseq, y = C, type = "l",
        xlab = "", ylab = "", las = 1,
        ylim = c(0, 1.5 * max(C)) )
    grid()
    mtext( side = 2, text = "Catch (kt)", line = 3)
    abline( h = cap / Ftarg, lty = 2, lwd = 2 )

  mtext( side = 1, outer = TRUE, text = "Spawning Biomass (kt)")


}
