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
                          MPlist      = testMPs,
                          MPnum       = 2,
                          interval    = 2  )
{
  # First, pull SSB from the MSEobj
  SSB <- MSEobj@SSB[MPnum + 1,simNum,,]/1e6

  # Now we have a matrix of biomasses with
  # stock in the rows (E/W) and time in the columns
  # Pick out the correct MP
  MPid <- MPlist[[MPnum]][1]

  # Now, get the right checkTable
  checkTable <- tables[[simNum]] %>%
                filter( mpName == MPid )


  # Count AMs
  AMs   <- unique(checkTable$AM)
  nAMs  <- length(AMs)

  # Count years
  yrs   <- 1965:2070
  tMP   <- 2020

  MPyrs <- seq( from  = tMP-1, to = max(yrs),
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
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    lines( x = yrs, y = SSB[1,], col = "red", lwd = 3 )
    points( x = checkTable$yr + 1, y = checkTable$Bnext_E,
            pch = 16, col = checkTable$col )
    mtext( side = 3, font = 2, text = "East Stock")

  plot( x = range(yrs), y = c(0,max(SSB[2,],checkTable$Bnext_W,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1 )
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
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
  mtext( side = 3, outer = TRUE, text = MPid, 
          font = 2, cex =  2 )
}

# Plot a 2 panel plot of the current HCR used in
# the MP
plotHCR <- function(  Ftarg = 0.08, 
                      cap   = 4,
                      LCP   = .4,
                      UCP   = 1.0,
                      Bmsy  = 57 )
{
  # Calculate a max B for the x axis
  
  # Create a vector of x vals
  Dseq <- seq(from = 0, to = 1.5, length.out = 100 )

  Fseq <- rep(0.1, 100)

  # Now split into control points and modify F
  Fseq[Dseq >= LCP & Dseq <= UCP ] <- 0.1 + 0.9/(UCP - LCP) * (Dseq[Dseq >= LCP & Dseq <= UCP] - LCP)
  Fseq[Dseq >= UCP ] <- 1.0

  Fseq <- Fseq * Ftarg
  Bseq <- Dseq * Bmsy

  # Create a catch sequence
  C                   <- Fseq * Bseq

  # Now modify the F sequence with the cap
  Fcap <- Fseq
  Fcap[C >= cap] <- (cap / Bseq)[C >= cap]
  Ccap <- Fcap * Bseq

  par(  mfrow = c(2,1), 
        mar = c(2,1,1,1),
        oma = c(3,3,1,1) )
  plot( x = Bseq, y = Fseq, type = "l",
        ylim = c(0, 1.5 * Ftarg), lwd = 2,
        xlab = "", ylab = "", las = 1, col = "grey50" )
    grid()
    lines( x = Bseq, y = Fcap, lwd = 2 )
    mtext( side = 2, text = "Exploitation Rate (/yr)",
            line = 3)
    abline( v = LCP * Bmsy, lty = 2, lwd = 2, col = "red")
    abline( v = UCP * Bmsy, lty = 2, lwd = 2, col = "orange")

  plot( x = Bseq, y = C, type = "l",
        xlab = "", ylab = "", las = 1, lwd = 2,
        ylim = c(0, 1.5 * max(C)), col = "grey50" )
    grid()
    lines( x = Bseq, y = Ccap, lwd = 2 )
    mtext( side = 2, text = "Catch (kt)", line = 3)
    abline( v = LCP * Bmsy, lty = 2, lwd = 2, col = "red")
    abline( v = UCP * Bmsy, lty = 2, lwd = 2, col = "orange")
    legend( x = "topright", bty = "n",
            legend = expression( 0.4*B[MSY], B[MSY], "Uncapped", "Capped" ),
            col = c("red","orange","grey50","black"),
            lty = c(2,2,1,1), lwd = 2,
             )

  mtext( side = 1, outer = TRUE, text = "Spawning Biomass (kt)")


}

plot_TACperformance <- function(  simIdx      = 1,
                                  MSEobj      = MSEtest_ROM_1d,
                                  eastTables  = checkTables[[1]],
                                  westTables  = checkTables[[2]],
                                  MPlist      = testMPs,
                                  MPidx       = 2,
                                  interval    = 2  )
{ 
  # First, pull SSB from the MSEobj
  SSB <- MSEobj@SSB[MPidx + 1,simIdx,,]/1e6

  # Now get catch quantities
  CWa <- MSEobj@CWa[MPidx + 1,simIdx,,]/1e6
  TAC <- MSEobj@TAC[simIdx,MPidx+1,,]/1e6
  # And reference points
  Bmsy_s <- MSEobj@BMSY[simIdx,]
  Fmsy_s <- MSEobj@UMSY[simIdx,]

  # Now we have a matrix of biomasses with
  # stock in the rows (E/W) and time in the columns
  # Pick out the correct MP
  eastMPid <- MPlist[[MPidx]][1]
  westMPid <- MPlist[[MPidx]][2]

  # Get the index for the check tables with the
  # correct simIdx - OR: Rbind the tables and filter

  # Now, get the right checkTables
  westCheckTable <- do.call("rbind", westTables ) %>%
                      filter( simNum == simIdx,
                              mpName == MPlist[[MPidx]][2])

  eastCheckTable <- do.call("rbind", eastTables ) %>%
                      filter( simNum == simIdx,
                              mpName == MPlist[[MPidx]][2])

  if( any(westCheckTable$mpName != westMPid) | any(eastCheckTable$mpName != eastMPid) |
      any(westCheckTable$simNum != simIdx) | any(eastCheckTable$simNum != simIdx) |
      nrow(eastCheckTable) == 0 | nrow(westCheckTable) == 0 )
  {

    message("Error in table, either empty or MP/sim index don't match.\n")
    browser()
  }

  # Count AMs
  AMs   <- unique(westCheckTable$AM)
  nAMs  <- length(AMs)

  # Count years
  yrs   <- 1965:2070
  tMP   <- 2020

  MPyrs <- seq( from  = tMP-1, to = max(yrs),
                by = interval )

  yrCol <- rep( MPyrs, rep(5,length(MPyrs)) )

  eastCheckTable$yr   <- rep( MPyrs, rep(5,length(MPyrs)) )
  westCheckTable$yr   <- rep( MPyrs, rep(5,length(MPyrs)) )

  AMcols <- RColorBrewer::brewer.pal(n = length(AMs), "Dark2")
  westCheckTable$col  <- AMcols
  eastCheckTable$col  <- AMcols

  AMlabels <- paste( "AM", as.character(AMs), sep = "_" )


  westCatchCheckTable <-  westCheckTable %>%
                          group_by(yr) %>%
                          summarise(  meanTAC_W = mean(TAC_W),
                                      aicTAC_W  = sum( wtdTAC ),
                                      q.05TAC_W = quantile( TAC, probs = 0.05),
                                      q.95TAC_W = quantile( TAC, probs = 0.95) )

  eastCatchCheckTable <-  eastCheckTable %>%
                          group_by(yr) %>%
                          summarise(  meanTAC_E = mean(TAC_E),
                                      aicTAC_E  = sum( wtdTAC ),
                                      q.05TAC_E = quantile( TAC, probs = 0.05),
                                      q.95TAC_E = quantile( TAC, probs = 0.95))                          

  layoutMat <- matrix( c( 1,1,1,1,
                          1,1,1,1,
                          2,2,2,2,
                          3,3,3,3,
                          3,3,3,3,
                          4,4,4,4), nrow = 6, ncol = 4,
                                    byrow = TRUE ) 

  layout( layoutMat )

  # Set up the E/W plot
  par( mar =c(0,1,2,1), oma = c(4,4.5,2,1) )

  plot( x = range(yrs), y = c(0,max(SSB[1,],eastCheckTable$Bnext_E,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1, axes = F )
    axis( side = 2, las = 1)
    box()
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    lines( x = yrs, y = SSB[1,], col = "red", lwd = 3 )
    points( x = eastCheckTable$yr + 1, y = eastCheckTable$Bnext_E,
            pch = 16, col = eastCheckTable$col )
    mtext( side = 3, font = 2, text = "East Stock")
    mtext(  side = 4, text = eastMPid, 
            font = 2, cex =  1, line = 2 )

  par( mar =c(2,1,0,1) )

  plot( x = range(yrs), y = c(0,max(CWa[1,], TAC[1,], na.rm = T) ),
        xlab = "", ylab = "", type = "n", las = 1, axes = F )
    axis( side = 1 )
    axis( side = 2, las = 1)
    box()
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    # lines( x = yrs, y = TAC[1,], lty = 1 )
    lines( x = yrs, y = CWa[1,], lty = 1, lwd = 2,
            col = "grey40" )
    segments( x0 = eastCatchCheckTable$yr + 1, x1 = eastCatchCheckTable$yr + 1,
              y0 = eastCatchCheckTable$q.05TAC_E, y1 = eastCatchCheckTable$q.95TAC_E, lwd = 2,
              col = "grey60" )
    points( x = eastCatchCheckTable$yr + 1, y = eastCatchCheckTable$meanTAC_E,
            pch = 16, cex = .8 )
    points( x = eastCatchCheckTable$yr + 1, y = eastCatchCheckTable$aicTAC_E,
            pch = 17, cex = .8 )

  par( mar =c(0,1,2,1) )

  plot( x = range(yrs), y = c(0,max(SSB[2,],westCheckTable$Bnext_W,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1, axes = F )
    axis( side = 2, las = 1)
    box()
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    lines( x = yrs, y = SSB[2,], col = "red", lwd = 3 )
    points( x = westCheckTable$yr + 1, y = westCheckTable$Bnext_W,
            pch = 16, col = westCheckTable$col )
    mtext( side = 3, font = 2, text = "West Stock")
    mtext(  side = 4, text = westMPid, 
            font = 2, cex =  1, line = 2 )
    legend( x = "topleft", legend = AMlabels,
            pch = 16, col = AMcols, bty = "n" )

  par( mar =c(2,1,0,1) )
  plot( x = range(yrs), y = c(0,max(CWa[2,], TAC[2,], na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1, axes = F )
    axis( side = 1 )
    axis( side = 2, las = 1)
    box()
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    # lines( x = yrs, y = TAC[2,], lty = 1 )
    lines( x = yrs, y = CWa[2,], lty = 1, lwd = 2, 
            col = "grey40" )
    segments( x0 = westCatchCheckTable$yr + 1, x1 = westCatchCheckTable$yr + 1,
              y0 = westCatchCheckTable$q.05TAC_W, y1 = westCatchCheckTable$q.95TAC_W, lwd = 2,
              col = "grey60" )
    points( x = westCatchCheckTable$yr + 1, y = westCatchCheckTable$meanTAC_W,
            pch = 16, cex = .8 )
    points( x = westCatchCheckTable$yr + 1, y = westCatchCheckTable$aicTAC_W,
            pch = 17, cex = .8 )

  mtext(  side = 2, text = "Spawning Biomass and Catch (kt)", 
          outer = T, line = 2.5, cex = 1.5 )
  mtext( side = 1, text = "Year", outer = TRUE,
          cex = 2, line = 2)

}

loadMSE <- function(  #OMvec = paste("OM_",1:15,"d", sep = ""),
                      OMid = "OM_1d",
                      prefix = "test",
                      folder = "./MSEs" )
{ 

  # Search the folder for MSE outputs
  mseObjFileList  <- list.files(folder)
  mseObjID        <- paste("MSE", prefix, "_", OMid, sep = "" )
  mseFileName     <- paste(mseObjID, ".Rdata", sep = "")

  load( file.path(folder,mseFileName) )
  
  return( get( mseObjID ) )
}

getBr30 <- function( MSEobj )
{
  B_BMSY <- MSEobj@B_BMSY[,,,82]

  Bquants <- apply( X = B_BMSY, FUN = quantile,
                    MARGIN = c(1,3), probs = c(0.05, 0.5, 0.95) )

  dimnames( Bquants )[[2]] <- names(MSEobj@MPs)
  dimnames( Bquants )[[2]][1] <- "ZeroC"

  return(Bquants)

}

getCquants <- function( MSEobj, tIdx = 53:62 )
{
  CWa <- MSEobj@CWa[,,,tIdx]/1e6

  Cmean <- apply( X = CWa, FUN = mean,
                      MARGIN = c(1,2,3) )

  Cquants <- apply( X = Cmean, FUN = quantile,
                      MARGIN = c(1,3), probs = c(0.05, 0.5, 0.95) )

  dimnames( Cquants )[[2]] <- names(MSEobj@MPs)
  dimnames( Cquants )[[2]][1] <- "ZeroC"

  return(Cquants)
}

getMPnames <- function( MSEobj )
{
  MPnames <- names(MSEobj@MPs)
  MPnames[1] <- "ZeroC"

  MPnames
}

# plotAllMSEs
plotAllMSEs <- function(  OMvec = paste("OM_",1:15,"d",sep = ""), 
                          prefix = NULL )
{
  MSElist <- lapply( X = OMvec, FUN = loadMSE, prefix = prefix )
  names(MSElist) <- OMvec

  nMSE <- length(MSElist)
  graphics.off()

  outFolder <- file.path("./MSEs","plots")
  if( !dir.exists(outFolder))
    dir.create(outFolder)

  for( i in 1:nMSE )
  {
    fileName <- paste(OMvec[i],"_MSEplot.png",sep = "")
    outFile <- file.path(outFolder, fileName)



    png( filename = outFile, width = 11, 
          height = 8.5, 
          units = "in",
          res = 300 )
    plot( MSElist[[i]] )
    dev.off()
  }

  
}

# plotMSEperf()
# Takes a vector of OM ids and plots the distributions
# MSE performance metrics Br30 (B_2046/Bmsy) and
# AvC30 (Average catch in 2046) for all MPs and OMs
plotMSEperf <- function(  OMvec = paste("OM_",1:15,"d",sep = ""), 
                          prefix = NULL,
                          ptcex = 1 )
{
  MSElist <- lapply( X = OMvec, FUN = loadMSE, prefix = prefix )
  names(MSElist) <- OMvec
  # First do Br30 plots

  nMSE <- length(OMvec)

  B30quants <- sapply( X = MSElist, FUN = getBr30, simplify = "array" )
  C10quants <- sapply( X = MSElist, FUN = getCquants, simplify = "array", tIdx = 53:62 )
  C20quants <- sapply( X = MSElist, FUN = getCquants, simplify = "array", tIdx = 63:72 )
  C30quants <- sapply( X = MSElist, FUN = getCquants, simplify = "array", tIdx = 73:82 )
  MPnames <- lapply( X = MSElist, FUN = getMPnames )

  MPnameUnion <- MPnames[[1]]
  if( nMSE > 1 )
  {
    for( j in 2:nMSE )
      MPnameUnion <- union(MPnameUnion,MPnames[[j]])
  }

  MPnames <- MPnameUnion
  nMPs <- dim(B30quants)[2]
  
  MPcols  <-  brewer.pal( nMPs,  "Dark2")
  MPpch   <-  rep(16, nMPs)

  par( mfrow = c(2,2), mar = c(.25,1.5,.25,1.5), oma = c(5,3,2,2) )

  # Plot B/Bmsy in year 30
  plot( x = c(1, length(MSElist)), y = c(0,3.5),
        axes = F, type = "n", xlab = "", ylab = "" )
    mfg <- par("mfg")
    # axis( side = 1, at = 1:nMSE, labels = OMvec, las = 2 )
    axis( side = 2, las = 1 )
    box()
    abline( h = c(0.5, 1, 1.5,2,2.5,3), lty = c(2,1,2,2,2,2)+1, 
            lwd = c(0.8,1.5,0.8,.8,.8,.8),
            col = "grey60" )
    abline( v = seq(from = 1.5, to = nMSE - .5, by = 1),
            lty = 3, lwd = .3, col = "grey30" )
    mtext( side = 2, text = expression(B[2046]/B[MSY]), line = 2.5)
    # Loop over MSEs and MPs, plot performance
    for( j in 1:nMSE )
    {
      
      splits <- seq( from = -.3, to = .3, length.out = nMPs )
      for( k in 1:nMPs )
      {
        # Central 90%
        segments( x0 = splits[k] + j, 
                  y0 = B30quants[1,k,1,j],
                  y1 = B30quants[3,k,1,j], col = "grey40",
                  lwd = .8 )
        # Median
        points( x = splits[k] + j, y = B30quants[2,k,1,j],
                pch = MPpch[k], col = MPcols[k], cex = ptcex )

      }
    }
    legend( x = "bottomleft", legend = MPnames,
            pch = 16, col = MPcols,
            lwd = .8, cex = .8, bg = "white" )
    mtext( side = 3, text = "East Stock" )

  
  plot( x = c(1, length(MSElist)), y = c(0,3.5),
        axes = F, type = "n", xlab = "", ylab = "" )
    # axis( side = 1, at = 1:nMSE, labels = OMvec, las = 2 )
    axis( side = 2, las = 1 )
    box()
    abline( h = c(0.5, 1, 1.5,2,2.5,3), 
            lty = c(2,1,2,2,2,2)+1, 
            lwd = c(0.8,1.5,0.8,.8,.8,.8),
            col = "grey60" )
    abline( v = seq(from = 1.5, to = nMSE - .5, by = 1),
            lty = 3, lwd = .3, col = "grey30" )
    # Loop over MSEs and MPs, plot performance
    for( j in 1:nMSE )
    {
      splits <- seq( from = -.3, to = .3, length.out = nMPs )
      for( k in 1:nMPs )
      {
        # Central 90%
        segments( x0 = splits[k] + j, 
                  y0 = B30quants[1,k,2,j],
                  y1 = B30quants[3,k,2,j], col = "grey40",
                  lwd = .8 )
        # Median
        points( x = splits[k] + j, y = B30quants[2,k,2,j],
                pch = MPpch[k], col = MPcols[k], cex = ptcex )

      }
    }
    mtext( side = 3, text = "West Stock" )


  ##### Plot ave catch over third 10 years ####
  plot( x = c(1, length(MSElist)), y = range(C30quants[,,1,]),
        axes = F, type = "n", xlab = "", ylab = "" )
    mfg <- par("mfg")
    axis( side = 1, at = 1:nMSE, labels = OMvec, las = 2 )
    axis( side = 2, las = 1 )
    box()
    # abline( h = c(0.5, 1, 1.5,2,2.5,3), lty = c(2,1,2,2,2,2)+1, 
    #         lwd = c(0.8,1.5,0.8,.8,.8,.8),
    #         col = "grey40" )
    abline( v = seq(from = 1.5, to = nMSE - .5, by = 1),
            lty = 3, lwd = .3, col = "grey30" )
    mtext( side = 2, text = expression(paste(C[30], " (kt)",sep = "")), line = 2.5)
    # Loop over MSEs and MPs, plot performance
    for( j in 1:nMSE )
    {
      
      splits <- seq( from = -.3, to = .3, length.out = nMPs )
      for( k in 1:nMPs )
      {
        # Central 90%
        segments( x0 = splits[k] + j, 
                  y0 = C30quants[1,k,1,j],
                  y1 = C30quants[3,k,1,j], col = "grey40",
                  lwd = .8 )
        # Median
        points( x = splits[k] + j, y = C30quants[2,k,1,j],
                pch = MPpch[k], col = MPcols[k], cex = ptcex )

      }
    }


  plot( x = c(1, length(MSElist)), y = range(C30quants[,,2,]),
        axes = F, type = "n", xlab = "", ylab = "" )
    axis( side = 1, at = 1:nMSE, labels = OMvec, las = 2 )
    axis( side = 2, las = 1 )
    box()
    # abline( h = c(0.5, 1, 1.5,2,2.5,3), 
    #         lty = c(2,1,2,2,2,2)+1, 
    #         lwd = c(0.8,1.5,0.8,.8,.8,.8),
    #         col = "grey40" )
    abline( v = seq(from = 1.5, to = nMSE - .5, by = 1),
            lty = 3, lwd = .3, col = "grey30" )
    # Loop over MSEs and MPs, plot performance
    for( j in 1:nMSE )
    {
      splits <- seq( from = -.3, to = .3, length.out = nMPs )
      for( k in 1:nMPs )
      {
        # Central 90%
        segments( x0 = splits[k] + j, 
                  y0 = C30quants[1,k,2,j],
                  y1 = C30quants[3,k,2,j], col = "grey40",
                  lwd = .8 )
        # Median
        points( x = splits[k] + j, y = C30quants[2,k,2,j],
                pch = MPpch[k], col = MPcols[k], cex = ptcex )

      }
    }


}


