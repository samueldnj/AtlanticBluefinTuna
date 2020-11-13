# --------------------------------------------------------------------------
# plots.R
# 
# plots for testing the DD MP
#
# Authors: Samuel D N Johnson, Steven Rossi
# Date: July 16, 2019
#
# To plot MSE performance, first run plotViolin(saveStats=TRUE) to
# generate mseStats.Rdata.
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

  #yrCol <- rep( MPyrs, rep(5,length(MPyrs)) )

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

plotHCRB0 <- function(  Ftarg = 0.08, 
                        OM    = 1,
                        nI    = 100 )
{
  z <- read.csv("../OMs/empPars.csv")
  ins <- read.csv("../OMs/AMinits.csv")
  B0_s <- as.numeric(z[OM,c("B0_GOM","B0_MED")])
  Bmsy_s <- as.numeric(z[OM,c("Bmsy_GOM","Bmsy_MED")])
  M_s <- as.numeric(ins[OM,c("M_W","M_E")])

  UCP_s <- 0.2*B0_s
  LCP_s <- 0.4*UCP_s
  
  # Create a vector of x vals
  BseqW <- seq(from = 0, to = 1.2*UCP_s[1], length.out = nI )
  BseqE <- seq(from = 0, to = 1.2*UCP_s[2], length.out = nI )

  # change fmsy_s to ftarget
  FtargW <- rampHCR( B_s = BseqW,
                     Fmsy_s = rep(M_s[1],nI),
                     Bmsy_s = rep(UCP_s[1],nI) )
  FtargE <- rampHCR( B_s = BseqE,
                     Fmsy_s = rep(M_s[2],nI),
                     Bmsy_s = rep(UCP_s[2],nI) )

  par(  mfrow = c(1,2), 
        mar = c(2,2,1,1),
        oma = c(3,3,1,1) )
  plot( x = BseqW, y = FtargW, type = "l", main="West", ylim=c(0,max(FtargW)),
        lwd = 2, xlab = "", ylab = "", las = 1, col = "grey50" )
  grid()
  mtext( side = 2, text = "Exploitation Rate (/yr)",
          line = 3)
  abline( v = LCP_s[1], lty = 2, lwd = 1.5, col = "red")
  abline( v = UCP_s[1], lty = 2, lwd = 1.5, col = "orange")
  lines( x = BseqW, y = FtargW, lwd = 2, col = "grey50" )

  plot( x = BseqE, y = FtargE, type = "l", main="East", ylim=c(0,max(FtargW)),
        lwd = 2, xlab = "", ylab = "", las = 1, col = "grey50" )
  grid()
  abline( v = LCP_s[2], lty = 2, lwd = 1.5, col = "red")
  abline( v = UCP_s[2], lty = 2, lwd = 1.5, col = "orange")
  lines( x = BseqE, y = FtargE, lwd = 2, col = "grey50" )


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
                              mpName == MPlist[[MPidx]][1])

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

  # Get AM names
  load("../OMs/clust.Rdata")
  AMlabs <- clust[[as.character(nAMs)]]$OMs

  # Count years
  yrs   <- 1965:2070
  tMP   <- 2020

  MPyrs <- seq( from  = tMP-1, to = max(yrs),
                by = interval )

  #yrCol <- rep( MPyrs, rep(5,length(MPyrs)) )

  eastCheckTable$yr   <- rep( MPyrs[-length(MPyrs)], rep(nAMs,length(MPyrs)-1) )
  westCheckTable$yr   <- rep( MPyrs[-length(MPyrs)], rep(nAMs,length(MPyrs)-1) )

  AMcols <- RColorBrewer::brewer.pal(n = nAMs, "Dark2")
  westCheckTable$col  <- AMcols
  eastCheckTable$col  <- AMcols

  westCheckTable$col2 <- NA
  eastCheckTable$col2 <- NA
  wtmp <- round(westCheckTable$amWts*100)
  etmp <- round(eastCheckTable$amWts*100)
  westCheckTable$col2[wtmp>10] <- paste("grey",100-wtmp[wtmp>10],sep="")
  eastCheckTable$col2[etmp>10] <- paste("grey",100-etmp[etmp>10],sep="")

  AMlabels <- paste( "AM", AMlabs, sep = "_" )


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
    points( x = eastCheckTable$yr + 1, y = eastCheckTable$Bnext_E,
            col = eastCheckTable$col2, cex=1.1 )
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

plotHCRs <- function( Be, Fes, Fea, ptse, Bw, Fws, Fwa, ptsw )
{
  k <- nrow(Be)

  tcols <- brewer.pal(k,"Set1")

  # EAST

  plot( x=range(Be), y=c(0,1.1*max(Fes,Fea)), type="n", las=1, main="East" )
  plotbg()
  # Fishing mortality
  for( i in 1:k )
  {
    lines( x=Be[i, ], y=Fes[i, ], lwd=1.5, col=tcols[i] )
    lines( x=Be[i, ], y=Fea[i, ], lwd=1.5, lty=2, col=tcols[i] )
    points( x=ptse[[i]]["Bs"], y=ptse[[i]]["Fs"], pch=16, col=tcols[i] )
    points( x=ptse[[i]]["Ba"], y=ptse[[i]]["Fa"], pch=16, col=tcols[i] )
    abline( v=min(ptse[[i]]["Ba"],ptse[[i]]["Bs"]), col=tcols[i] )
  }

  # Catch
  plot( x=range(Be), y=c(0,1.1*max(Be*Fes,Be*Fea)), type="n", las=1 )
  plotbg()
  for( i in 1:k )
  {
    lines( x=Be[i, ], y=(Be*(1-exp(-Fes)))[i, ], lwd=1.5, col=tcols[i] )
    lines( x=Be[i, ], y=(Be*(1-exp(-Fea)))[i, ], lwd=1.5, lty=2, col=tcols[i] )
    Cs <- ptse[[i]]["Bs"]*(1-exp(-ptse[[i]]["Fs"]))
    Ca <- ptse[[i]]["Ba"]*(1-exp(-ptse[[i]]["Fa"]))
    points( x=ptse[[i]]["Bs"], y=Cs, pch=16, col=tcols[i] )
    points( x=ptse[[i]]["Ba"], y=Ca, pch=16, col=tcols[i] )
    abline( v=min(ptse[[i]]["Ba"],ptse[[i]]["Bs"]), col=tcols[i] )
  }

  # WEST

  plot( x=range(Bw), y=c(0,1.1*max(Fws,Fwa)), type="n", las=1, main="West" )
  plotbg()
  # Fishing mortality
  for( i in 1:k )
  {
    lines( x=Bw[i, ], y=Fws[i, ], lwd=1.5, col=tcols[i] )
    lines( x=Bw[i, ], y=Fwa[i, ], lwd=1.5, lty=2, col=tcols[i] )
    points( x=ptsw[[i]]["Bs"], y=ptsw[[i]]["Fs"], pch=16, col=tcols[i] )
    points( x=ptsw[[i]]["Ba"], y=ptsw[[i]]["Fa"], pch=16, col=tcols[i] )
    abline( v=min(ptsw[[i]]["Ba"],ptsw[[i]]["Bs"]), col=tcols[i] )
  }
  legend( x="bottomright", lty=1:2, legend=c("By stock","By area"), bty="n",
          lwd=1.5, cex=0.8 )

  # Catch
  plot( x=range(Bw), y=c(0,1.1*max(Bw*Fws,Bw*Fwa)), type="n", las=1 )
  plotbg()
  for( i in 1:k )
  {
    lines( x=Bw[i, ], y=(Bw*(1-exp(-Fws)))[i, ], lwd=1.5, col=tcols[i] )
    lines( x=Bw[i, ], y=(Bw*(1-exp(-Fwa)))[i, ], lwd=1.5, lty=2, col=tcols[i] )
    Cs <- ptsw[[i]]["Bs"]*(1-exp(-ptsw[[i]]["Fs"]))
    Ca <- ptsw[[i]]["Ba"]*(1-exp(-ptsw[[i]]["Fa"]))
    points( x=ptsw[[i]]["Bs"], y=Cs, pch=16, col=tcols[i] )
    points( x=ptsw[[i]]["Ba"], y=Ca, pch=16, col=tcols[i] )
    abline( v=min(ptsw[[i]]["Ba"],ptsw[[i]]["Bs"]), col=tcols[i] )
  }

  mtext( side=1, text="Biomass (kt)", outer=TRUE )
  mtext( side=2, text="         TAC (kt)                      Fishing mortality (/yr)",
         outer=TRUE, line=1 )

}

plotViolin <- function( OMvec = paste0("MSE_",1:96), 
                        prefix = NULL,
                        ptcex = 1,
                        modelBased=TRUE,
                        mseStatsFile="mseStats.Rdata",
                        folder = "./MSEs/oct22/rdas" )
{
  library(vioplot)

#  noCap <- lapply( X = OMvec, 
#                   FUN = loadMSE, 
#                   prefix = prefix,
#                   folder = "./MSEs/noCap" )
#  names(noCap) <- OMvec
#
#  noCapFM <- lapply( X = OMvec, 
#                       FUN = loadMSE, 
#                       prefix = prefix,
#                       folder = "./MSEs/noCapFM" )
#  names(noCapFM) <- OMvec
#
#  mixCap <- lapply( X = OMvec, 
#                       FUN = loadMSE, 
#                       prefix = prefix,
#                       folder = "./MSEs/mixCap" )
#  names(mixCap) <- OMvec
#
#  emp <- lapply( X = OMvec, 
#                       FUN = loadMSE, 
#                       prefix = prefix,
#                       folder = "../MME_MP/MSEs/noCapAug30" )
#  names(emp) <- OMvec  

  if( modelBased )
  {
    X <- paste0( folder, "/", OMvec, ".rda" )
    test0 <- lapply( X = X, 
                     FUN = readRDS )
    names(test0) <- OMvec  
    
    mixCap <- lapply( X = paste0(1:96,"d"), 
                         FUN = loadMSE, 
                         prefix = "_OM",
                         folder = "./MSEs/newClustAug28-2" )
    names(mixCap) <- OMvec

    J <- test0[[1]]@nMPs
    J[2] <- mixCap[[1]]@nMPs-1

    allMSE <- list( test0=test0, mixCap=mixCap )

    j <- c("No Catch",
           "a=0.1",
           "a=0.01",
           "last10",
           "a=0")

  }
  else
  {
    test <- lapply( X = OMvec, 
                     FUN = loadMSE, 
                     prefix = prefix,
                     folder = "../MME_MP/MSEs/sep5" )
    names(test) <- OMvec
    
    test2 <- lapply( X = OMvec, 
                     FUN = loadMSE, 
                     prefix = prefix,
                     folder = "../MME_MP/MSEs/sep15" )
    names(test2) <- OMvec

    test3 <- lapply( X = OMvec, 
                     FUN = loadMSE, 
                     prefix = prefix,
                     folder = "../MME_MP/MSEs/sep16" )
    names(test3) <- OMvec

    J <- rep(1,2)
    J[1] <- test[[1]]@nMPs
    J[2] <- test2[[1]]@nMPs-1
    J[3] <- test3[[1]]@nMPs-1

    allMSE <- list( test=test,
                    test2=test2,
                    test3=test3 )

    j <- c("No Catch",
           "emp_msyCap","emp_msyCapB0","emp_msyCapFM","emp_msyCapFMB0",
           "emp_msyCapFMAlt","emp_msyCapFMAlt" )
  }

  I <- length(test0)
  #j <- character(sum(J))
  k <- 1
  nudge <- 0
  for( iMSE in 1:length(J) )
  {
    if( iMSE > 1 )
      nudge <- 1
    for( iMP in (1:J[iMSE]+nudge) )
    {
      #j[k] <- allMSE[[iMSE]][[iMSE]]@MPs[[iMP]][1]
      k <- k+1
    }
  }
  j2 <- character(length(j))

  MPcols  <-  brewer.pal( 3,  "Set1")

  cols <- rep("grey",length(k))
  #cols[2:4] <- MPcols[1]
  #cols[5:8] <- MPcols[2]
  #cols[7:8] <- MPcols[3]
  
#  x <- c("AvC30","C10","C20","C30","D10","D20","D30","LD","DNC",
#         "LDNC","POF","POS","PGK","AAVC","Br30")
  x <- c("C10","C20","C30","D30","LD","Br30")
  X <- length(x)

  east <- array( data=NA, dim=c(I,sum(J),X), dimnames=list(OMvec,j,x) )
  west <- array( data=NA, dim=c(I,sum(J),X), dimnames=list(OMvec,j,x) )

  j0 <- 1
  for( iMSE in 1:length(J) )
  {
    j1 <- j0 + J[iMSE] - 1
    keepRow <- 1:J[iMSE]

    if( iMSE>1 )
    {
      keepRow <- keepRow + 1
    }

    for( i in 1:I )
    {
      perf <- getperf(allMSE[[iMSE]][[i]])
      east[i,j0:j1, ] <- as.matrix(perf[[1]][keepRow,x])
      west[i,j0:j1, ] <- as.matrix(perf[[2]][keepRow,x])
    }
    j0 <- j1 + 1
  }

  if( !is.null(mseStatsFile) )
  {
    mseStats <- list(east=east,west=west)
    save( mseStats, file=mseStatsFile )
  }

  par( mfrow=c(X+1,2), mar=c(0.4,4,0,1), oma=c(0,0,2,0) )
  for( k in 1:X )
  {
    if( k<=3 )
      ymax <- c( max(west[ , ,k]), max(east[ , ,k]) )
    else if( k==4 )
      ymax <- c(1,1)
    else if( k==5 )
      ymax <- c(0.75,0.75)
    else if( k==6 )
      ymax <- c(4.1,4.1)

    par( mar=c(0.4,4,0,0.5) )
    vioplot( west[ ,1,k], west[ ,2,k], west[ ,3,k], west[ ,4,k],
             west[ ,5,k], #west[ ,6,k], #west[ ,7,k], west[ ,8,k], #west[ ,9,k],
             ylim=c(0,ymax[1]),
             ylab=paste("",x[k]), las=1, names=j2 )
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col="white", border=NA )
    grid()
    vioplot( west[ ,1,k], west[ ,2,k], west[ ,3,k], west[ ,4,k],
             west[ ,5,k], #west[ ,6,k], #west[ ,7,k], west[ ,8,k], #west[ ,9,k],
             ylab=paste("",x[k]), las=1, add=TRUE, col=cols )

    if( k==X )
      abline( h=1, lty=2 )

    par( mar=c(0.4,2,0,0.5) )
    vioplot( east[ ,1,k], east[ ,2,k], east[ ,3,k], east[ ,4,k],
             east[ ,5,k], #east[ ,6,k], #east[ ,7,k], east[ ,8,k], #east[ ,9,k],
             ylim=c(0,ymax[2]),
             ylab=paste("",x[k]), las=1, names=j2 )
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col="white", border=NA )
    grid()
    vioplot( east[ ,1,k], east[ ,2,k], east[ ,3,k], east[ ,4,k],
             east[ ,5,k], #east[ ,6,k], #east[ ,7,k], east[ ,8,k], #east[ ,9,k],
             ylab=paste("",x[k]), las=1, add=TRUE, col=cols )

    if( k==X )
      abline( h=1, lty=2 )
  }

  parr <- par("usr")
  par( mar=c(0,4,0,0.5) )
  for( h in 1:2 )
  {
    if( h==2 )
      par( mar=c(0,2,0,0.5) )
    plot( x=c(parr[1],parr[2]), y=c(0,1), type="n", yaxs="i",
          axes=FALSE, xlab="", ylab="" )
    text( seq(0.9,sum(J)+0.3,length.out=sum(J)), par("usr")[4]-0.04, labels=j,
          srt=45, adj=c(1.1,1.1), xpd=TRUE, cex=.9 )
  }
# triangles at max, horiz line at 1, symbols for axes,
# vertical line at last ten yrs avg C
  par( font=2 )
  mtext( side=3, text="         West                                                    East", outer=TRUE )
  par( font=1 )

}

plotROMs <- function( par="Br30", CMP=2 )
{
  nI <- 12
  cols <- brewer.pal(3,"Set1")[1:2]
  east <- numeric(nI)
  west <- numeric(nI)
  for( i in 1:nI )
  {
    z <- readRDS( paste("MSEs/LFR-DelayDiff/MSE_R_",i,".rda",sep="") )
    east[i] <- getperf(z)$East[CMP,"Br30"]
    west[i] <- getperf(z)$West[CMP,"Br30"]
  }

  par( mfrow=c(1,1), mar=c(3,3,1,1), oma=c(2,2,0,0) )
  plot( x=c(1,nI), y=c(0,2), type="n", las=1 )
  grid()
  box()
  points( x=1:nI, east, col=cols[1], pch=16 )
  points( x=1:nI, west, col=cols[2], pch=16 )
  abline( h=1, lty=2 )

  legend( x="bottomleft", legend=c("East","West"), col=cols, pch=16, bty="n" )

  mtext( side=1, text="Robustness OM", outer=TRUE )
  mtext( side=2, text=par, outer=TRUE )

}

plotbg <- function(col=rgb(235,235,235,maxColorValue=255))
{
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=col)
  grid( col="white", lty=1 )
}

plotTradeoffAll <- function( xstat="C10", ystat="Br30", type="mod",
                             fac=0 )
{
  load("mseStats.Rdata")

  facDesc <- c( "Steep", "Spawn/M", "Mig", "SSB", "LCW" )[fac]
  if( fac )
    levs <- list( c("W1975Change","Const","ProjChange"),
                  c("Young/Hi","Old/Lo"),
                  c("Lo","Hi"),
                  c("W15/E200","W15/E400","W50/E200","W50/E400"),
                  c("1/20","1") )[[fac]]
  else
    levs <- ""

  caps <- list( c(20,25), c(2.5,4) )

  dims <- dim(mseStats[[1]])

  if( type=="mod" )
    j <- 2:7
  else if( type=="emp" )
    j <- 8:dims[2]
  else
    j <- 1:dims[2]

  jnames <- names(mseStats[[1]][1,j,1])
  jnames <- gsub(".*_","",jnames)
  J <- length(j)

  cols <- brewer.pal(J,"Set1")
  stocks <- c("East","West")

  C_h <- getC(type="avg10")
  minC_h <- getC(type="min10")
  maxC_h <- getC(type="max10")

  if( fac )
  {
    pchs <- as.numeric(Design[[3]][ ,fac])
    pchs[pchs==2] <- 0
  }
  else
    pchs <- 1

  col_ij <- matrix( rep(cols,each=dims[1]), nrow=dims[1], ncol=J )
  pch_ij <- matrix( pchs, nrow=dims[1], ncol=J )

  par( mfrow=c(2,1), mar=c(3,3,1,1), oma=c(2,2,0,0) )

  for( h in 2:1 )
  {
    x_ij <- mseStats[[h]][ ,j,xstat]
    y_ij <- mseStats[[h]][ ,j,ystat]

    xmax <- max(x_ij,caps[[h]])
    xmin <- min(x_ij)
    ymax <- 1.2*max(y_ij)

    plot( x_ij, y_ij, col=col_ij, las=1, xlim=c(xmin,xmax), ylim=c(0,ymax) )
    plotbg(col="grey90")
    abline( h=1, lty=3 )
    abline( v=C_h[h], lty=1 )
    abline( v=c(minC_h[h],maxC_h[h]), lty=2 )
    points( x_ij, y_ij, col=col_ij, cex=0.8, lwd=1.2, pch=pch_ij )

    # Caps
    points( x=caps[[h]], y=rep(par("usr")[3]+0.06,2), pch=2, lwd=2, col=cols[c(1,4)] )

    par( font=2 )
    legend( x="topleft", legend=stocks[h], bty="n" )
    par( font=1 )

    if( h==2 )
    {
      leg <- c(jnames,paste(facDesc,": ",levs,sep=""))
      lpch <- c(rep(15,J),1:length(levs))
      lpch[lpch==2] <- 0
      lcols <- c(cols,rep("black",length(levs)))
      legend( x="topright", legend=leg, bty="n", col=lcols, pch=lpch,
              pt.cex=0.8, lwd=1.2, lty=0, cex=0.6 )
    }

  }

  par( font=2 )
  mtext( side=1, text=xstat, outer=TRUE )
  mtext( side=2, text=ystat, outer=TRUE )
  par( font=1 )

}

plotTradeoff <- function( xstat="C10", ystat="D30" )
{
  load("mseStats.Rdata")

  dims <- dim(mseStats[[1]])

  cols <- brewer.pal(dims[2],"Paired")

  par( mfrow=c(4,3), mar=c(3,3,1,1) )

  x_ij <- mseStats[[1]][ , ,xstat]
  y_ij <- mseStats[[1]][ , ,ystat]

  for( j in 1:dims[2] )
    plot( x_ij[ ,j], y_ij[ ,j] )

}





# plotMSEperf()
# Takes a vector of OM ids and plots the distributions
# MSE performance metrics Br30 (B_2046/Bmsy) and
# AvC30 (Average catch in 2046) for all MPs and OMs
plotMSEperfOld <- function(  OMvec = paste("OM_",1:96,"d",sep = ""), 
                          prefix = NULL,
                          ptcex = 1, 
                          projFolder = "./MSEs/loCaps" )
{
  MSElist <- lapply(  X = OMvec, 
                      FUN = loadMSE, 
                      prefix = prefix,
                      folder = projFolder )
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
    legend( x = "topleft", legend = MPnames, bty = "n",
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


plotIndexFit <- function( i="",
                          repObj=report,
                          plotPDF=NULL )
{
  

  I_gt <- repObj$I_gt
  E_gt <- repObj$predI_gt

  nG <- nrow(I_gt)
  nT <- ncol(I_gt)
  yr <- 1:nT

  # Replace negative indices with NA
  I_gt[I_gt < 0] <- NA
  E_gt[E_gt < 0] <- NA

  I_gt <- apply( I_gt, 2, "*", repObj$q_g )
  E_gt <- apply( E_gt, 2, "*", repObj$q_g )

  pdfFile <- paste("AMfits/AM",i,"fit",nT,".pdf",sep="")

  gname <- repObj$grs
  pdfDim <- c(7,9)

  if( plotPDF )
    pdf( pdfFile, height=pdfDim[1], width=pdfDim[2] )

  x <- ifelse( nG>20, 5, 4 )

  par(mfrow=c(3,3),mar=c(0,5,0,0),oma=c(2,5,2,3))

  for( g in 1:nG )
  {
    I <- I_gt[g, ]
    E <- E_gt[g, ]
    plot( x=yr, y=I, axes=FALSE,
          ylim=c(0,1.1*max(I,E,na.rm=1)),
          las=1, xlab="", ylab="" )
    axis( side=2, las=1 )
    grid()
    box()
    lines( x=yr, y=E, lwd=2 )
    legend("topleft",
           legend=gname[g],
           #legend=paste(gname[g],"\nq=",round(repObj$q_g[g],4)),
           bty="n")
    if( g>6 )
      axis( side=1 )
  }

  mtext( side=2, text="Biomass (kt)", outer=TRUE, line=3 )

  if( plotPDF )
    dev.off()

}

plotUnorderedStat <- function( par="Br30", js=2:4, mseFile="mseStats.Rdata" )
{
  load(mseFile)
  eBr30 <- mseStats[["east"]][ ,js,par]
  wBr30 <- mseStats[["west"]][ ,js,par]
  nMP <- ncol(eBr30)

  jnames <- dimnames(eBr30)[[2]]
  jnames <- gsub(".*_","",jnames)

  cols <- brewer.pal(nMP,"Set1")
  
  par( mfrow=c(2,1), mar=c(2,2,2,1), oma=c(2,2,1,0) )

  plot( x=c(1,96), y=c(0,1.15*max(wBr30[ ,])), type="n", las=1, main="West" )
  plotbg()
  x <- seq(7,96,by=12)
  y <- seq(1,96,by=6)
  z <- seq(1,96,by=24)
  zz <- unique(as.character(Design[[3]][,4]))
  for( i in 1:length(x) )
    rect(x-0.5,par("usr")[3],x+5.5,par("usr")[4],col="grey80",border=NA)
  grid( col="white", lty=1 )
  abline( v=seq(10,90,by=10), col="white" )
  abline( v=seq(5,95,by=10), col="white", lty=2 )
  for( i in 1:length(y) )
    rect(y-0.5,par("usr")[4]-0.05,y+2.5,par("usr")[4],col="red",border=NA)
  for( i in 1:length(z) )
  {
    rect(z-0.5,par("usr")[3],z+11.5,0,col="black",border=NA)
    rect(z+11.5,par("usr")[3],z+23.5,0,col="white",border=NA)
    text( x=z+5.5, y=par("usr")[3]+0.04, labels=zz[c(1,3)], col="white"  )
    text( x=z+17.5, y=par("usr")[3]+0.04, labels=zz[c(2,4)] )
  }
  box()
  for( i in 1:nMP )
  {
    points( x=1:96, y=(wBr30[ ,i]), col=cols[i], cex=0.6, pch=as.character(Design[[3]][,1]) )
    #lines( x=1:96, y=(wBr30[ ,i]), col=cols[i-1], lwd=0.6 )
  }
  axis( side=1, at=seq(10,90,by=20) )
  legend( x="topleft", legend=jnames, bty="n", pt.cex=0.6,
          col=cols, pch=1, cex=0.6 )

  plot( x=c(1,96), y=c(0,1.15*max(eBr30)), type="n", las=1, main="East" )
  plotbg()
  for( i in 1:length(x) )
    rect(x-0.5,par("usr")[3],x+5.5,par("usr")[4],col="grey80",border=NA)
  grid( col="white", lty=1 )
  abline( v=seq(10,90,by=10), col="white" )
  abline( v=seq(5,95,by=10), col="white", lty=2 )
  for( i in 1:length(y) )
    rect(y-0.5,par("usr")[4]-0.05,y+2.5,par("usr")[4],col="red",border=NA)
  for( i in 1:length(z) )
  {
    rect(z-0.5,par("usr")[3],z+11.5,0,col="black",border=NA)
    rect(z+11.5,par("usr")[3],z+23.5,0,col="white",border=NA)
    text( x=z+5.5, y=par("usr")[3]+0.04, labels=zz[c(1,3)], col="white"  )
    text( x=z+17.5, y=par("usr")[3]+0.04, labels=zz[c(2,4)] )
  }
  box()
  for( i in 1:nMP )
  {    points( x=1:96, y=(eBr30[ ,i]), col=cols[i], cex=0.6, pch=as.character(Design[[3]][,1]) )
    #lines( x=1:96, y=(eBr30[ ,i]), col=cols[i-1], lwd=0.6 )
  }
  axis( side=1, at=seq(10,90,by=20) )
  legend( x="topleft", legend=jnames, bty="n", pt.cex=0.6,
          col=cols, pch=1, cex=0.6 )

#  plot( x=c(1,96), y=c(0,1.15*max(wBr30[ ,8:11])), type="n", las=1 )
#  plotbg()
#  for( i in 8:11 )
#  {
#    points( x=1:96, y=(wBr30[ ,i]), col=cols[i-7], cex=0.6 )
#    lines( x=1:96, y=(wBr30[ ,i]), col=cols[i-7], lwd=0.6 )
#  }
#  legend( x="topleft", legend=jnames[8:11], bty="n", pt.cex=0.6,
#          col=cols[1:4], pch=1, cex=0.8 )
#  axis( side=3, labels=NA )
#  
#  plot( x=c(1,96), y=c(0,1.15*max(eBr30[ ,8:11])), type="n", las=1 )
#  plotbg()
#  for( i in 8:11 )
#  {
#    points( x=1:96, y=(eBr30[ ,i]), col=cols[i-7], cex=0.6 )
#    lines( x=1:96, y=(eBr30[ ,i]), col=cols[i-7], lwd=0.6 )
#  }
#  legend( x="topleft", legend=jnames[8:11], bty="n", pt.cex=0.6,
#          col=cols[1:4], pch=1, cex=0.8 )
#  axis( side=3, labels=NA )

  mtext( side=1, text="OM", outer=TRUE )
  mtext( side=2, text=par, outer=TRUE )

}

plotOrderedStat <- function( par="Br30", mods=2:4, emps=5:6 )
{
  load("mseStats.Rdata")
  eBr30 <- mseStats[["east"]][ , ,par]
  wBr30 <- mseStats[["west"]][ , ,par]

  N <- ncol(eBr30)
  nM <- length(mods)
  nE <- length(emps)

  modNames <- dimnames(mseStats[[1]][ ,mods, ])[[2]]
  modNames <- gsub(".*_","",modNames)
  empNames <- dimnames(mseStats[[1]][ ,emps, ])[[2]]
  empNames <- gsub(".*_","",empNames)  

  cols <- brewer.pal(6,"Set1")
  
  par( mfrow=c(2,2), mar=c(2,2,1,1), oma=c(2,2,1,0) )

  plot( x=c(1,96), y=c(0,max(wBr30[ ,mods])), type="n", las=1, main="West" )
  plotbg()
  for( i in 1:nM )
    points( x=1:96, y=sort(wBr30[ ,mods[i]]), col=cols[i], cex=0.6 )
  legend( x="topleft", legend=modNames, bty="n", pt.cex=0.6,
          col=cols[1:nM], pch=1, cex=0.8 )

  plot( x=c(1,96), y=c(0,max(eBr30[ ,mods])), type="n", las=1, main="East" )
  plotbg()
  for( i in 1:nM )
    points( x=1:96, y=sort(eBr30[ ,mods[i]]), col=cols[i], cex=0.6 )
  legend( x="topleft", legend=modNames, bty="n", pt.cex=0.6,
          col=cols[1:nM], pch=1, cex=0.8 )

  plot( x=c(1,96), y=c(0,max(wBr30[ ,emps])), type="n", las=1 )
  plotbg()
  for( i in 1:nE )
    points( x=1:96, y=sort(wBr30[ ,emps[i]]), col=cols[i], cex=0.6 )
  legend( x="topleft", legend=empNames, bty="n", pt.cex=0.6,
          col=cols[1:nE], pch=1, cex=0.8 )
  axis( side=3, labels=NA )
  
  plot( x=c(1,96), y=c(0,max(eBr30[ ,emps])), type="n", las=1 )
  plotbg()
  for( i in 1:nE )
    points( x=1:96, y=sort(eBr30[ ,emps[i]]), col=cols[i], cex=0.6 )
  legend( x="topleft", legend=empNames, bty="n", pt.cex=0.6,
          col=cols[1:nE], pch=1, cex=0.8 )
  axis( side=3, labels=NA )

  mtext( side=1, text="OM rank", outer=TRUE )
  mtext( side=2, text=par, outer=TRUE )

}


plotOrderedStat2 <- function( par1="C10", par2="Br30" )
{
  load("mseStats.Rdata")
  eC10 <- mseStats[["east"]][ , ,par1]
  wC10 <- mseStats[["west"]][ , ,par1]
  eBr30 <- mseStats[["east"]][ , ,par2]
  wBr30 <- mseStats[["west"]][ , ,par2]

  jnames <- names(mseStats[[1]][1, ,1])
  jnames <- gsub(".*_","",jnames)

  cols <- brewer.pal(6,"Set1")
  
  par( mfrow=c(2,2), mar=c(2,2,1,1), oma=c(2,2,1,0) )

  plot( x=c(0,max(wC10[ ,2:7])), y=c(0,max(wBr30[ ,2:7])), type="n", las=1, main="West" )
  plotbg()
  for( i in 2:7 )
    points( x=sort(wC10[ ,i]), y=sort(wBr30[ ,i]), col=cols[i-1], cex=0.6 )
  legend( x="topleft", legend=jnames[2:7], bty="n", pt.cex=0.6,
          col=cols[1:6], pch=1, cex=0.8 )

  plot( x=c(0,max(eC10[ ,2:7])), y=c(0,max(eBr30[ ,2:7])), type="n", las=1, main="East" )
  plotbg()
  for( i in 2:7 )
    points( x=sort(eC10[ ,i]), y=sort(eBr30[ ,i]), col=cols[i-1], cex=0.6 )
  legend( x="bottomleft", legend=jnames[2:7], bty="n", pt.cex=0.6,
          col=cols[1:6], pch=1, cex=0.8 )

  plot( x=c(0,max(wC10[ ,8:11])), y=c(0,max(wBr30[ ,8:11])), type="n", las=1 )
  plotbg()
  for( i in 8:11 )
    points( x=sort(wC10[ ,i]), y=sort(wBr30[ ,i]), col=cols[i-7], cex=0.6 )
  legend( x="bottomleft", legend=jnames[8:11], bty="n", pt.cex=0.6,
          col=cols[1:4], pch=1, cex=0.8 )
  axis( side=3, labels=NA )
  
  plot( x=c(0,max(eC10[ ,8:11])), y=c(0,max(eBr30[ ,8:11])), type="n", las=1 )
  plotbg()
  for( i in 8:11 )
    points( x=sort(eC10[ ,i]), y=sort(eBr30[ ,i]), col=cols[i-7], cex=0.6 )
  legend( x="topright", legend=jnames[8:11], bty="n", pt.cex=0.6,
          col=cols[1:4], pch=1, cex=0.8 )
  axis( side=3, labels=NA )

  mtext( side=1, text=par1, outer=TRUE )
  mtext( side=2, text=par2, outer=TRUE )

}

plotOrderedEvW <- function( par="Br30", mods=2:4 )
{
  load("mseStats.Rdata")
  eBr30 <- mseStats[["east"]][ , ,par]
  wBr30 <- mseStats[["west"]][ , ,par]

  nM <- length(mods)

  modNames <- dimnames(mseStats[[1]][ ,mods, ])[[2]]
  modNames <- gsub(".*_","",modNames)

  cols <- brewer.pal(6,"Set1")

  par( mfrow=c(1,1), mar=c(2,2,1,1), oma=c(2,3,1,0) )
  #plot( x=c(0,max(wBr30[ ,mods],eBr30[ ,mods])), y=c(0,max(eBr30[ ,mods],wBr30[ ,mods])), type="n", las=1 )
  plot( x=c(0,0.1), y=c(0,0.1), type="n", las=1 )
  plotbg()
  for( i in 1:nM )
  {
    points( x=sort(wBr30[ ,mods[i]]), y=sort(eBr30[ ,mods[i]]),
            col=cols[i], cex=0.6 )
  }
  legend( x="bottomright", legend=modNames, bty="n", pt.cex=0.6,
          col=cols[1:nM], pch=1, cex=0.8 )
  abline(a=0,b=1)

  mtext( side=1, text="West Br30", outer=TRUE )
  mtext( side=2, text="East Br30", outer=TRUE, line=1 )

}

plotUnorderedEvW <- function( mod=3, par="Br30" )
{
  load("mseStats.Rdata")
  eBr30 <- mseStats[["east"]][ , ,par]
  wBr30 <- mseStats[["west"]][ , ,par]

  nOM <- nrow(eBr30)

  par( mfrow=c(1,1), mar=c(2,2,1,1), oma=c(2,3,1,0) )
  plot( x=c(0,max(wBr30[ ,mod],eBr30[ ,mod])), y=c(0,max(eBr30[ ,mod],wBr30[ ,mod])), type="n", las=1 )
  #plot( x=c(0,0.1), y=c(0,0.1), type="n", las=1 )
  plotbg()
  x <- wBr30[ ,mod]
  y <- eBr30[ ,mod]
  text( x=x, y=y, cex=0.6, labels=1:nOM )
  abline(a=0,b=1)


  mtext( side=1, text="West Br30", outer=TRUE )
  mtext( side=2, text="East Br30", outer=TRUE, line=1 )

  crashFlag <- x==0 & y==0

  Design[[3]][crashFlag, ]

}

plotRankEvW <- function( par="Br30" )
{
  load("mseStats.Rdata")
  eBr30 <- mseStats[["east"]][ , ,par]
  wBr30 <- mseStats[["west"]][ , ,par]

  jnames <- names(mseStats[[1]][1, ,1])
  jnames <- gsub(".*_","",jnames)

  cols <- brewer.pal(6,"Set1")
  
  par( mfrow=c(1,2), mar=c(2,2,1,1), oma=c(2,3,1,0) )

  plot( x=1:96, y=1:96, type="n", las=1 )
  plotbg()
  for( i in 2:7 )
    points( x=order(wBr30[ ,i]), y=order(eBr30[ ,i]), col=cols[i-1], cex=0.6 )
  legend( x="topleft", legend=jnames[2:7], bty="n", pt.cex=0.6,
          col=cols[1:6], pch=1, cex=0.8 )

  plot( x=1:96, y=1:96, type="n", las=1 )
  plotbg()
  for( i in 8:11 )
    points( x=order(wBr30[ ,i]), y=order(eBr30[ ,i]), col=cols[i-7], cex=0.6 )
  legend( x="topleft", legend=jnames[8:11], bty="n", pt.cex=0.6,
          col=cols[1:4], pch=1, cex=0.8 )

  mtext( side=1, text="West OM rank", outer=TRUE )
  mtext( side=2, text="East OM rank", outer=TRUE, line=1 )

}

plotBiomass <- function( repObj=report,
                         pdfFile=NULL )
{
  yr <- OMI_1@years[1]:(OMI_1@years[2]+1)
  nT <- length(yr)-1
  cols <- brewer.pal(3,"Set1")
  R_st <- repObj$R_st*repObj$wk_s
  B_st <- repObj$B_st
  B_at <- repObj$B_at
  C_at <- repObj$C_at
  p_sat <- repObj$propCatch_sat

  if( !is.null(pdfFile) )
    pdf( pdfFile, height=6, width=7.5 )
  par(mfcol=c(2,2),mar=c(0,0,0,0),oma=c(3,5,3,5))

  stocks <- c("East stock","West stock")
  maxy_s <- 1.2*c( max(B_st[1, ],B_at[1, ]),
                   max(B_st[2, ],B_at[2, ]) )


  for( s in 1:2 )
  {
    plot( x=yr, y=B_st[s, ], type="l", lwd=2, col="black",
          ylim=c(0,maxy_s[s]), yaxs="i", xlab="", ylab="", axes=0 )
    axis(side=2,las=1)
    if(s==1)
      axis(side=3)
    else
      axis(side=1)
    grid()
    box()
    par(font=2)
    legend( legend=stocks[s], x="topleft", bty="n", cex=1.2 )
    par(font=1)
    lines( x=yr, y=B_st[s, ], lwd=2 )
  }

  areas <- c("East area","West area")
  maxC_a <- 1.2*c( max(C_at[1, ]), max(C_at[2, ]) )
  for( a in 1:2 )
  {
    C_ts <- C_at[a, ]*t(p_sat[ ,a, ])
    plot( x=yr[1:nT], y=B_at[a, ], type="l", lwd=2, col="black", las=1,
          ylim=c(0,maxy_s[a]), yaxs="i", xlab="", ylab="", axes=FALSE )
    if(a==1)
      axis(side=3)
    else
      axis(side=1)
    grid()
    box()
    par(font=2)
    legend( legend=areas[a], x="topleft", bty="n", cex=1.2 )
    par(font=1)
    lines( x=yr[1:nT], y=B_at[a, ], lwd=2 )
    par(new=TRUE)
    plot( x=yr, y=yr, type="l", lwd=2, col="black", las=1,
          ylim=c(0,maxC_a[a]), yaxs="i", xlab="", ylab="", axes=0 )
    if( a==2 )
      axis( side=4, las=1, at=seq(0,20,by=5) )
    else
      axis( side=4, las=1 )
    rect( xleft=yr[1:nT]-0.4, xright=yr[1:nT]-0.05,
          ybottom=0, ytop=C_ts[ ,1],
          col=cols[1], border=cols[1] )
    rect( xleft=yr[1:nT]+0.05, xright=yr[1:nT]+0.4,
          ybottom=0, ytop=C_ts[ ,2],
          col=cols[2], border=cols[2] )
  }

  mtext( side=2, text="Biomass (kt)", outer=TRUE, cex=1.3, line=3 )
  mtext( side=4, text="Catch (kt)", outer=TRUE, cex=1.3, line=3 )

  if( !is.null(pdfFile) )
    dev.off()
}

plotBiasAM <- function()
{
  load("AMfits/AMfits.Rdata")

  AMs <- paste("OM",names(AMfits),sep="_")

  z <- read.csv("../OMs/empPars.csv")
  z <- z[as.numeric(names(AMfits)),]

  B0om <- t(z[ ,c("B0_MED","B0_GOM")])
  Bom <- t(z[ ,c("Bmsy_MED","Bmsy_GOM")])
  Fom <- t(z[ ,c("Fmsy_MED","Fmsy_GOM")])
  Mom <- t(z[ ,c("MSY_MED","MSY_GOM")])


  Bam <- cbind( AMfits[[1]]$Bmsy, AMfits[[2]]$Bmsy, AMfits[[3]]$Bmsy,
                AMfits[[4]]$Bmsy, AMfits[[5]]$Bmsy )
  Fam <- cbind( AMfits[[1]]$Fmsy, AMfits[[2]]$Fmsy, AMfits[[3]]$Fmsy,
                AMfits[[4]]$Fmsy, AMfits[[5]]$Fmsy )
  Mam <- cbind( AMfits[[1]]$msy, AMfits[[2]]$msy, AMfits[[3]]$msy,
                AMfits[[4]]$msy, AMfits[[5]]$msy )

  par( mfrow=c(3,2), mar=c(2,3,1,1), oma=c(2,2,0,0) )

  X <- ncol(Bom)

  plot( 1:X, Bom[2, ], ylim=c(0,1.1*max(Bom[2, ],Bam[2, ])),
        axes=FALSE, main="West" )
  grid()
  box()
  axis( side=1, at=1:X, labels=AMs )
  axis( side=2, las=1 )
  points( 1:X, Bom[2, ], lwd=1.5, pch=0 )
  points( 1:X, Bam[2, ], col="red", lwd=1.5, pch=2 )
  mtext( side=2, text="BMSY", line=3 )
  legend( x="topleft", legend=c("OM","AM"), lwd=1.5, lty=0,
          col=c("black","red"), pch=c(0,2) )

  plot( 1:X, Bom[1, ], ylim=c(0,1.1*max(Bom[1, ],Bam[1, ])),
        axes=FALSE, main="East" )
  grid()
  box()
  axis( side=1, at=1:X, labels=AMs )
  axis( side=2, las=1 )
  points( 1:X, Bom[1, ], lwd=1.5, pch=0 )
  points( 1:X, Bam[1, ], col="red", lwd=1.5, pch=2 )

  plot( 1:X, Fom[2, ], ylim=c(0,1.1*max(Fom[2, ],Fam[2, ])),
        axes=FALSE )
  grid()
  box()
  axis( side=1, at=1:X, labels=AMs )
  axis( side=2, las=1 )
  axis( side=3, labels=NA )
  points( 1:X, Fom[2, ], lwd=1.5, pch=0 )
  points( 1:X, Fam[2, ], col="red", lwd=1.5, pch=2 )
  mtext( side=2, text="FMSY", line=3 )

  plot( 1:X, Fom[1, ], ylim=c(0,1.1*max(Fom[1, ],Fam[1, ])),
        axes=FALSE )
  grid()
  box()
  axis( side=1, at=1:X, labels=AMs )
  axis( side=2, las=1 )
  axis( side=3, labels=NA )
  points( 1:X, Fom[1, ], lwd=1.5, pch=0 )
  points( 1:X, Fam[1, ], col="red", lwd=1.5, pch=2 )

  plot( 1:X, Mom[2, ], ylim=c(0,1.1*max(Mom[2, ],Mam[2, ])),
        axes=FALSE )
  grid()
  box()
  axis( side=1, at=1:X, labels=AMs )
  axis( side=2, las=1 )
  axis( side=3, labels=NA )
  points( 1:X, Mom[2, ], lwd=1.5, pch=0 )
  points( 1:X, Mam[2, ], col="red", lwd=1.5, pch=2 )
  mtext( side=2, text="MSY", line=3 )

  plot( 1:X, Mom[1, ], ylim=c(0,1.1*max(Mom[1, ],Mam[1, ])),
        axes=FALSE )
  grid()
  box()
  axis( side=1, at=1:X, labels=AMs )
  axis( side=2, las=1 )
  axis( side=3, labels=NA )
  points( 1:X, Mom[1, ], lwd=1.5, pch=0 )
  points( 1:X, Mam[1, ], col="red", lwd=1.5, pch=2 )

}







