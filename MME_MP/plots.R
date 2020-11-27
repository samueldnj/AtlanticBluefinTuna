# --------------------------------------------------------------------------
# plots.R
# 
# plots for testing the DD MP
#
# Author: Samuel D N Johnson
# Date: July 16, 2019
#
# --------------------------------------------------------------------------

source("empiricalMP.R")
source("MPs.R")
library(ABTMSE)
library(vioplot)

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

plotViolin <- function( MSE = c("sep17","sep7","eastMgrid"),
                        OMvec = 1:96, 
                        prefix = NULL,
                        ptcex = 1,
                        saveStats = TRUE )
{
  folders <- paste0("./MSEs/",MSE)

  allMSE <- vector(mode = "list", length = length(MSE))
  J <- numeric(length=length(MSE))

  for( k in 1:length(MSE))
  {
    # First list.dirs
    MSEdir <- list.dirs(folders[k],recursive = FALSE, full.names = FALSE )
    if("rdas" %in% MSEdir )
    {

      X <- paste0( folders[k], "/rdas/MSE_", OMvec, ".rda" )
      allMSE[[k]] <- lapply(  X = X,
                              FUN = readRDS )
      names( allMSE[[k]] ) <- paste("MSE_",OMvec)

    } else {
      OMlabs <- paste("OM_",OMvec,"d", sep = "")
      allMSE[[k]] <- lapply( X = OMlabs, 
                     FUN = loadMSE, 
                     prefix = NULL,
                     folder = folders[k] )
      names(allMSE[[k]]) <- OMlabs
    }
    
    J[k] <- allMSE[[k]][[1]]@nMPs
    if(k > 1)
      J[k] <- J[k] - 1
  }

  I <- length(allMSE[[1]])


  jEast <- character(sum(J))
  jWest <- character(sum(J))
  k <- 1
  nudge <- 0
  for( iMSE in 1:length(J) )
  {
    if( iMSE > 1 )
      nudge <- 1
    for( iMP in (1:J[iMSE]+nudge) )
    {
      jEast[k] <- allMSE[[iMSE]][[iMSE]]@MPs[[iMP]][1]
      jWest[k] <- allMSE[[iMSE]][[iMSE]]@MPs[[iMP]][2]
      k <- k+1
    }
  }
  j2 <- character(length(jEast))

  MPcols  <-  RColorBrewer::brewer.pal( 3,  "Set1")

  cols <- rep("grey",length(k))
  #cols[2:5] <- MPcols[1]
  #cols[6:7] <- MPcols[2]
  #cols[8] <- MPcols[3]
  
#  x <- c("AvC30","C10","C20","C30","D10","D20","D30","LD","DNC",
#         "LDNC","POF","POS","PGK","AAVC","Br30")
  x <- c("C10","C20","C30","D30","LD","Br30")
  X <- length(x)

  # Let's pull MPs from MSE objects

  # j <- c("No Catch",
  #        #"emp_noCap", 
  #        # "emp_noCapB0",
  #        # "emp_noCapFM",
  #        # "emp_noCapFMB0" )
  #         "emp_trendTAC" )

  east <- array( data=NA, dim=c(I,sum(J),X), dimnames=list(OMvec,jEast,x) )
  west <- array( data=NA, dim=c(I,sum(J),X), dimnames=list(OMvec,jWest,x) )

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

  if( saveStats )
  {
    mseStats <- list(east=east,west=west)
    save( mseStats, file="mseStats.Rdata" )
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
    westMat <- west[,,k]
    vioplot(  westMat,
              use.cols = TRUE,
              ylim=c(0,ymax[1]),
              ylab=paste("",x[k]), las=1, names=j2 )
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col="white", border=NA )
    grid()

    vioplot(  westMat,
              use.cols = TRUE,
              ylab=paste("",x[k]), las=1, add=TRUE, col=cols )

    if( k==X )
      abline( h=1, lty=2 )

    par( mar=c(0.4,2,0,0.5) )
    eastMat <- east[,,k]
    vioplot(  eastMat,
              use.cols = TRUE,
              ylim=c(0,ymax[2]),
              ylab=paste("",x[k]), las=1, names=j2 )
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col="white", border=NA )
    grid()
    vioplot(  eastMat,
              use.cols = TRUE,
              ylab=paste("",x[k]), las=1, add=TRUE, col=cols )

    if( k==X )
      abline( h=1, lty=2 )
  }

  parr <- par("usr")
  par( mar=c(0,4,0,0.5) )
  labels <- list(jWest = jWest, jEast = jEast)
  for( h in 1:2 )
  {
    if( h==2 )
      par( mar=c(0,2,0,0.5) )
    plot( x=c(parr[1],parr[2]), y=c(0,1), type="n", yaxs="i",
          axes=FALSE, xlab="", ylab="" )
    text( seq(0.9,sum(J)+0.3,length.out=sum(J)), par("usr")[4]-0.04, labels=labels[[h]],
          srt=45, adj=c(1.1,1.1), xpd=TRUE, cex=.9 )
  }
# triangles at max, horiz line at 1, symbols for axes,
# vertical line at last ten yrs avg C
  par( font=2 )
  mtext( side=3, text="         West                                                    East", outer=TRUE )
  par( font=1 )

}


plot_MPfits <- function(  simNum      = 1,
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


  # Count OMs
  OMs   <- unique(checkTable$OM)
  nOMs  <- length(OMs)

  # Count years
  yrs   <- 1965:2070
  tMP   <- 2020

  MPyrs <- seq( from  = tMP-1, to = max(yrs),
                by = interval )

  yrCol <- rep( MPyrs, rep(5,length(MPyrs)) )

  checkTable$yr   <- rep( MPyrs, rep(5,length(MPyrs)) )

  OMcols <- RColorBrewer::brewer.pal(n = length(OMs), "Dark2")
  checkTable$col  <- OMcols

  OMlabels <- paste( "OM", as.character(OMs), sep = "_" )

  # Set up the E/W plot
  par( mfrow = c(2,1), mar =c(2,1,2,1), oma = c(4,4.5,2,1) )

  plot( x = range(yrs), y = c(0,max(SSB[1,],checkTable$B_MED,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1 )
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    lines( x = yrs, y = SSB[1,], col = "red", lwd = 3 )
    points( x = checkTable$yr + 1, y = checkTable$B_MED,
            pch = 16, col = checkTable$col )
    mtext( side = 3, font = 2, text = "East Stock")

  plot( x = range(yrs), y = c(0,max(SSB[2,],checkTable$B_GOM,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1 )
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    lines( x = yrs, y = SSB[2,], col = "red", lwd = 3 )
    points( x = checkTable$yr + 1, y = checkTable$B_GOM,
            pch = 16, col = checkTable$col )
    mtext( side = 3, font = 2, text = "West Stock")
    legend( x = "topleft", legend = OMlabels,
            pch = 16, col = OMcols, bty = "n" )

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
    mtext( side = 2, text = "Harvest Rate (/yr)",
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
    legend( x = "topleft", bty = "n",
            legend = expression( 0.4*B[MSY], B[MSY], "Uncapped", "Capped" ),
            col = c("red","orange","grey50","black"),
            lty = c(2,2,1,1), lwd = 2,
             )

  mtext( side = 1, outer = TRUE, text = "Spawning Biomass (kt)")


}

plotbg <- function(col=rgb(235,235,235,maxColorValue=255))
{
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=col)
  grid( col="white", lty=1 )
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

plot_TACperformance <- function(  simIdx      = 1,
                                  MSEobj      = MSEtest_ROM_1d,
                                  westTables  = westCheckTables,
                                  eastTables  = eastCheckTables,
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

  # Count OMs
  OMs   <- unique(westCheckTable$OM)
  nOMs  <- length(OMs)

  # Count years
  yrs   <- 1965:2070
  tMP   <- 2020

  MPyrs <- seq( from  = tMP-1, to = max(yrs),
                by = interval )

  yrCol <- rep( MPyrs, rep(nOMs,length(MPyrs)) )

  eastCheckTable$yr   <- rep( MPyrs[-length(MPyrs)], rep(nOMs,length(MPyrs)-1) )
  westCheckTable$yr   <- rep( MPyrs[-length(MPyrs)], rep(nOMs,length(MPyrs)-1) )

  OMcols <- RColorBrewer::brewer.pal( n=nOMs, "Dark2" )
  westCheckTable$col  <- OMcols
  eastCheckTable$col  <- OMcols

  OMlabels <- paste( "OM", as.character(OMs), sep = "_" )

  westCatchCheckTable <-  westCheckTable %>%
                          group_by(yr) %>%
                          summarise(  meanTAC_E = mean(TAC_E),
                                      meanTAC_W = mean(TAC_W),
                                      minTAC_E  = min(TAC_E),
                                      minTAC_W  = min(TAC_W),
                                      q.05TAC_E = quantile( TAC_E, probs = 0.05),
                                      q.05TAC_W = quantile( TAC_W, probs = 0.05),
                                      q.95TAC_E = quantile( TAC_E, probs = 0.95),
                                      q.95TAC_W = quantile( TAC_W, probs = 0.95) )

  eastCatchCheckTable <-  eastCheckTable %>%
                          group_by(yr) %>%
                          summarise(  meanTAC_E = mean(TAC_E),
                                      meanTAC_W = mean(TAC_W),
                                      minTAC_E  = min(TAC_E),
                                      minTAC_W  = min(TAC_W),
                                      q.05TAC_E = quantile( TAC_E, probs = 0.05),
                                      q.05TAC_W = quantile( TAC_W, probs = 0.05),
                                      q.95TAC_E = quantile( TAC_E, probs = 0.95),
                                      q.95TAC_W = quantile( TAC_W, probs = 0.95) )

  layoutMat <- matrix( c( 1,1,1,1,
                          1,1,1,1,
                          2,2,2,2,
                          3,3,3,3,
                          3,3,3,3,
                          4,4,4,4), nrow = 6, ncol = 4,
                                    byrow = TRUE ) 

  layout( layoutMat )

  # Set up the E/W plot
  par( mar =c(0,1,2,1), oma = c(4,4.5,2,3) )

  plot( x = range(yrs), y = c(0,max(SSB[1,],eastCheckTable$B_MED,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1, axes = F )
    axis( side = 2, las = 1)
    box()
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    lines( x = yrs, y = SSB[1,], col = "red", lwd = 3 )
    points( x = eastCheckTable$yr + 1, y = eastCheckTable$B_MED,
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

  plot( x = range(yrs), y = c(0,max(SSB[2,],westCheckTable$B_GOM,na.rm = T)),
        xlab = "", ylab = "", type = "n", las = 1, axes = F )
    axis( side = 2, las = 1)
    box()
    grid()
    abline( v = tMP, col = "black", lty = 2, lwd = .8 )
    lines( x = yrs, y = SSB[2,], col = "red", lwd = 3 )
    points( x = westCheckTable$yr + 1, y = westCheckTable$B_GOM,
            pch = 16, col = westCheckTable$col )
    mtext( side = 3, font = 2, text = "West Stock")
    legend( x = "topleft", legend = OMlabels,
            pch = 16, col = OMcols, bty = "n" )
    mtext(  side = 4, text = westMPid, 
            font = 2, cex =  1, line = 2 )

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
plotAllMSEs <- function(  OMvec = paste("OM_",1:15,sep = ""), 
                          prefix = "",
                          folder = "./MSEs/hiLoCaps" )
{
  MSElist <- lapply(  X = OMvec, 
                      FUN = loadMSE, 
                      prefix = prefix,
                      folder = folder )
  names(MSElist) <- OMvec

  nMSE <- length(MSElist)
  graphics.off()

  outFolder <- file.path(folder,"plots")
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
# AvC30 (Average catch over first 30 years) for all MPs and OMs
plotMSEperf <- function(  OMvec = paste("OM",1:15,sep = "_"),
                          projFolder = "./MSEs/hiLoCaps", 
                          prefix = "",
                          ptcex = 1, segwd = 1 )
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
  AvC30quants <- sapply( X = MSElist, FUN = getCquants, simplify = "array", tIdx = 53:82 )
  
  MPnames <- lapply( X = MSElist, FUN = getMPnames )

  MPnameUnion <- MPnames[[1]]
  if( nMSE > 1 )
  {
    for( j in 2:nMSE )
      MPnameUnion <- union(MPnameUnion,MPnames[[j]])
  }

  MPnames <- MPnameUnion
  nMPs <- dim(B30quants)[2]
  
  MPcols  <-  RColorBrewer::brewer.pal( nMPs,  "Dark2")
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
            lty = 3, lwd = .5, col = "grey30" )
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
                  y1 = B30quants[3,k,1,j], col = MPcols[k],
                  lwd = segwd )
        # Median
        points( x = splits[k] + j, y = B30quants[2,k,1,j],
                pch = MPpch[k], col = MPcols[k], cex = ptcex )

      }
    }
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
            lty = 3, lwd = .5, col = "grey30" )
    # Loop over MSEs and MPs, plot performance
    for( j in 1:nMSE )
    {
      splits <- seq( from = -.3, to = .3, length.out = nMPs )
      for( k in 1:nMPs )
      {
        # Central 90%
        segments( x0 = splits[k] + j, 
                  y0 = B30quants[1,k,2,j],
                  y1 = B30quants[3,k,2,j], col = MPcols[k],
                  lwd = segwd )
        # Median
        points( x = splits[k] + j, y = B30quants[2,k,2,j],
                pch = MPpch[k], col = MPcols[k], cex = ptcex )

      }
    }
    mtext( side = 3, text = "West Stock" )


  ##### Plot ave catch over third 10 years ####
  plot( x = c(1, length(MSElist)), y = range(AvC30quants[,,1,]),
        axes = F, type = "n", xlab = "", ylab = "" )
    mfg <- par("mfg")
    axis( side = 1, at = 1:nMSE, labels = OMvec, las = 2 )
    axis( side = 2, las = 1 )
    box()
    # abline( h = c(0.5, 1, 1.5,2,2.5,3), lty = c(2,1,2,2,2,2)+1, 
    #         lwd = c(0.8,1.5,0.8,.8,.8,.8),
    #         col = "grey40" )
    abline( v = seq(from = 1.5, to = nMSE - .5, by = 1),
            lty = 3, lwd = .5, col = "grey30" )
    mtext( side = 2, text = expression(paste(C[30], " (kt)",sep = "")), line = 2.5)
    # Loop over MSEs and MPs, plot performance
    for( j in 1:nMSE )
    {
      
      splits <- seq( from = -.3, to = .3, length.out = nMPs )
      for( k in 1:nMPs )
      {
        # Central 90%
        segments( x0 = splits[k] + j, 
                  y0 = AvC30quants[1,k,1,j],
                  y1 = AvC30quants[3,k,1,j], col = MPcols[k],
                  lwd = segwd )
        # Median
        points( x = splits[k] + j, y = AvC30quants[2,k,1,j],
                pch = MPpch[k], col = MPcols[k], cex = ptcex )

      }
    }
    legend( x = "bottomleft", legend = MPnames,
            pch = 16, col = MPcols,
            lwd = segwd, cex = .8, bg = "white" )
    


  plot( x = c(1, length(MSElist)), y = range(AvC30quants[,,2,]),
        axes = F, type = "n", xlab = "", ylab = "" )
    axis( side = 1, at = 1:nMSE, labels = OMvec, las = 2 )
    axis( side = 2, las = 1 )
    box()
    # abline( h = c(0.5, 1, 1.5,2,2.5,3), 
    #         lty = c(2,1,2,2,2,2)+1, 
    #         lwd = c(0.8,1.5,0.8,.8,.8,.8),
    #         col = "grey40" )
    abline( v = seq(from = 1.5, to = nMSE - .5, by = 1),
            lty = 3, lwd = .5, col = "grey30" )
    # Loop over MSEs and MPs, plot performance
    for( j in 1:nMSE )
    {
      splits <- seq( from = -.3, to = .3, length.out = nMPs )
      for( k in 1:nMPs )
      {
        # Central 90%
        segments( x0 = splits[k] + j, 
                  y0 = AvC30quants[1,k,2,j],
                  y1 = AvC30quants[3,k,2,j], col = MPcols[k],
                  lwd = .8 )
        # Median
        points( x = splits[k] + j, y = AvC30quants[2,k,2,j],
                pch = MPpch[k], col = MPcols[k], cex = ptcex )

      }
    }


}


