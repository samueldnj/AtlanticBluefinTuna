# glm.R

fitEW <- function( mseStatsFile="mseStats.Rdata" )
{
  load(mseStatsFile)
  MPs <- dimnames(mseStats[[1]])[[2]]
  N <- length(MPs)
  facs <- c("Intercept","SR2","SR3","M2","Mix2","SSB2","SSB3","SSB4","LC2")
  nF <- length(facs)
  z <- array( data=NA, dim=c(N,nF,3,2) )
  for( i in 1:N )
  {
    fitW <- fitGLM( MP=MPs[i], stock="west", mseStatsFile=mseStatsFile )
    fitE <- fitGLM( MP=MPs[i], stock="east", mseStatsFile=mseStatsFile )
    estW <- round(summary(fitW)$coefficients[ ,c(1,4)],3)
    estE <- round(summary(fitE)$coefficients[ ,c(1,4)],3)
    for( j in 1:nF )
    {
      z[i, ,c(1,3),1] <- suppressMessages(confint(fitW))
      z[i, ,c(1,3),2] <- suppressMessages(confint(fitE))
      z[i, ,2,1] <- fitW$coefficients
      z[i, ,2,2] <- fitE$coefficients
    }
  }

  cols <- brewer.pal(3,"Set1")[1:2]
  par( mfrow=c(3,3), mar=c(0.5,4,0,1), oma=c(11,0,1,0) )
  for( j in 2:nF )
  {
    plot( x=c(0.5,N+0.5), y=range(z[ ,-1, , ]), las=1, type="n",
          ylab=facs[j], axes=FALSE )
    parr <- par("usr")
    grid()
    box()
    axis( side=2, las=1 )
    segments( x0=1:N-0.1, y0=z[ ,j,1,1], y1=z[ ,j,3,1], col=cols[1], lwd=2 )
    segments( x0=1:N+0.1, y0=z[ ,j,1,2], y1=z[ ,j,3,2], col=cols[2], lwd=2 )
    points( x=1:N-0.1, y=z[ ,j,2,1], col=cols[1], pch=16 )
    points( x=1:N+0.1, y=z[ ,j,2,2], col=cols[2], pch=16 )
    abline( h=0, lty=2 )
    if( j<7 )
      axis( side=1, labels=NA )
    else
      axis( side=1, las=2, at=1:N, labels=MPs )
    if( j==1 )
    {
      legend( x="topright", bty="n", cex=0.9, col=cols, lwd=2, legend=c("West","East") )
      legend( x="topright", bty="n", cex=0.9, col=cols, pch=16, lwd=0, legend=c("West","East") )
    }
  }

  #plot( x=c(0,1), y=c(0,1), type="n", axes=FALSE, xlab="", ylab="" )
  #legend( x="topleft", bty="n", legend=MPs, col=cols, pch=16, cex=0.9, pt.cex=1.2 )

  mtext( side=1, text="Management procedure", outer=TRUE, line=9 )


}

fitGLMs <- function( stock="both" )
{
  load("mseStats.Rdata")
  MPs <- dimnames(mseStats[[1]])[[2]]
  N <- length(MPs)
  facs <- c("Int","SR2","SR3","M2","Mix2","SSB2","SSB3","SSB4","LC2")
  nF <- length(facs)
  coefs <- matrix( data=NA, nrow=N, ncol=nF, dimnames=list(MPs,facs) )
  z <- array( data=NA, dim=c(N,nF,3) )
  for( i in 1:N )
  {
    fit <- fitGLM( MP=MPs[i], stock=stock )
    est <- round(summary(fit)$coefficients[ ,c(1,4)],3)
    for( j in 1:nF )
    {
      z[i, ,c(1,3)] <- suppressMessages(confint(fit))
      z[i, ,2] <- fit$coefficients
      coefs[i,j] <- paste( est[j,1], " (", est[j,2], ")", sep="" )
    }
  }

  cols <- c(brewer.pal(8,"Dark2"),brewer.pal(9,"Set1"))[1:N]
  par( mfrow=c(3,3), mar=c(1,2,1,1), oma=c(9,0,0,0) )
  for( j in 1:nF )
  {
    plot( x=c(0.5,N+0.5), y=range(z[ ,j, ]), las=1, type="n", main=facs[j], axes=FALSE )
    parr <- par("usr")
    grid()
    box()
    axis( side=2, las=1 )
    segments( x0=1:N, y0=z[ ,j,1], y1=z[ ,j,3], col=cols, lwd=2 )
    points( x=1:N, y=z[ ,j,2], col=cols, pch=16 )
    abline( h=0, lty=2 )
    if( j<7 )
      axis( side=1, labels=NA )
    else
      axis( side=1, las=2, at=1:N, labels=MPs )
  }

  #plot( x=c(0,1), y=c(0,1), type="n", axes=FALSE, xlab="", ylab="" )
  #legend( x="topleft", bty="n", legend=MPs, col=cols, pch=16, cex=0.9, pt.cex=1.2 )

  mtext( side=1, text="Management procedure", outer=TRUE, line=7 )

  coefs

}

fitGLMsi <- function( stock="both" )
{
  load("mseStats.Rdata")
  MPs <- dimnames(mseStats[[1]])[[2]]
  N <- length(MPs)
  facs <- c("Int","SR2","SR3","M2","Mix2","SSB2","SSB3","SSB4","LC2",
            "SR2:M2","SR3:M2","SR2:Mix2","SR3:Mix2","SR2:SSB2","SR3:SSB2",
            "SR2:SSB3","SR3:SSB3","SR2:SSB4","SR3:SSB4","SR2:LC2","SR3:LC2",
            "M2:Mix2","M2:SSB2","M2:SSB3","M2:SSB4","M2:LC2",
            "Mix2:SSB2","Mix2:SSB3","Mix2:SSB4","Mix2:LC2",
            "SSB2:LC2","SSB3:LC2","SSB4:LC2")
  nF <- length(facs)
  coefs <- matrix( data=NA, nrow=N, ncol=nF, dimnames=list(MPs,facs) )
  p <- matrix( data=NA, nrow=N, ncol=nF, dimnames=list(MPs,facs) )
  for( i in 1:N )
  {
    fit <- fitGLM( MP=MPs[i], stock=stock, interact=TRUE )
    est <- summary(fit)$coefficients[ ,c(1,4)]
    coefs[i, ] <- est[ ,1]
    p[i, ] <- est[ ,2]
  }
  
  list(coefs,p)
}

plotInter <- function()
{
  z <- fitGLMsi( stock="east" )

  facs <- colnames(z[[1]])
  MPs <- rownames(z[[1]])

  X <- nrow(z[[1]])
  Y <- ncol(z[[1]])

  z1 <- fitGLMsi( stock="west" )
  z <- list(z1,z)

  par( mfrow=c(1,2), mar=c(6,9,2,1) )

  for( s in 1:2 )
  {
    b <- z[[s]][[1]]
    p <- z[[s]][[2]]

    plot( x=c(0,X), y=c(0,Y), type="n", xaxs="i", yaxs="i", axes=FALSE,
          xlab="", ylab="", main=c("West","East")[s] )

    for( x in 1:X )
    {
      for( y in 1:Y )
      {
        yy <- Y-y+1
        if( p[x,y] < 0.05 )
          rect( xleft=x-1, xright=x, ybottom=yy-1, ytop=yy,
                col="red", border=NA )
        text( x=x-0.5, y=yy-0.5, labels=round(b[x,y],2) )
      }
    }
    
    abline( h=0:Y )
    abline( v=0:X )

    axis( side=1, at=1:X-0.5, labels=MPs, las=2, cex.axis=0.5 )
    axis( side=2, at=1:Y-0.5, labels=rev(facs), las=1 )

  }

}

fitGLM <- function( par="Br30", stock="both", MP="MP_msyCap",
                    interact=FALSE, mseStatsFile="mseStats.Rdata" )
{
  d <- Design[[3]]

  # Stock-recruitment and Maturity/M - nominal (no modification needed)
  # West stock migration - ordinal
  d[ ,3] <- as.ordered( d[ ,3] )
  # Mean SSB - ordinal
  d[ ,4] <- as.ordered( d[ ,4] )
  # Length comp weight - ordinal
  d[ ,5] <- as.ordered( d[ ,5] )

  if(is.null(mseStatsFile))
    load("mseStats.Rdata")
  else
    load(mseStatsFile)

  e <- mseStats[["east"]][ ,MP,par] - 1
  w <- mseStats[["west"]][ ,MP,par] - 1

  ed <- cbind( e, d )
  wd <- cbind( w, d )

  colnames( ed ) <- c("y","SR","M","Mix","SSB","LC")
  colnames( wd ) <- c("y","SR","M","Mix","SSB","LC")

  if( stock=="both" )
    z <- rbind( ed, wd )
  else if( stock=="east" )
    z <- ed
  else if( stock=="west" )
    z <- wd

  if( interact )
    fit <- glm( y~(SR+M+Mix+SSB+LC)^2, data=z )
  else
    fit <- glm( y~SR+M+Mix+SSB+LC, data=z )
  
  return(fit)

}

scale <- function( x, lb=-1, ub=1 )
  lb + (ub-lb)*(x-min(x))/(max(x)-min(x))