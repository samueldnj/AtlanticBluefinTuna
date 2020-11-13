writeOMSSB <- function( nI=96 )
{
  source("/Users/rossi/AtlanticBluefinTuna/MME_MP/tools.R")
  wSSB_it <- NULL
  eSSB_it <- NULL
  for( i in 1:nI )
  {
    z <- read.rep(paste("../OMs/",i,"/M3.rep",sep=""))
    eSSB_it <- rbind(eSSB_it,z$SSB[1:(nrow(z$SSB)/2),1])
    wSSB_it <- rbind(wSSB_it,z$SSB[(nrow(z$SSB)/2+1):nrow(z$SSB),1])
  }
  
  SSB <- list( e=eSSB_it*1e-6, w=wSSB_it*1e-6 )
  save( SSB, file="../OMs/SSB.Rdata" )
}

plotOMSSB <- function( cents=NULL, w=NULL, newPar=TRUE, nI=96, plotx=FALSE )
{
  load("../OMs/SSB.Rdata")

  yr <- OMI_1@years
  yrs <- yr[1]:yr[2]
  nT <- ncol( SSB$e )

  xi <- seq(1,nT,by=5)
  
  ylabs <- c("East SSB (kt)","West SSB (kt)")
  
  if( newPar )
    par( mfrow=c(1,2) )
  
  for( j in 1:2 )
  {
    plot( c(1,nT), c(0,1.1*max(SSB[[j]])), type="n", xlab="", las=1,
          ylab=ylabs[j], xaxs="i", yaxs="i", axes=newPar )
    if( !newPar )
    {
      axis( side=2, las=1 )
      box()
      if( plotx )
        axis( side=1, las=1, at=xi, labels=yrs[xi] )
    }
    for( i in 1:nI )
      lines( 1:nT, SSB[[j]][i, ], col=rgb(0,0,0,0.05) )
    if( !is.null(cents) )
    {
      K <- length(cents[[j]])
      cols <- suppressWarnings(brewer.pal(K,"Set1"))
      for( k in 1:K )
        lines( 1:nT, cents[[j]][[k]], col=cols[k], lwd=2 )
      legend( x="topright", bty="n", col=cols, lwd=2, cex=0.8,
              legend=paste("w =",round(w,2)) )
      par(font=2)
      legend( x="topleft", bty="n", legend=paste("k=",k,sep="") )
      par(font=1)
    }
  }
}

dtw2 <- function (x,y)
{
  X <- length(x)/2
  d1 <- dtw_basic(x[1:X],y[1:X])
  d2 <- dtw_basic(x[-(1:X)],y[-(1:X)])
  d1 + d2
}

clusterOMSSB <- function( k=3,
                          type="fuzzy",
                          newPar=TRUE,
                          plotx=FALSE,
                          plotFig=FALSE,
                          maxIter=1e5 )
{
  load("../OMs/SSB.Rdata")

  if( type=="fuzzy" )
    control <- fuzzy_control( iter.max = maxIter )
  else if( type=="partitional" )
    control <- partitional_control( iter.max = maxIter )

  nT <- ncol(SSB$e)
  x <- cbind( SSB$e,SSB$w )
  y <- tsclust( x, distance="dtw2", k=k, type=type,
                control=control )

  if( !y@converged )
    print(paste("Nonconvergence for k=",k,sep=""))

  # Centroids 
  cents <- y@centroids
  eCent <- list()
  wCent <- list()
  for( i in 1:k )
  {
    eCent[[i]] <- cents[[i]][1:nT]
    wCent[[i]] <- cents[[i]][-(1:nT)]
  }

  # Weights
  if( type=="fuzzy" )
    wtmp <- colSums(y@fcluster)
  else if( type=="partitional" )
    wtmp <- as.numeric(table(y@cluster))

  wts <- wtmp / sum(wtmp)

  # Reorder by weight
  wOrd <- order(-wts)
  wts <- wts[wOrd]
  eCent <- eCent[wOrd]
  wCent <- wCent[wOrd]

  out <- list()
  for( i in 1:k )
    out[[i]] <- rbind( eCent[[i]], wCent[[i]] )
  out[["wts"]] <- wts

  if( plotFig )
    plotOMSSB( cents=list(eCent,wCent), w=wts, newPar=newPar, plotx=plotx )

  out

}

clusterOMSSBold <- function( k=3, newPar=TRUE, plotFig=FALSE, plotx=FALSE )
{
  load("../OMs/SSB.Rdata")

  # Cluster
  ec <- tsclust( SSB$e, k=k, type="fuzzy" )
  wc <- tsclust( SSB$w, k=k, type="fuzzy" )

  # Weights
  ew <- colSums(ec@fcluster)/sum(ec@fcluster)
  ww <- colSums(wc@fcluster)/sum(wc@fcluster)

  # Centroids
  eCent <- ec@centroids
  wCent <- wc@centroids

  # Reorder by weight
  eord <- order(-ew)
  word <- order(-ww)
  ew <- ew[eord]
  ww <- ww[word]
  eCent <- eCent[eord]
  wCent <- wCent[word]

  out <- list()
  for( i in 1:k )
    out[[i]] <- rbind( eCent[[i]], wCent[[i]] )
  out[["wts"]] <- rbind( ew, ww )

  if( plotFig )
    plotOMSSB( cents=list(eCent,wCent), w=list(ew,ww), newPar=newPar, plotx=plotx )

  out

}

plotClusters <- function( k=2:5, type="fuzzy" )
{
  K <- length(k)
  par( mfrow=c(K,2), mar=c(1,2,0,1), oma=c(3,4,3,0) )
  for( i in k )
    clusterOMSSB(k=i,type=type,newPar=FALSE,plotFig=TRUE,plotx=i==k[K])
  mtext( side=1, text="Year", outer=TRUE, line=2, cex=1.5 )
  mtext( side=2, text="SSB (kt)", outer=TRUE, line=2, cex=1.5 )
  mtext( side=3, outer=TRUE, line=0, cex=1.5,
         text="  East stock                          West stock" )
}




























