compareBiasBr30 <- function(folder="newClustAug28-2",j=1)
{
  load("../OMs/clust.Rdata")
  OMs <- clust[["6"]]$OMs
  cols <- rep("black",96)
  cols[OMs] <- "red"
  relErr_is <- array( data=0, dim=c(96,2) )
  Br30_is <- array( data=0, dim=c(96,2) )
  for( i in 1:96 )
  {
    load(paste("MSEs/",folder,"/fitCheck/OM_",i,"d/checkTables.Rdata",sep=""))
    load(paste("MSEs/",folder,"/MSE_OM_",i,"d.Rdata",sep=""))
    mse <- get(paste("MSE_OM_",i,"d",sep=""))
    east <- ewCheckTableList[["east"]]
    west <- ewCheckTableList[["west"]]
    eRelErr <- NULL
    wRelErr <- NULL

    SSB_st <- mse@SSB[2,j, , ]/1e6

    eas <- filter( east[[j]], deltaNLL==0 )
    wes <- filter( west[[j]], deltaNLL==0 )

    relErr_is[i,1] <- mean((eas$Bnext_E-SSB_st[1,eas$nT]) / SSB_st[1,eas$nT])
    relErr_is[i,2] <- mean((wes$Bnext_W-SSB_st[2,wes$nT]) / SSB_st[2,wes$nT])

    Br30_is[i,1] <- getperf(mse)$East[2,"Br30"]
    Br30_is[i,2] <- getperf(mse)$West[2,"Br30"]

  }
  par( mfrow=c(1,2), mar=c(0,0,3,0.5), oma=c(4,4,1,3) )
  for( s in 2:1 )
  {
    x <- relErr_is[ ,s]
    y <- Br30_is[ ,s]
    plot( x=x, y=y, type="n", axes=FALSE,
          xlab="", ylab="", main=c("East","West")[s] )
    abline( h=1, col="grey", lty=2 )
    abline( v=0, col="grey", lty=2 )
    box()
    abline(lm(y~x),col="red3")
    text( x=x, y=y, labels=1:96, cex=0.7, col=cols )
    axis( side=1 )
    if( s==1 )
      axis( side=4, las=1 )
    else
      axis( side=2, las=1 )
  }
  mtext( side=1, text="Relative error in biomass", outer=TRUE, line=3 )
  mtext( side=2, text="Br30", outer=TRUE, line=3 )
}

calcProjBias <- function(folder="newClustAug28-2")
{
  load("../OMs/clust.Rdata")
  OMs <- clust[["5"]]$OMs
  cols <- rep("grey85",96)
  col2 <- rep("black",96)
  cols[OMs] <- "red"
  col2[OMs] <- "red3"
  relErr_ijs <- array( data=0, dim=c(96,2,2) )
  for( i in 1:96 )
  {
    load(paste("MSEs/",folder,"/fitCheck/OM_",i,"d/checkTables.Rdata",sep=""))
    load(paste("MSEs/",folder,"/MSE_OM_",i,"d.Rdata",sep=""))
    mse <- get(paste("MSE_OM_",i,"d",sep=""))
    east <- ewCheckTableList[["east"]]
    west <- ewCheckTableList[["west"]]
    eRelErr <- NULL
    wRelErr <- NULL
    for( j in 1:2 )
    {
      SSB_st <- mse@SSB[2,j, , ]/1e6

      eas <- filter( east[[j]], deltaNLL==0 )
      wes <- filter( west[[j]], deltaNLL==0 )

      relErr_ijs[i,j,1] <- mean((eas$Bnext_E-SSB_st[1,eas$nT]) / SSB_st[1,eas$nT])
      relErr_ijs[i,j,2] <- mean((wes$Bnext_W-SSB_st[2,wes$nT]) / SSB_st[2,wes$nT])
    }
  }
  par( mfrow=c(1,2), mar=c(0,0,3,0.5), oma=c(4,4,1,3) )
  i <- seq( 1,96, by=5 )
  for( s in 2:1 )
  {
    plot( x=range( relErr_ijs[ , ,s] )*1.02, y=c(0,97), type="n", axes=FALSE,
          xlab="", ylab="", main=c("East","West")[s] )
    axis( side=1 )
    if( s==1 )
      axis( side=4, las=1, at=(1:96)[i], labels=(96:1)[i] )
    else
      axis( side=2, las=1, at=(1:96)[i], labels=(96:1)[i] )
    abline( h=96:1, col=cols )
    abline( v=0, lty=2 )
    points( x=rowMeans(relErr_ijs[ ,,s]), y=96:1, lwd=2, col=col2, cex=0.8 )
    box()
  }
  mtext( side=1, text="Relative error in biomass", outer=TRUE, line=3 )
  mtext( side=2, text="Operating Model", outer=TRUE, line=3 )
}