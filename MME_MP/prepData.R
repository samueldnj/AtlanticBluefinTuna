# --------------------------------------------------------------------------
# PrepData.R
# 
# Prepares data for use in the DD multi-model MP for the ABTMSE 
#
# Author: Samuel D N Johnson
# Date: May 30, 2019
#
# --------------------------------------------------------------------------


getIndicesOM <- function(i,nT=52)
{
  z <- read.rep(paste("../OMs/",i,"/M3.rep",sep=""))
  T <- z$"ny,"
  hT <- z$"nHy,"
  omSSB_st <- array( data=NA, dim=c(2,hT+T) )
  omSSB_st[1,1:hT] <- z$hSSB[1:hT,1]
  omSSB_st[2,1:hT] <- z$hSSB[-(1:hT),1]
  omSSB_st[1,-(1:hT)] <- z$SSB[1:T,1]
  omSSB_st[2,-(1:hT)] <- z$SSB[-(1:T),1]
  browser()
  I_gt <- matrix(data=NA,nrow=2,ncol=nT,
                 dimnames=list(c("EAST_OM_SSB","WEST_OM_SSB"),NULL))
  I_gt[1, ] <- omSSB_st[1,-(1:(T+hT-nT))]
  I_gt[2, ] <- omSSB_st[2,-(1:(T+hT-nT))]
  I_gt*1e-6
}


#indicesOM <- lapply( X = 1:36, FUN = getIndicesOM )

#save(indicesOM, file = "indicesOM.RData")

