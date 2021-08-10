# Alternative performance metrics requested by Pew

# PGK likely gives us one of the Pew metrics, but
# the remainder either require integration over process
# errors (i.e., stochastic OMs) or over OM axes (so based
# on combining multiple MSE objects)


#' pH30
#' Probability of being in the green quadrant of the
#' Kobe plot after 30 years, i.e., B > Bmsy and F < Fmsy
#' @param MSE An object of class MSE
#' @param pp population (stock) index
#' @examples
#' source("pewPMs.R")
#' pH30(myMSE,pp=1)
pH30 <-  function( MSE, pp = 1)
{
  F_FMSY <- MSE@F_FMSY[,,pp,MSE@nyears+MPlag+30] < 1
  B_BMSY <- MSE@B_BMSY[,,pp,MSE@nyears+MPlag+30] > 1

  greenKobe <- F_FMSY & B_BMSY

  greenKobe
}
class(pH30) <- "PM"

tfHealthy_t <- function( MSE, pp = 1)
{
  F_FMSY <- MSE@F_FMSY[,,pp,MSE@nyears+MPlag+1:30] < 1
  B_BMSY <- MSE@B_BMSY[,,pp,MSE@nyears+MPlag+1:30] > 1

  greenKobe <- F_FMSY & B_BMSY  

  greenTF <- apply(X = greenKobe, FUN = prod, MARGIN = c(1,3) )

  greenTF
}

# #' Average Br (spawning biomass relative to dynamic SSBMSY) over projection years 11-30 (a performance metrics function of class PM)
# #'
# #' @param MSE An object of class MSE
# #' @return a matrix n Management procedures (MSE@nMP) rows and nsim (MSE@nsim) columns from \code{MSE}
# #' @examples
# #' loadABT()
# #' AvgBr(myMSE2)
# AvgBr<-function(MSE,pp=1){

#   SSB<-MSE@SSB[,,pp,MSE@nyears+MPlag+(11:30)]
#   dynB0<-MSE@dynB0[,pp,MPlag+11:30]*MSE@SSBMSY_SSB0[,pp]
#   Brs<-array(NA,dim(SSB))
#   ind<-TEG(dim(SSB))
#   Brs[ind]<-SSB[ind]/dynB0[ind[,c(2,3)]]
#   apply(Brs,1:2,mean)

# }
# class(AvgBr)<-"PM"


