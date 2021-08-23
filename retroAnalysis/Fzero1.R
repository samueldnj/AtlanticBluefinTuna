## An alternate version of the Fzero1 MP that does OK. No constraint 
## on TAC change. Tune using q

#####################
# required packages #
if(!require(fishmethods)){
  install.packages('fishmethods')
}
library(fishmethods)

 
##################
# F01 based MP
##################
Fzero1W <- function(  x, 
                      dset, 
                      yrsmth = 3, 
                      lim = c(.1,.4,1), 
                      alp = c(0.75, 0.6, 0.5),
                      ny = NULL,
                      IndexID_y = 13, 
                      IndexID_m = 12, 
                      IndexID_o = 14, 
                      nyears=55,
                      q = 0.608, # from 2015 VPA continuity run
                      IndexID_bio = 14)
{
    
  # required functions
  # Range Normalization
  RangeNorm = function(x,maxN=1,minN=0){
    maxD = max(x, na.rm=T)
    minD = min(x, na.rm=T)
    (( (x-minD)*(maxN-minN))/(maxD-minD)) + minN
  }
 
  # YPR where
  # wgt is the weight at age in 2015 from west VPA
  # M is scaled to the Lorenzen function SCRS/2017/176
  # age is as defined in the 2015 West VPA
  # partial is calculated from indicators
 
  BFT_yprW = function(Small=1,Med=1,Large=1){
    wgt=c(3.1,    9.8,        15.1,     19.9,     43.3,     60.5,     89.9,     111.6,   144.8,              174,       201.1,   225.5,
          247.7,      264,       283.5,   340)
    M = c(0.40, 0.33, 0.27, 0.23, 0.20, 0.18, 0.16, 0.14, 0.13, 0.12, 0.12, 0.11,
          0.11, 0.11, 0.11, 0.11)
    age = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
    partial = c(rep(Small,4), rep(Med,2),rep(Large,10))
    if(sum(is.na(c(Small,Med,Large)))==0) {
      ypr(age=age, wgt = wgt, partial = partial, M = M, plus=TRUE,
          oldest = 16, maxF = 10, incrF=0.01, graph=F)$Reference_Points[1,1]
    } else {0}
  }
  #####################
 
  # index for F01 calculation
  if(is.null(ny))
    ny <- length(dset$Cobs[x, ])   # number of years of data
  
  ind_a <- (ny - (yrsmth - 1)):ny    # subset of years used in YPR calc
 
  # Range normalization of indicators indexing young, medium and old fish
  # Keeping mean of most recent 3 years
  Small = mean(RangeNorm(dset$Iobs[x,IndexID_y,])[ind_a], na.rm=T)
  Med = mean(RangeNorm(dset$Iobs[x,IndexID_m,])[ind_a], na.rm=T)
  Large = mean(RangeNorm(dset$Iobs[x,IndexID_o,])[ind_a], na.rm=T)
  Total = Small + Med + Large
 
  # Fraction at age
  fSmall = (Small/Total)
  fMed = (Med/Total)
  fLarge = (Large/Total)
 
  # Calculate F01
  if(!is.na(Total)==T){
    F01 = BFT_yprW(Small=fSmall,Med=fMed,Large=fLarge)
  }else{F01 = .2}
 
  # New TAC is F01*I/q
  
  TAC =F01*(dset$Iobs[x,IndexID_bio,][ny]/q/1e-6)
 
  TAC

}
 
class(Fzero1W) = 'MP'
environment(Fzero1W) <- asNamespace('ABTMSE')
 
#$#$#
 
Fzero1E <- function(  x, 
                      dset, 
                      yrsmth = 3, 
                      lim = c(.1,.4,1), 
                      ny = NULL,
                      alp = c(0.75, 0.6, 0.5),
                      IndexID_y = 1, 
                      IndexID_m = 12, 
                      IndexID_o = 2, 
                      nyears=55,
                      q = 0.25,
                      IndexID_bio = 2)
{
  
  # required functions
  # Range Normalization
  RangeNorm = function(x,maxN=1,minN=0){
    maxD = max(x, na.rm=T)
    minD = min(x, na.rm=T)
    (( (x-minD)*(maxN-minN))/(maxD-minD)) + minN
  }
 
  # YPR where
  # wgt is the weight at age in 2017 from east VPA
  # M is scaled to the Lorenzen function SCRS/2017/176
  # age is as defined in the 2015 East VPA
  # partial is calculated from indicators
 
  BFT_yprE = function(Small=1,Med=1,Large=1){
    wgt=c(3.,   10.,   19.,   35.,   50.,   69.,   90.,  113.,  138.,  205.)
    M = c(0.40, 0.33, 0.27, 0.23, 0.20, 0.18, 0.16, 0.14, 0.13, 0.12)
    age = c(1,2,3,4,5,6,7,8,9,10)
    partial = c(rep(Small,4), rep(Med,2),rep(Large,4))
    if(sum(is.na(c(Small,Med,Large)))==0) {
      ypr(age=age, wgt = wgt, partial = partial, M = M, plus=TRUE,
          oldest = 10, maxF = 10, incrF=0.01, graph=F)$Reference_Points[1,1]
    } else {0}
  }
  #####################
 
  # index for F01 calculation
  if(is.null(ny))
    ny <- length(dset$Cobs[x, ])   # number of years of data
  
  ind_a <- (ny - (yrsmth - 1)):ny    # subset of years used in YPR calc
 
  # Range normalization of indicators indexing young, medium and old fish
  # Keeping mean of most recent 3 years
  Small = mean(RangeNorm(dset$Iobs[x,IndexID_y,])[ind_a], na.rm=T)
  Med = mean(RangeNorm(dset$Iobs[x,IndexID_m,])[ind_a], na.rm=T)
  Large = mean(RangeNorm(dset$Iobs[x,IndexID_o,])[ind_a], na.rm=T)
  Total = Small + Med + Large
 
  # Fraction at age
  fSmall = (Small/Total)
  fMed = (Med/Total)
  fLarge = (Large/Total)
 
  # Calculate F01
  if(!is.na(Total)==T){
    F01 = BFT_yprE(Small=fSmall,Med=fMed,Large=fLarge)
  }else{F01 = .2}
 
  # New TAC is F01*I/q
   # from 2015 VPA continuity run
  TAC =F01*(dset$Iobs[x,IndexID_bio,][ny]/q/1e-6)
 
  TAC
}

class(Fzero1E) = 'MP'
environment(Fzero1E) <- asNamespace('ABTMSE')