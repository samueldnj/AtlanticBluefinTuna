# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3star) tuned to 1.5
# indices modified 
# with cap in the west
BR_E3s<-function( x, 
                  dset, 
                  lastyr = NULL,
                  alp=1.69,
                  TACcap = 45,
                  phaseTime = 0,
                  initPhz = 56,
                  pullD = FALSE )
{  
  if(is.null(lastyr))
    lastyr <- dim(dset$Iobs)[3]                # Most recent year

  cury <- dim(dset$TAC)[2] 
  #print(lastyr)
  
  if(pullD & lastyr < 58 & x == 1)
  {
    saveRDS(dset, file = "dsetE.rds")
  }

  if(lastyr < dim(dset$Iobs)[3])
  {
    lastTAC <- dset$Cobs[x,lastyr-1]
    currTAC <- dset$Cobs[x,lastyr]
  } else {
    lastTAC <- dset$MPrec[x]
    currTAC <- dset$curTAC[x]
  }
  

  # 
  relInd1 = dset$Iobs[x,1,]/mean(dset$Iobs[x,1,50:53],na.rm=T)	# FR_AER_SUV2 (!use 2014-2017 for the average)
  relInd2 = dset$Iobs[x,2,]/mean(dset$Iobs[x,2,48:52],na.rm=T)	# MED_LAR_SUV (!use 2012-2016 for the average)
  relInd3 = dset$Iobs[x,4,]/mean(dset$Iobs[x,4,51:54],na.rm=T)	# GBYP_AER_SUV_BAR (!use 2015-2018 for the average)
  relInd4 = dset$Iobs[x,5,]/mean(dset$Iobs[x,5,48:54],na.rm=T)	# MOR_POR_TRAP (!use 2012-2018 for the average)
  relInd5 = dset$Iobs[x,6,]/mean(dset$Iobs[x,6,48:55],na.rm=T)	# JPN_LL_NEAtl2 (!use 2012-2019 for the average)
  
  # Relative index weights
  Iw1<-1.0  #1.0
  Iw2<-8.96 #3.16
  Iw3<-0    #3.16
  Iw4<-3.16 #3.16
  Iw5<-4.94 #4.94
  
  Ind_E2017<-(Iw1*relInd1[53]+Iw2*relInd2[53]+Iw3*relInd3[53]+Iw4*relInd4[53]+Iw5*relInd5[53])/(Iw1+Iw2+Iw3+Iw4+Iw5)			
  Ind_E<-vector()
  
  relInd_it <- array( NA, dim = c(5,length(relInd4)))
  relInd_it[1,] <- relInd1 * Iw1
  relInd_it[2,] <- relInd2 * Iw2
  relInd_it[3,] <- relInd3 * Iw3
  relInd_it[4,] <- relInd4 * Iw4
  relInd_it[5,] <- relInd5 * Iw5

  wts_i <- c(Iw1,Iw2,Iw3,Iw4,Iw5)

  if(lastyr<56){
    
    Ind_E_unnorm <- apply(X = relInd_it, FUN = sum, MARGIN = 2, na.rm = T)


    wtIdx_lyrm4 <- which(!is.na(relInd_it[,lastyr-4]))
    wtIdx_lyrm3 <- which(!is.na(relInd_it[,lastyr-3]))
    wtIdx_lyrm2 <- which(!is.na(relInd_it[,lastyr-2]))

    Ind_E[lastyr-4] <- Ind_E_unnorm[lastyr-4]/sum(wts_i[wtIdx_lyrm4])
    Ind_E[lastyr-3] <- Ind_E_unnorm[lastyr-3]/sum(wts_i[wtIdx_lyrm3])
    Ind_E[lastyr-2] <- Ind_E_unnorm[lastyr-2]/sum(wts_i[wtIdx_lyrm2])
  }

  if(lastyr==56){
    Ind_E[lastyr-4]<-(Iw1*relInd1[lastyr-4]+Iw2*relInd2[lastyr-4]+                      Iw4*relInd4[lastyr-4]+Iw5*relInd5[lastyr-4])/(Iw1+Iw2+    Iw4+Iw5)
    Ind_E[lastyr-3]<-(Iw1*relInd1[lastyr-3]+Iw2*relInd2[lastyr-3]+Iw3*relInd3[lastyr-3]+Iw4*relInd4[lastyr-3]+Iw5*relInd5[lastyr-3])/(Iw1+Iw2+Iw3+Iw4+Iw5)
    Ind_E[lastyr-2]<-(Iw1*relInd1[lastyr-2]+                      Iw3*relInd3[lastyr-2]+Iw4*relInd4[lastyr-2]+Iw5*relInd5[lastyr-2])/(Iw1+    Iw3+Iw4+Iw5)
  }
  if(lastyr==57){
    Ind_E[lastyr-4]<-(Iw1*relInd1[lastyr-4]+Iw2*relInd2[lastyr-4]+Iw3*relInd3[lastyr-4]+Iw4*relInd4[lastyr-4]+Iw5*relInd5[lastyr-4])/(Iw1+Iw2+Iw3+Iw4+Iw5)
    Ind_E[lastyr-3]<-(Iw1*relInd1[lastyr-3]+                      Iw3*relInd3[lastyr-3]+Iw4*relInd4[lastyr-3]+Iw5*relInd5[lastyr-3])/(Iw1+    Iw3+Iw4+Iw5)
    Ind_E[lastyr-2]<-(Iw1*relInd1[lastyr-2]+Iw2*relInd2[lastyr-2]+                     Iw4*relInd4[lastyr-2]+Iw5*relInd5[lastyr-2])/(Iw1+Iw2+     Iw4+Iw5)
  }
  if(lastyr==58){
    Ind_E[lastyr-4]<-(Iw1*relInd1[lastyr-4]+                      Iw3*relInd3[lastyr-4]+Iw4*relInd4[lastyr-4]+Iw5*relInd5[lastyr-4])/(Iw1+   Iw3+Iw4+Iw5)
    Ind_E[lastyr-3]<-(Iw1*relInd1[lastyr-3]+Iw2*relInd2[lastyr-3]+                      Iw4*relInd4[lastyr-3]+Iw5*relInd5[lastyr-3])/(Iw1+Iw2+   Iw4+Iw5)
    Ind_E[lastyr-2]<-(Iw1*relInd1[lastyr-2]+                                            Iw4*relInd4[lastyr-2]                      )/(Iw1+       Iw4    )
  }
  if(lastyr==59){
    Ind_E[lastyr-4]<-(Iw1*relInd1[lastyr-4]+Iw2*relInd2[lastyr-4]+                      Iw4*relInd4[lastyr-4]+Iw5*relInd5[lastyr-4])/(Iw1+Iw2+   Iw4+Iw5)
    Ind_E[lastyr-3]<-(Iw1*relInd1[lastyr-3]+                                            Iw4*relInd4[lastyr-3]                      )/(Iw1+       Iw4    )
    Ind_E[lastyr-2]<-(Iw1*relInd1[lastyr-2]+Iw2*relInd2[lastyr-2]+Iw3*relInd3[lastyr-2]+Iw4*relInd4[lastyr-2]+Iw5*relInd5[lastyr-2])/(Iw1+Iw2+Iw3+Iw4+Iw5)
  }
  if(lastyr==60){
    Ind_E[lastyr-4]<-(Iw1*relInd1[lastyr-4]+                                            Iw4*relInd4[lastyr-4]                      )/(Iw1+       Iw4    )
    Ind_E[lastyr-3]<-(Iw1*relInd1[lastyr-3]+Iw2*relInd2[lastyr-3]+Iw3*relInd3[lastyr-3]+Iw4*relInd4[lastyr-3]+Iw5*relInd5[lastyr-3])/(Iw1+Iw2+Iw3+Iw4+Iw5)
    Ind_E[lastyr-2]<-(Iw1*relInd1[lastyr-2]+Iw2*relInd2[lastyr-2]+Iw3*relInd3[lastyr-2]+Iw4*relInd4[lastyr-2]+Iw5*relInd5[lastyr-2])/(Iw1+Iw2+Iw3+Iw4+Iw5)
  }
  if(lastyr>=61){
    Ind_E[lastyr-4]<-(Iw1*relInd1[lastyr-4]+Iw2*relInd2[lastyr-4]+Iw3*relInd3[lastyr-4]+Iw4*relInd4[lastyr-4]+Iw5*relInd5[lastyr-4])/(Iw1+Iw2+Iw3+Iw4+Iw5)
    Ind_E[lastyr-3]<-(Iw1*relInd1[lastyr-3]+Iw2*relInd2[lastyr-3]+Iw3*relInd3[lastyr-3]+Iw4*relInd4[lastyr-3]+Iw5*relInd5[lastyr-3])/(Iw1+Iw2+Iw3+Iw4+Iw5)
    Ind_E[lastyr-2]<-(Iw1*relInd1[lastyr-2]+Iw2*relInd2[lastyr-2]+Iw3*relInd3[lastyr-2]+Iw4*relInd4[lastyr-2]+Iw5*relInd5[lastyr-2])/(Iw1+Iw2+Iw3+Iw4+Iw5)
  }
  
  aveInd_E<-matrix(data=NA,nrow=200,ncol=200)
  aveInd_E[x,cury-2]=mean(Ind_E[(lastyr-4):(lastyr-2)],na.rm=T)
  # cat(sprintf("x%d year%d %s %.3f %s %.3f %.3f \n",x,lastyr,"oldTAC ",dset$curTAC[x], "Ind2017 ",Ind_E2017,aveInd_E[x,cury-2]),file="TACE.txt",append=TRUE)
  
  #print(lastyr)
  
  # setting the TAC 
  TAC_E=(currTAC/Ind_E2017)*alp*aveInd_E[x,cury-2]  
  
  # quadratic decrease below TvalE
  TvalE=1.0
  
  if(aveInd_E[x,cury-2]>=TvalE){
    adjE=1
  }
  else if(aveInd_E[x,cury-2]<TvalE){
    adjE=aveInd_E[x,cury-2]/TvalE
  }
  else{
    adjE=1
  }
  TAC_E=adjE*TAC_E
  
  # constraints on TAC max change: maxUp, maxDown  
  maxUp<-0.2
  maxDown<-0.2

  if (aveInd_E[x,cury-2]<Ind_E2017) {
    maxDown = (0.4-0.2*aveInd_E[x,cury-2]/Ind_E2017)
  }
  if (maxDown>0.3){
    maxDown=0.3
  }  
  # cat(sprintf("x%d year%d %s %.3f %s %.3f %.3f \n",x,lastyr,"Beforen ",TAC_E, "Ind2017 ",Ind_E2017,aveInd_E[x,cury-2]),file="maxdown.txt",append=TRUE)
  
  if(TAC_E>(lastTAC*(1+maxUp)) ){
    TAC_E=lastTAC*(1+maxUp)
  }
  if(TAC_E<(lastTAC*(1-maxDown)) ){
    TAC_E=lastTAC*(1-maxDown)
  }
  
  #cat(sprintf("x%d year%d %s %.3f %s %.3f %.3f \n",x,lastyr,"Catch ",TAC_E, "Ind2017 ",Ind_E2017,aveInd_E[x,cury-2]),file="East.txt",append=TRUE)
  
  if(TAC_E>TACcap*1e6){  
    TAC_E=TACcap*1e6
  }
  
  if(lastyr<=66){
    if(TAC_E>0.8*TACcap*1e6 ){
      TAC_E=0.8*TACcap*1e6
    }
    if(TAC_E<12000000 ){
      TAC_E=12000000
    }
  }

  # Phase-in
  if(phaseTime > 0 & (lastyr <= initPhz + phaseTime) )
  {
    sqTAC <- dset$Cobs[x,initPhz]

    tNow <- lastyr - initPhz 
    # phase-in TAC
    TAC_E <- tNow/phaseTime * TAC_E + (phaseTime - tNow)/phaseTime * sqTAC
  }
  
  #if(lastyr==104){
  #  cat(sprintf("x%d year%d %s %.3f %s %.3f %.3f \n",x,lastyr,"Before ",TAC_E, "Ind2017 ",relInd1,relInd2),file="I1to3.txt",append=TRUE)
  #  cat(sprintf("x%d year%d %s %.3f %s %.3f %.3f \n",x,lastyr,"Before ",relInd3, "Ind2017 ",relInd4,relInd5),file="I4to5.txt",append=TRUE)
  #}
  # cat(sprintf("x%d year%d %.3f  \n",x,lastyr,TAC_E),file="ET.txt",append=TRUE)
  
  print(lastyr)
  print(TAC_E)

  return(TAC_E)
}
class(BR_E3s)<-"MP"
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3star) tuned to 1.25
# indices modified 
# with cap in the west
BR_W3s<-function( x,
                  dset,
                  lastyr = NULL,
                  bet=0.95,
                  gam=10.0,
                  TACcap = 2.35,
                  phaseTime = 0,
                  initPhz = 56,
                  pullD = FALSE)
{
  
  if(is.null(lastyr))
    lastyr <- dim(dset$Iobs)[3]                # Most recent year

  cury <- dim(dset$TAC)[2] 
  #print(cury)

  if(pullD & lastyr < 58 & x == 1)
  {
    # browser()
    saveRDS(dset, file = "dsetW.rds" )
  }

  if(lastyr < dim(dset$Iobs)[3])
  {
    lastTAC <- dset$Cobs[x,lastyr-1]
    currTAC <- dset$Cobs[x,lastyr]
  } else {
    lastTAC <- dset$MPrec[x]
    currTAC <- dset$curTAC[x]
  }
  
  # !! At the moment Iobs for year 54 and 55 doesn't have the real data
  relInd6 = dset$Iobs[x,3,]/mean(dset$Iobs[x,3,42:53],na.rm=T)	# GOM_LAR_SUV (!use 42-53 for the average)
  relInd7 = dset$Iobs[x,7,]/mean(dset$Iobs[x,13,42:54],na.rm=T)	# US_RR_66_144 (!use 42-54 for the average)
  relInd8 = dset$Iobs[x,8,]/mean(dset$Iobs[x,8,42:54],na.rm=T)	# US_RR_115_144 (!use 42-54 for the average)
  relInd9 = dset$Iobs[x,9,]/mean(dset$Iobs[x,9,42:54],na.rm=T)	# US_RR_177 (!use 42-54 for the average)
  relInd10 = dset$Iobs[x,14,]/mean(dset$Iobs[x,14,42:54],na.rm=T)	# MEXUS_GOM_PLL (!use 42-54 for the average)
  relInd11 = dset$Iobs[x,10,]/mean(dset$Iobs[x,10,46:55],na.rm=T)	# JPN_LL_West2 (!use 46-55 for the average)
  relInd12 = dset$Iobs[x,12,]/mean(dset$Iobs[x,12,42:53],na.rm=T)	# CAN_SWNS (!use 42-53 for the average)
  
  # Relative index weights
  Iw6<-3.64   #2.97
  Iw7<-0.95   #1.39 (3x)
  Iw8<-0      #5.91 (3x)
  Iw9<-0.10   #0.60    
  Iw10<-4.49  #0.60
  Iw11<-5.83  #2.60    
  Iw12<-0.31  #0.34

  relInd_it <- array( NA, dim = c(7,length(relInd6)))
  relInd_it[1,] <- relInd6 * Iw6
  relInd_it[2,] <- relInd7 * Iw7
  relInd_it[3,] <- relInd8 * Iw8
  relInd_it[4,] <- relInd9 * Iw9
  relInd_it[5,] <- relInd10 * Iw10
  relInd_it[6,] <- relInd11 * Iw11
  relInd_it[7,] <- relInd12 * Iw12

  wts_i <- c(Iw6,Iw7,Iw8,Iw9,Iw10,Iw11,Iw12)

  Ind_W2017<-(Iw6*relInd6[53]+Iw7*relInd7[53]+Iw8*relInd8[53]+Iw9*relInd9[53]+Iw10*relInd10[53]+Iw11*relInd11[53]+Iw12*relInd12[53])/(Iw6+Iw7+Iw8+Iw9+Iw10+Iw11+Iw12)     # MED_LAR_SUV not available for 2017
  Ind_W<-vector()
  
  if( lastyr >= 56 )
  {
    Ind_W[lastyr-6]<-(Iw6*relInd6[lastyr-6]+Iw7*relInd7[lastyr-6]+Iw8*relInd8[lastyr-6]+Iw9*relInd9[lastyr-6]+Iw10*relInd10[lastyr-6]+Iw11*relInd11[lastyr-6]+Iw12*relInd12[lastyr-6])/(Iw6+Iw7+Iw8+Iw9+Iw10+Iw11+Iw12)
    Ind_W[lastyr-5]<-(Iw6*relInd6[lastyr-5]+Iw7*relInd7[lastyr-5]+Iw8*relInd8[lastyr-5]+Iw9*relInd9[lastyr-5]+Iw10*relInd10[lastyr-5]+Iw11*relInd11[lastyr-5]+Iw12*relInd12[lastyr-5])/(Iw6+Iw7+Iw8+Iw9+Iw10+Iw11+Iw12)
    Ind_W[lastyr-4]<-(Iw6*relInd6[lastyr-4]+Iw7*relInd7[lastyr-4]+Iw8*relInd8[lastyr-4]+Iw9*relInd9[lastyr-4]+Iw10*relInd10[lastyr-4]+Iw11*relInd11[lastyr-4]+Iw12*relInd12[lastyr-4])/(Iw6+Iw7+Iw8+Iw9+Iw10+Iw11+Iw12)
    Ind_W[lastyr-3]<-(Iw6*relInd6[lastyr-3]+Iw7*relInd7[lastyr-3]+Iw8*relInd8[lastyr-3]+Iw9*relInd9[lastyr-3]+Iw10*relInd10[lastyr-3]+Iw11*relInd11[lastyr-3]+Iw12*relInd12[lastyr-3])/(Iw6+Iw7+Iw8+Iw9+Iw10+Iw11+Iw12)
    Ind_W[lastyr-2]<-(Iw6*relInd6[lastyr-2]+Iw7*relInd7[lastyr-2]+Iw8*relInd8[lastyr-2]+Iw9*relInd9[lastyr-2]+Iw10*relInd10[lastyr-2]+Iw11*relInd11[lastyr-2]                        )/(Iw6+Iw7+Iw8+Iw9+Iw10+Iw11     )
  }
    
  

  if(lastyr<56){
    
    Ind_W_unnorm <- apply(X = relInd_it, FUN = sum, MARGIN = 2, na.rm = T)

  
    wtIdx_lyrm6 <- which(!is.na(relInd_it[,lastyr-6]))
    wtIdx_lyrm5 <- which(!is.na(relInd_it[,lastyr-5]))
    wtIdx_lyrm4 <- which(!is.na(relInd_it[,lastyr-4]))
    wtIdx_lyrm3 <- which(!is.na(relInd_it[,lastyr-3]))
    wtIdx_lyrm2 <- which(!is.na(relInd_it[,lastyr-2]))[-7]

    Ind_W[lastyr-6] <- Ind_W_unnorm[lastyr-6]/sum(wts_i[wtIdx_lyrm6])
    Ind_W[lastyr-5] <- Ind_W_unnorm[lastyr-5]/sum(wts_i[wtIdx_lyrm5])
    Ind_W[lastyr-4] <- Ind_W_unnorm[lastyr-4]/sum(wts_i[wtIdx_lyrm4])
    Ind_W[lastyr-3] <- Ind_W_unnorm[lastyr-3]/sum(wts_i[wtIdx_lyrm3])
    Ind_W[lastyr-2] <- Ind_W_unnorm[lastyr-2]/sum(wts_i[wtIdx_lyrm2])
  }
  
  
  aveInd_W<-matrix(data=NA,nrow=200,ncol=200)
  aveInd_W[x,cury-2]=mean(Ind_W[(lastyr-4):(lastyr-2)],na.rm=T)
  #cat(sprintf("x%d year%d %s %.3f %s %.3f %.3f %.3f\n",x,lastyr, "av", aveInd_W[x,cury-2],"Ind ",Ind_W[lastyr-4],Ind_W[lastyr-3],Ind_W[lastyr-2]),file="tryW.txt",append=TRUE)
  #cat(sprintf("%d\t%f\n%d\t%f\n",x,Ind_W2[lastyr-2],x,Ind_W2[lastyr-3]),file="jwvalue2.txt",append=TRUE)
  #cat(sprintf("%d\t%f\n%d\t%f\n",x,Ind_W[lastyr-2],x,Ind_W[lastyr-3]),file="jwvalue.txt",append=TRUE)
  #cat(sprintf("%d\t%f\n",x,aveInd_W), file="avejwvalue.txt",append=TRUE)
  
  # Setting the TAC
  TAC_W=(currTAC/Ind_W2017)*bet*aveInd_W[x,cury-2]
  
  # quadratic decrease below Tval
  Tval=1.0
  if(aveInd_W[x,cury-2]>=Tval){
    adj=1
  }
  else if(aveInd_W[x,cury-2]<Tval){
    adj=aveInd_W[x,cury-2]/Tval
  }
  else{
    adj=1
  }
  TAC_W=adj*TAC_W
  
  # Slope component
  itmp<-log(Ind_W[(lastyr-6):(lastyr-2)])
  ytmp<-c(lastyr-6,lastyr-5,lastyr-4,lastyr-3,lastyr-2)
  slppar<-summary(lm(itmp~ytmp))$coefficients[2,1:2]
  s_W <-slppar[1]
  s_threshold=0
  if(s_W>=s_threshold){
    adj2=1.
  }
  else if(s_W<s_threshold){
    adj2=(1. + gam*(s_W-s_threshold))
  }
  TAC_W=adj2*TAC_W
  #cat(sprintf("%d %f year%d %f\n",x,s_W,cury,adj2),file="jwvalue.txt",append=TRUE)
  #cat(sprintf("x%d year%d %s %.3f %s %.3f %.3f %.3f %.3f %.3f\n",x,lastyr, "av", aveInd_W[x,cury-2],"Ind ",Ind_W[lastyr-6],Ind_W[lastyr-5],Ind_W[lastyr-4],Ind_W[lastyr-3],Ind_W[lastyr-2]),file="tryW.txt",append=TRUE)
  
  
  # constraints on TAC max change: maxWUp, maxWDown  
  maxWUp<-0.2
  maxWDown<-0.2
  if (aveInd_W[x,cury-2]<Ind_W2017) {
    maxWDown <- (0.4-0.2*aveInd_W[x,cury-2]/Ind_W2017)
  }
  if (maxWDown>0.3){
    maxWDown<-0.3
  }  
  
  if(TAC_W>(lastTAC*(1+maxWUp)) ){
    TAC_W=lastTAC*(1+maxWUp)
  }
  if(TAC_W<(lastTAC*(1-maxWDown)) ){
    TAC_W=lastTAC*(1-maxWDown)
  }
  
  #if(TAC_W>4000000){
  #  TAC_W=4000000}
  #else{
  #  TAC_W=TAC_W
  #}  
  if(lastyr<=66){
    if(TAC_W>TACcap*1e6 ){
      TAC_W=TACcap*1e6
    }
  }

    # Phase-in
  if(phaseTime > 0 & (lastyr <= initPhz + phaseTime) )
  {
    sqTAC <- dset$Cobs[x,initPhz]

    tNow <- lastyr - initPhz 
    # phase-in TAC
    TAC_W <- tNow/phaseTime * TAC_W + (phaseTime - tNow)/phaseTime * sqTAC
  }
  
  cat(sprintf("x%d year%d %.3f  \n",x,lastyr,TAC_W),file="WT.txt",append=TRUE)
  
  print(TAC_W)

  return(TAC_W)
}
class(BR_W3s)<-"MP"
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


