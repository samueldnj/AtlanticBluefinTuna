# ==============================================================================
# === Functions for iterative reweighting of M3 models =========================
# ==============================================================================

# May 12th 2018

CalcCVs<-function(out,OMI){

  Ccv<-sd(log(out$Cobs[,5])-log(out$Cpred[out$Cobs[,1:4]]))
  Cspec<-OMI@CobsCV[1]
  Cfac<-Ccv/Cspec

  CPUEres<-log(OMI@CPUEobs[,6])-log(out$CPUEpred_vec)
  CPUEcv<-aggregate(CPUEres,list(OMI@CPUEobs[,4]),sd)$x
  CPUEspec<-OMI@CPUEobsCV
  CPUEfac<-mean(CPUEcv/CPUEspec)

  Ires<-log(OMI@Iobs[,7])-log(out$Ipred_vec)
  Icv<-aggregate(Ires,list(OMI@Iobs[,5]),sd)$x
  Ispec<-OMI@IobsCV
  Ifac<-mean(Icv/Ispec)

  #log(CLprop(i)+tiny),log(CLobs(i,6)),sqrt(0.01/CLobs(i,6))

  CLres<-log(OMI@CLobs[,6])-log(out$CLprop)
  CLspec_byobs<-(0.01/OMI@CLobs[,6])^0.5
  CLfac<-sd(CLres/CLspec_byobs)

  #propbins<-floor(OMI@CLobs[,6]*5)/5
  #CLcv<-aggregate(CLres,list(propbins),sd)$x
  #CLspec<-aggregate(CLspec_byobs,list(propbins),mean)$x
  #CLfac<-mean(CLcv/CLspec)

  SOOres<-OMI@SOOobs[,6]-out$SOOpred
  SOOfac<-sd(SOOres/OMI@SOOobs[,7])

  PSATres<-log(OMI@PSAT[,8]/out$PSATpred)
  PSATfac<-sd(PSATres)/1

  ind<- rep(c(T,F),100)[1:OMI@ny]
  RDcv<-sd(out$lnRD[,ind])
  RDspec<-OMI@RDCV
  RDfac<-RDcv/RDspec

  Fmodfac<-sd(out$Fmod)/OMI@FCV

  FDYfac<-sd(out$FDYt)/OMI@MICV

  list(Cfac=Cfac, CPUEfac=CPUEfac, Ifac=Ifac, CLfac=CLfac, SOOfac=SOOfac,
       PSATfac=PSATfac, RDfac=RDfac, Fmodfac=Fmodfac, FDYfac=FDYfac)

}




