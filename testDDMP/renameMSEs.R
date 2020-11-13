saveRDSs <- function()
{
  for( i in 1:96 )
  {
    load(paste("MSE_OM_",i,"d.Rdata",sep=""))
    saveRDS( get(paste("OM_",i,"d",sep="")),
             file=paste0("../LFR-DelayDiff/MSE_",i,".rda") )
  }
  for( i in 1:12 )
  {
    load(paste("MSE_ROM_",i,"d.Rdata",sep=""))
    saveRDS( get(paste("ROM_",i,"d",sep="")),
             file=paste0("../LFR-DelayDiff/MSE_R_",i,".rda") )
  }
}

renameMSEs <- function()
{
  for( i in 1:96 )
    system( paste("mv MSE_R_",i,".rda",
                  " MSE_",i,".rda",sep="") )
}

i <- 1
tempMSE <- new('MSE',OM=get(paste0('OM_',i,'d')),MPs=list(MP_mix = c("MP_noCapF23M","MP_msyCapF23M")),
               Obs=Perfect_Obs, Deterministic=TRUE)
saveRDS(tempMSE,file=paste0("LFR-DelayDiff/MSE_R_",i,".rda"))
z<-readRDS("~/ABT/testDDMP/MSEs/LFR-DelayDiff/MSE_96.rda")



CompRes <- Results_compiler(dir="MSEs/LFR-DelayDiff",
                            name="LFR-DelayDiff",
                            CMPdesc=c("East: Uncapped, Fmax=M; West: TAC capped at MSY, Fmax=M*2/3"))

saveRDS(CompRes,"LFR-DelayDiff.rda")