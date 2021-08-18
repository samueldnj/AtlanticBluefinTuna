
#Indices 
#1	FR_AER_SUV2
#2	MED_LAR_SUV
#3	GOM_LAR_SUV
#4	GBYP_AER_SUV_BAR
#5	MOR_POR_TRAP
#6	JPN_LL_NEAtl2
#7	US_RR_66_114
#8	US_RR_115_144
#9	US_RR_177
#10	JPN_LL_West2
#11 CAN GSL
#12 CAN SWNS
#13	US_RR_66_144
#14 MEXUS_GOM_PLL

tune_parE=9.30
tune_parW=2.64

ConstU_E <- function( x,
											dset,
											IndexE=2,
											yrs4mean=3,
											lastyr = NULL,
											target_yr=55,
											deltaE_up=0.5,
											deltaE_down=0.5,
											phaseTime = 0,
                    	initPhz = 56,
											multiplierE=9.3)
{
	min_delta=(1-deltaE_down)
	max_delta=(1+deltaE_up)
	target_yrs=target_yr-(yrs4mean-1):0
	targetI=mean(dset$Iobs[x,IndexE,target_yrs],na.rm=TRUE)+mean(dset$Iobs[x,IndexE,target_yrs],na.rm=TRUE)
	targetC=mean(dset$Cobs[x,target_yrs],na.rm=TRUE)
	targetU=multiplierE*(targetC/targetI)

	# Change for retro
	if(is.null(lastyr))
		lastyr=dim(dset$Iobs)[3]

	if(lastyr < dim(dset$Iobs)[3])
  {
    lastTAC <- dset$Cobs[x,lastyr-1]
    currTAC <- dset$Cobs[x,lastyr]
  } else {
    lastTAC <- dset$MPrec[x]
    currTAC <- dset$curTAC[x]
  }

	# browser()
	
	
	
	datayrs=lastyr-(yrs4mean-1):0
	curI=mean(dset$Iobs[x,IndexE,datayrs],na.rm=TRUE)
	curC=mean(dset$Cobs[x,datayrs],na.rm=TRUE)
	curU=curC/curI
	delta_ratio=targetU/curU
	
	oldTAC = lastTAC
	
	if(delta_ratio<1)
	{
   	TAC=max(oldTAC*delta_ratio,oldTAC*min_delta)
	}
	else {
    TAC=min(oldTAC*delta_ratio,oldTAC*max_delta)
	}

	# Phase-in
	if(phaseTime > 0 & (lastyr <= initPhz + phaseTime) )
  {
    sqTAC <- dset$Cobs[x,initPhz]

    tNow <- lastyr - initPhz 
    # phase-in TAC
    TAC <- tNow/phaseTime * TAC + (phaseTime - tNow)/phaseTime * sqTAC
  }


	TAC
}
class(ConstU_E)<-"MP"

ConstU_W <- function( x,
											dset,
											IndexE=2,
											IndexW=3,
											yrs4mean=3,
											lastyr=NULL,
											target_yr=55,
											deltaW_down=0.5,
											deltaW_up=0.5,
											phaseTime = 0,
                    	initPhz = 56,
											multiplierW=2.64)
{
	min_delta=(1-deltaW_down)
	max_delta=(1+deltaW_up)
	target_yrs=target_yr-(yrs4mean-1):0
	targetI=mean(dset$Iobs[x,IndexW,target_yrs],na.rm=TRUE)/mean(dset$Iobs[x,IndexW,1:target_yr],na.rm=TRUE)+
				mean(dset$Iobs[x,IndexE,target_yrs],na.rm=TRUE)/mean(dset$Iobs[x,IndexE,1:target_yr],na.rm=TRUE)
	targetC=mean(dset$Cobs[x,target_yrs],na.rm=TRUE)
	targetU=targetC/targetI
	# Change for retro
	if(is.null(lastyr))
		lastyr=dim(dset$Iobs)[3]

	if(lastyr < dim(dset$Iobs)[3])
  {
    lastTAC <- dset$Cobs[x,lastyr-1]
    currTAC <- dset$Cobs[x,lastyr]
  } else {
    lastTAC <- dset$MPrec[x]
    currTAC <- dset$curTAC[x]
  }

	datayrs=lastyr-(yrs4mean-1):0
	curI=mean(dset$Iobs[x,IndexW,datayrs],na.rm=TRUE)/mean(dset$Iobs[x,IndexW,1:target_yr],na.rm=TRUE)+
				mean(dset$Iobs[x,IndexE,datayrs],na.rm=TRUE)/mean(dset$Iobs[x,IndexE,1:target_yr],na.rm=TRUE)
	curC=mean(dset$Cobs[x,datayrs],na.rm=TRUE)
	curU=(curC/curI)
	delta_ratio=multiplierW*targetU/curU

		
	oldTAC = lastTAC

	if(delta_ratio<1)
	{
 		TAC=max(oldTAC*delta_ratio,oldTAC*min_delta)
	}
	else {
    TAC=min(oldTAC*delta_ratio,oldTAC*max_delta)
	}

	# Phase-in
	if(phaseTime > 0 & (lastyr <= initPhz + phaseTime) )
  {
    sqTAC <- dset$Cobs[x,initPhz]

    tNow <- lastyr - initPhz 
    # phase-in TAC
    TAC <- tNow/phaseTime * TAC + (phaseTime - tNow)/phaseTime * sqTAC
  }

	
	TAC
}
class(ConstU_W)<-"MP"

# NOAA_CMPs<-list(
# c('ConstU_E','ConstU_W'))

# OM_Grid=t(sapply(1:48,function(i)
# 	{
#   	tempMSE<-new('MSE', OM=get(paste0('OM_',i,'d')), MPs=NOAA_CMPs)
#   	saveRDS(tempMSE,file=paste0(main_dir,"determ_runs_tuning/MSE_",i,".rda"))
# 	RES= c(Br30(tempMSE,pp=1)[2,1],Br30(tempMSE,pp=2)[2,1])  #E and W
# 	RES
# 	}))
# colMeans(OM_Grid)

# sapply(1:44,function(i)
# 	{
# 	tempMSE<-new('MSE',OM=get(paste0('ROM_',i,'d')),MPs=NOAA_CMPs)
# 	saveRDS(tempMSE,file=paste0(main_dir,"determ_runs_tuning/MSE_R_",i,".rda"))
# 	})

# CompRes <- Results_compiler(dir = paste0(main_dir,"/determ_runs_tuning"),
# 	name = "ML_1",
# 	CMPdesc = list(c("ConstU")))
# saveRDS(CompRes,paste0(main_dir,"CompRes_ML_JW_tuning.rda"))

