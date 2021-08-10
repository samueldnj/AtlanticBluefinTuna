## MPs for EU using the weighted mean. 
# Weight (w=1/sigma^2) is calculating the sigma values provided in document SCRS_2020_075 (Table 1, see below), 
# for each of the indices used in the EAST and the WEST respectively. 

# 1.FR_AER_SUV2  (sigma=0.76 -> w=1.74)
# 2.MED_LAR_SUV (sigma= 1.03 -> w= 0.95)
# 5.MOR_POR_TRAP (sigma= 0.53 -> w = 3.59)
# 6.JPN_LL_NEAtl2 (sigma=0.62 -> w= 2.61) - Modified after reconditioning. Took values from Tom's presentation. 

# NEWS: target value for the Eastern stock is set to 1, and a 
# tunning parameter Gamma is used for both stocks. Now Irat (the 
# ratio between the current value of the indices
# and the target value, is affected by this tunning paramenter)

EA_2_E= function(x,dset,Targ=1.3,Deltaup=0.15,Deltadown=0.15,yrs4mean=3){

#normalizing indices: years 2014 to 2016 are years 50 to 52 since model starts in 1965  
  lastyr = dim(dset$Iobs)[3]                # Most recent year
  datayrs = lastyr-(yrs4mean-1):0           # Position of data for calculating current index
  
# we select the following indices for the East:
  
  dset$Iobs[x,1,]=dset$Iobs[x,1,]/mean(dset$Iobs[x,1,50:55], na.rm=TRUE)
  dset$Iobs[x,2,]=dset$Iobs[x,2,]/mean(dset$Iobs[x,2,50:55], na.rm=TRUE)
  dset$Iobs[x,5,]=dset$Iobs[x,5,]/mean(dset$Iobs[x,5,50:55], na.rm=TRUE)
  dset$Iobs[x,6,]=dset$Iobs[x,6,]/mean(dset$Iobs[x,6,50:55], na.rm=TRUE)
  
#Weights
  w = c(1.74, 0.95, 3.59, 2.61)
  #w=c(0.1, 0.5, 0.5, 1)

  #Calculating the weigthed mean
  curI = weighted.mean(as.vector(dset$Iobs[x,c(1,2,5,6),datayrs]),rep(w,3), na.rm=T) # mean of last yrs4mean years and five indices selected for the East area

  
   #  curI = mean(dset$Iobs[x,1,datayrs],na.rm=T) #
  
  Gamma <- 0.15
  Irat = curI/Targ                          # Index ratio
  Irat_n = Gamma*Irat + (1-Gamma)
  
  
  oldTAC = dset$MPrec[x]                    # The last TAC recommendation

  if (oldTAC>=50000){                     #Including a CAP at 50000 Tn TAC
    TAC=50000

  }else{
  
  if(Irat_n<(1-Deltadown)){                       # If index ratio is less than minimum adjustment
    TAC = oldTAC*(1-Deltadown)
    
    }else if(Irat_n>(1+Deltaup)){                 # If index ratio is greater than maximum adjustment
      TAC = oldTAC*(1+Deltaup)
    
    }else{
      TAC = oldTAC*Irat_n
  }
  
  TAC                                       # Last thing returned is the TAC recommendation
  }
}

class(EA_2_E) = "MP"   


# MP West 1:

# Indices used for the WEST: 
# 3.GOM_LAR_SUV (sigma = 0.70 -> w= 2.033)
# 10.JPN_LL_West2 (sigma=0.57 -> w= 3.045)
# 13.US_RR_66_114 (sigma= 1.16 -> w= 0.744)
# 14.MEXUS_GOM_PLL (sigma= 0.52 -> w= 3.68)


EA_2_W = function(x,dset,Targ=1.25, Deltaup=0.15,Deltadown=0.15, yrs4mean=3, na.rm=T){
#normalizing indices: years 2014 to 2016 are years 50 to 52 since model starts in 1965
  
  lastyr = dim(dset$Iobs)[3]                # Most recent year
  datayrs = lastyr-(yrs4mean-1):0           # Position of data for calculating current index
  
  dset$Iobs[x,3,]=dset$Iobs[x,3,]/mean(dset$Iobs[x,3,50:55], na.rm=TRUE)
  dset$Iobs[x,10,]=dset$Iobs[x,10,]/mean(dset$Iobs[x,10,50:55], na.rm=TRUE)
  dset$Iobs[x,13,]=dset$Iobs[x,13,]/mean(dset$Iobs[x,13,50:55], na.rm=TRUE)
  dset$Iobs[x,14,]=dset$Iobs[x,14,]/mean(dset$Iobs[x,14,50:55], na.rm=TRUE)

#Weights  

    w=c(2.033, 3.045, 0.744, 3.68)
    #w= c(1, 0.2, 0.5, 0.9)
#Calculating weighted mean
    curI = weighted.mean(as.vector(dset$Iobs[x,c(3,10,13,14),datayrs]), rep(w,3), na.rm=T) # mean of last yrs4mean years and four indices selected for the West area
  
  #  curI = mean(dset$Iobs[x,IndexNo,datayrs],na.rm=T) # mean of last four years
  
  Gamma <- 0.15
  Irat = curI/Targ        # Index ratio
  Irat_n = (Gamma*Irat) + (1-Gamma)
  
  oldTAC = dset$MPrec[x]                    # The last TAC recommendation
  
  if(Irat_n<(1-Deltadown)){                       # If index ratio is less than minimum adjustment
    TAC = oldTAC*(1-Deltadown)
    
  }else if(Irat_n>(1+Deltaup)){                 # If index ratio is greater than maximum adjustment
    TAC = oldTAC*(1+Deltaup)
    
  }else{
    TAC = oldTAC*Irat_n
  }
  
  TAC                                       # Last thing returned is the TAC recommendation
  
}

class(EA_2_W) = "MP"             # Finally make sure it is of class MP

