
# ===================================================
# = functions for model based MPs in TMB ============
# ===================================================

.calcEquilibriumDD <- function( rpt, F=0.1 )
{
  # Rescale parameters
  a <- rpt$reca_s/rpt$recb_s
  b <- 1/rpt$recb_s

  M_s   <- rpt$M_s
  rho_s <- rpt$rho_s

  # Growth-survival constant K
  #surv_s  <- exp(-M_s-F)
  surv_s  <- exp(-M_s)*(1-F)
  numer_s <- 1 - (1+rho_s)*surv_s + rho_s*surv_s^2
  denom_s <- rpt$wk_s - rho_s*rpt$wk1_s*surv_s
  K_s <- numer_s / denom_s

  # Equilibrium biomass
  #Be_s <- a/K_s - b/exp(-F)
  Be_s <- a/K_s - b/(1-F)

  # Equilibrium catch
  #Ce_s <- Be_s*(1-surv_s)*F/(F+M_s)
  Ce_s <- Be_s*F

  # Equilibrium recruitment
  Re_s <- Be_s*K_s

  # Equilibrium numbers
  Ne_s <- Re_s/(1-surv_s)

  # Equilibrium body weight
  We_s <- (1-surv_s)/K_s

  list( Be_s=Be_s, Ce_s=Ce_s, Re_s=Re_s, Ne_s=Ne_s, We_s=We_s )

}


.calcEquilibriumDD_instF <- function( rpt, F=0.1 )
{
  # Rescale parameters
  a <- rpt$reca_s/rpt$recb_s
  b <- 1/rpt$recb_s

  # Biological pars
  reca_s  <- rpt$reca_s
  recb_w  <- rpt$recb_s
  M_s     <- rpt$M_s
  rho_s   <- rpt$rho_s
  alpha_s <- rpt$alpha_s
  wk_s    <- rpt$wk_s
  # Survival
  surv_s  <- exp(-M_s - F)
  # numer_s <- 1 - (1+rho_s)*surv_s + rho_s*surv_s^2
  # denom_s <- rpt$wk_s - rho_s*rpt$wk1_s*surv_s
  # K_s <- numer_s / denom_s

  # Average weight
  We_s <- surv_s * alpha_s + wk_s*(1. - surv_s )
  We_s <- We_s / ( 1. - rho_s*surv_s);

  # Biomass
  Be_s <- (surv_s*(alpha_s + rho_s*We_s) + We_s*(reca_s*wk_s - 1));
  Be_s  <- Be_s / ( recb_s * ( We_s - rho_s*surv_s*We_s - alpha_s * surv_s) );

  # Numbers
  Ne_s <- Be_s / We_s;
  # Recruitment
  Re_s <- reca_s * Be_s / (1. + recb_s * Be_s);

  # # Growth-survival constant K
  # #surv_s  <- exp(-M_s-F)
  

  # # Equilibrium biomass
  # #Be_s <- a/K_s - b/exp(-F)
  # Be_s <- a/K_s - b/(1-F)

  # Equilibrium catch
  Ce_s <- Be_s*(1-surv_s)*F/(F+M_s)
  # Ce_s <- Be_s*F

  # Equilibrium recruitment
  # Re_s <- Be_s*K_s

  # Equilibrium numbers
  # Ne_s <- Re_s/(1-surv_s)

  # Equilibrium body weight
  # We_s <- (1-surv_s)/K_s

  list( Be_s=Be_s, Ce_s=Ce_s, Re_s=Re_s, Ne_s=Ne_s, We_s=We_s )

}

.calcMSY <- function( rpt, H=500 )
{
  hseq <- seq(0,0.999,length=H)
  C_sh <- array( data=0, dim=c(rpt$nS,H) )
  B_sh <- array( data=0, dim=c(rpt$nS,H) )

  for( h in 1:H )
  {
    eq <- .calcEquilibriumDD(rpt,hseq[h])
    C_sh[ ,h] <- eq$Ce_s
    B_sh[ ,h] <- eq$Be_s
  }  

  stocks <- c("East stock","West stock")

  Fmsy_s <- numeric(rpt$nS)
  Bmsy_s <- numeric(rpt$nS)
  msy_s  <- numeric(rpt$nS)

  
  for( s in 1:rpt$nS )
  {
    Cspline <- splinefun( x=hseq, y=C_sh[s, ] )
    Bspline <- splinefun( x=hseq, y=B_sh[s, ] )
    Fmsy_s[s] <- uniroot(Cspline,range(hseq),deriv=1)$root
    BRPs      <- .calcEquilibriumDD(rpt,Fmsy_s[s])
    Bmsy_s[s] <- BRPs$Be_s[s]
    msy_s[s]  <- BRPs$Ce_s[s]

  }

  list(Bmsy_s=Bmsy_s,Fmsy_s=Fmsy_s,msy_s=msy_s)

}