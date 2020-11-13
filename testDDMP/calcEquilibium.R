calcEquilibrium <- function( rpt, F=0.1 )
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

calcMSY <- function( rpt, H=500, folder=NULL )
{
  hseq <- seq(0,0.999,length=H)
  C_sh <- array( data=0, dim=c(rpt$nS,H) )
  B_sh <- array( data=0, dim=c(rpt$nS,H) )

  for( h in 1:H )
  {
    eq <- calcEquilibrium(rpt,hseq[h])
    C_sh[ ,h] <- eq$Ce_s
    B_sh[ ,h] <- eq$Be_s
  }  

  stocks <- c("East stock","West stock")

  Fmsy_s <- numeric(rpt$nS)
  Bmsy_s <- numeric(rpt$nS)
  msy_s  <- numeric(rpt$nS)

  if( !is.null(folder) )
    pdf( paste(folder,"/BRP.pdf",sep=""), height=6, width=5 )

  par( mfrow=c(rpt$nS,1), mar=c(2,3,1,1), oma=c(2,1,0,0) )
  for( s in 1:rpt$nS )
  {
    Cspline <- splinefun( x=hseq, y=C_sh[s, ] )
    Bspline <- splinefun( x=hseq, y=B_sh[s, ] )
    Fmsy_s[s] <- uniroot(Cspline,range(hseq),deriv=1)$root
    BRPs      <- calcEquilibrium(rpt,Fmsy_s[s])
    Bmsy_s[s] <- BRPs$Be_s[s]
    msy_s[s]  <- BRPs$Ce_s[s]

    plot( x=hseq, y=hseq, type="n", ylim=c(0,max(B_sh[s, ])), las=1,
          xaxs="i", yaxs="i", xlim=c(0,0.6) )
    grid()
    box()
    lines( x=hseq, y=B_sh[s, ], lwd=3 )
    lines( x=hseq, y=C_sh[s, ], lwd=2, col="grey65" )
    segments( x0=Fmsy_s[s], y0=0, y1=Bmsy_s[s], col="red", lty=2 )
    segments( x0=0, x1=Fmsy_s[s], y0=msy_s[s], col="blue", lty=2 )
    segments( x0=0, x1=Fmsy_s[s], y0=Bmsy_s[s], col="darkgreen", lty=2 )

    legend( x="topright", bty="n", legend=stocks[s], cex=1.3 )

    legend( x="bottomright", bty="n",
            legend=c("SSB",
                     "Catch",
                     paste("MSY",round(msy_s[s],2),sep="="),
                     paste("BMSY",round(Bmsy_s[s],2),sep="="),
                     paste("FMSY",round(Fmsy_s[s],2),sep="="),""),
            lwd=c(3,2,NA,NA,NA,NA),
            col=c("black","grey65",NA,NA,NA,NA),
            text.col=c("black","black","blue","darkgreen","red",NA) )
  }
  axis( side=3 )

  mtext( side=1, text=expression("Fishing mortality rate (yr"^{-1}~")"), outer=TRUE, line=0.7 )
  mtext( side=2, text="Equilibrium biomass (kt)", outer=TRUE, line=-0.5 )

  if( !is.null(folder) )
    dev.off()

  list(Bmsy_s=Bmsy_s,Fmsy_s=Fmsy_s,msy_s=msy_s)

}