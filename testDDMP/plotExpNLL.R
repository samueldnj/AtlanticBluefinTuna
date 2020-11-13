plotExpNLL <- function( a=c(1e-2,0.1) )
{
  nA <- length(a)
  T <- 100
  t <- 1:T
  y <- list()
  for( i in 1:nA )
  {
    y[[i]] <- (1-exp(-a[i]*t)) / (1-exp(-a[i]*T))
  }
  y10 <- 0*t
  y10[(T-9):T] <- 1
  ymax <- max(sapply(y,max))
  plot( x=range(t), y=c(0,ymax), type="n", las=1, xlab="t", ylab="NLL weight" )
  grid()
  box()
  for( i in 1:nA )
    lines( x=t, y=y[[i]], lty=i+1, lwd=2 ) 
  lines( x=t, y=y10, lwd=2 )
  legend( x="topleft", legend=c("Last10",paste0("a=",a)), lty=1:(nA+1),
          bty="n", lwd=2 )
}