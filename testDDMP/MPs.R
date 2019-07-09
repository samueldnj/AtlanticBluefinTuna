# MPs.R

MP_loCap <- function( x, dset )
{
  TAC_a <- assessDDmm( x       = x,
                       dset    = dset,
                       AMs     = c(1,4,7),
                       caps    = c(20,2.5),
                       F23M    = FALSE,
                       TACrule = "mean" )
  return(c( East=TAC_a[1], West=TAC_a[2] ))
}
class(MP_loCap)<-"MMMSMP"


MP_hiCap <- function( x, dset )
{
  TAC_a <- assessDDmm( x       = x,
                       dset    = dset,
                       AMs     = c(1,4,7),
                       caps    = c(25,4),
                       F23M    = FALSE,
                       TACrule = "mean" )
  return(c( East=TAC_a[1], West=TAC_a[2] ))
}
class(MP_hiCap)<-"MMMSMP"


MP_loCap23M <- function( x, dset )
{
  TAC_a <- assessDDmm( x       = x,
                       dset    = dset,
                       AMs     = c(1,4,7),
                       caps    = c(20,2.5),
                       F23M    = TRUE,
                       TACrule = "mean" )
  return(c( East=TAC_a[1], West=TAC_a[2] ))
}
class(MP_loCap23M)<-"MMMSMP"

MP_hiCap23M <- function( x, dset )
{
  TAC_a <- assessDDmm( x       = x,
                       dset    = dset,
                       AMs     = c(1,4,7),
                       caps    = c(25,4),
                       F23M    = TRUE,
                       TACrule = "mean" )
  return(c( East=TAC_a[1], West=TAC_a[2] ))
}
class(MP_hiCap23M)<-"MMMSMP"