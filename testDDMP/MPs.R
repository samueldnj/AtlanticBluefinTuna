# MPs.R

# MP_loCap - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West
MP_loCap <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,4,7),
                     caps    = c(20,2.5),
                     F23M    = FALSE,
                     TACrule = "mean",
                     AS      = AS  )
  return(TAC)
}
class(MP_loCap)<-"MSMP"

# MP_hiCap - a higher catch cap is applied,
# of 25 kt in the East, and 4.0 kt in the West
MP_hiCap <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                       dset    = dset,
                       AMs     = c(1,4,7),
                       caps    = c(25,4),
                       F23M    = FALSE,
                       TACrule = "mean",
                       AS      = AS )
  return(TAC)
}
class(MP_hiCap)<-"MSMP"

# MP_loCap23M - Same as loCap, but
# fixes Fmsy at 2/3 of the M value
# used in the MP, rather than the
# estimated value from the reference points
# calc
MP_loCap23M <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                       dset    = dset,
                       AMs     = c(1,4,7),
                       caps    = c(20,2.5),
                       F23M    = TRUE,
                       TACrule = "mean",
                       AS      = AS )
  return(TAC)
}
class(MP_loCap23M)<-"MSMP"

# MP_loCap23M - Same as hiCap, but
# fixes Fmsy at 2/3 of the M value
# used in the MP, rather than the
# estimated value from the reference points
# calc
MP_hiCap23M <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                       dset    = dset,
                       AMs     = c(1,4,7),
                       caps    = c(25,4),
                       F23M    = TRUE,
                       TACrule = "mean",
                       AS      = AS )
  return(TAC)
}
class(MP_hiCap23M)<-"MSMP"