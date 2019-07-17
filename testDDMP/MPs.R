# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# MPs.R
#
# Wrappers for the assessDDmm() function for different
# MP settings (lo/hi caps, assuming F is 2/3 of M etc.)
#
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# MP_loCap - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West
MP_test <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(20,2.5),
                     F23M    = FALSE,
                     TACrule = "mean",
                     AS      = AS,
                     check   = TRUE  )
  return(TAC)
}
class(MP_test)<-"MSMP"

# MP_loCap - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West
MP_loCap <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
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
                       AMs     = c(1,2,4,7,11),
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
                       AMs     = c(1,2,4,7,11),
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
                       AMs     = c(1,2,4,7,11),
                       caps    = c(25,4),
                       F23M    = TRUE,
                       TACrule = "mean",
                       AS      = AS )
  return(TAC)
}
class(MP_hiCap23M)<-"MSMP"


runCMPtest <- function( OM = OMlist[1],
                        MPs = testMPs,
                        assessInt = 2 )
{
  # Load ABT objects in this environment
  loadABT()

  OMobj <- get(OM)

  # Run MSE
  MSEobj <- new(  Class     = "MSE",
                  OM        = OMobj,
                  MPs       = MPs,
                  interval  = assessInt )

  # Assign MSE to a symbol that is descriptive
  MSEsymbol <- paste("MSEtest_",OM,sep = "")

  assign( x = paste("MSEtest_",OM,sep = ""),
          value = MSEobj )

  # Save that MSE
  save( list = MSEsymbol, file = file.path("MSEs",paste(MSEsymbol,".Rdata",sep = "")) )  
  # Create the report
  MSE_report( MSEobj, 
              dir=file.path(getwd(),"MSEs"), 
              Author='Landmark Fisheries Research', 
              introtext="Multi-model delay difference assessment", 
              filenam=paste(MSEsymbol,"_report",sep = ""))  

  # Collect the checkTables
  outTableFiles <- list.files("./outTables", full.names = TRUE)
  nSims <- length(outTableFiles)
  checkTables <- lapply(  X = outTableFiles, FUN = read.csv,
                          header = TRUE, stringsAsFactors = FALSE )


  # Save to output directory
  if(!dir.exists("MSEs/fitCheck"))
    dir.create("MSEs/fitCheck")

  if(!dir.exists(file.path("MSEs/fitCheck",OM)))
    dir.create(file.path("MSEs/fitCheck",OM))

  checkTablesSavePath <- file.path("MSEs/fitCheck",OM,"checkTables.Rdata")

  save( checkTables, file = checkTablesSavePath )

  for( i in 1:nSims )
  {
    fitCheckPlot <- paste("fitCheck_sim",i,"_",OM,".png",sep = "")
    png(  filename = file.path("MSEs/fitCheck",OM,fitCheckPlot),
          width = 7, height = 11, units = "in", res = 300 )
    plot_AMfits(  simNum   = i,
                  MSEobj   = MSEobj,
                  tables   = checkTables,
                  MPnum    = 2,
                  interval = assessInt )
    dev.off()
    # Remove checkTables for next run
    system( paste("rm -r ", outTableFiles[i], sep = "") )
  }
}