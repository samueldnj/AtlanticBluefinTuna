# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# MPs.R
#
# Wrappers for the assessDDmm() function for different
# MP settings (lo/hi caps, assuming F is 2/3 of M etc.)
#
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# MP_testMean - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West,
# TACs are averaged over 5 AMs with even weighting,
# and checktables are produced for MP development
MP_testMean <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(20,2.5),
                     F23M    = FALSE,
                     TACrule = "mean",
                     AS      = AS,
                     check   = TRUE,
                     mpName  = "MP_testMean"  )
  return(TAC)
}
class(MP_testMean)<-"MSMP"

# MP_testAIC - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West
# TACs are averaged over 5 AMs with AIC weighting,
# and checktables are produced for MP development
MP_testAIC <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(20,2.5),
                     F23M    = FALSE,
                     TACrule = "AIC",
                     AS      = AS,
                     check   = TRUE,
                     mpName  = "MP_testAIC"  )
  return(TAC)
}
class(MP_testAIC)<-"MSMP"

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

# MP_loCap23M.4B0 - Same as loCap_23M, but
# uses a proxy of .4B0 for Bmsy in the MP, rather 
# than the estimated value from the reference points
# calc
MP_loCap23M.4B0 <- function( x, dset, AS )
{
  TAC <- assessDDmm(  x       = x,
                      dset    = dset,
                      AMs     = c(1,2,4,7,11),
                      caps    = c(20,2.5),
                      F23M    = TRUE,
                      TACrule = "mean",
                      AS      = AS,
                      UCP     = ".4B0",
                      mpName  = "loCap23M.4B0" )

  return(TAC)
}
class(MP_loCap23M.4B0)<-"MSMP"

# MPtest_loCap23M.4B0 - Same as MP_loCap23M.4B0, but
# outputs a checktable for MP development
MPtest_loCap23M.4B0 <- function( x, dset, AS )
{
  TAC <- assessDDmm(  x       = x,
                      dset    = dset,
                      AMs     = c(1,2,4,7,11),
                      caps    = c(20,2.5),
                      F23M    = TRUE,
                      TACrule = "mean",
                      check   = TRUE,
                      AS      = AS,
                      UCP     = ".4B0",
                      mpName  = "MPtest_loCap23M.4B0" )

  return(TAC)
}
class(MPtest_loCap23M.4B0)<-"MSMP"


# runCMPtest()
# Wrapper function for the new("MSE")
# call. Allows for lapply functions
# to be called on a list of OMs. Needed
# to deploy to servers so we can set and forget
# Inputs:
#   OM = character of OM name 
#         (e.g. "OM_1","OM_1d" or "ROM_1")
#   MPs = a list of character 2-ples of MP names (E/W)
#   assessInt = integer of assessment intervals, required
#               for plotting purposes
runCMPtest <- function( OM = "OM_1d",
                        MPs = list( test = c("MP_testMean","MP_testMean") ),
                        assessInt = 2 )
{
  # Load ABT objects in this environment
  loadABT()

  # get OM as an environment variable from
  # the char vector label
  OMobj <- get(OM)

  # Count MPs
  nMPs  <- length(MPs)

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
    for( j in 1:nMPs )
    {
      MPid <- MPs[[j]][1]
      # Create a directory for the MP if it doesn't exist
      if( !dir.exists(file.path("MSEs/fitCheck",OM,MPid)) )
        dir.create(file.path("MSEs/fitCheck",OM,MPid))
      
      fitCheckPlot <- paste("fitCheck_sim",i,"_",OM,"_",MPid,".png",sep = "")
      png(  filename = file.path("MSEs/fitCheck",OM,MPid,fitCheckPlot),
            width = 8.5, height = 11, units = "in", res = 300 )
      plot_TACperformance(  simNum   = i,
                            MSEobj   = MSEobj,
                            tables   = checkTables,
                            MPlist   = MPs,
                            MPnum    = j,
                            interval = assessInt )
      dev.off()
    }
    # Remove checkTables for next run
    system( paste("rm -r ", outTableFiles[i], sep = "") )
  }
}

# runCMP()
# Wrapper function for the new("MSE")
# call. Allows for lapply functions
# to be called on a list of OMs. Needed
# to deploy to servers so we can set and forget.
# Basically the same as runCMPtest() but
# doesn't plot fit checks
# Inputs:
#   OM = character of OM name 
#         (e.g. "OM_1","OM_1d" or "ROM_1")
#   MPs = a list of character 2-ples of MP names (E/W)
#   assessInt = integer of assessment intervals, required
#               for plotting purposes
runCMPs <- function(  OM = "OM_1",
                      MPs = list( test = c("MP_loCap","MP_loCap") ),
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
  MSEsymbol <- paste("MSE_",OM,sep = "")

  assign( x = paste("MSE_",OM,sep = ""),
          value = MSEobj )

  # Save that MSE
  save( list = MSEsymbol, file = file.path("MSEs",paste(MSEsymbol,".Rdata",sep = "")) )  
  # Create the report
  MSE_report( MSEobj, 
              dir=file.path(getwd(),"MSEs"), 
              Author='Landmark Fisheries Research', 
              introtext="Multi-model delay difference assessment", 
              filenam=paste(MSEsymbol,"_report",sep = ""))  


}