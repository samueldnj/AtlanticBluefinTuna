# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# MPs.R
#
# Wrappers for the assessDDmm() function for different
# MP settings (lo/hi caps, assuming F is 2/3 of M etc.)
#
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

checkMP <<- TRUE

# MP_testMean - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West,
# TACs are averaged over 5 AMs with even weighting,
# and checktables are produced for MP development
MPtest_Mean <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(20,2.5),
                     F23M    = FALSE,
                     TACrule = "mean",
                     AS      = AS,
                     check   = checkMP,
                     mpName  = "MPtest_Mean"  )
  return(TAC)
}
class(MPtest_Mean)<-"MSMP"

# MP_testAIC - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West
# TACs are averaged over 5 AMs with AIC weighting,
# and checktables are produced for MP development
MPtest_AIC <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(20,2.5),
                     F23M    = FALSE,
                     TACrule = "AIC",
                     AS      = AS,
                     check   = checkMP,
                     mpName  = "MPtest_AIC"  )
  return(TAC)
}
class(MPtest_AIC)<-"MSMP"

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
                      check   = checkMP,
                      AS      = AS,
                      UCP     = ".4B0",
                      mpName  = "MPtest_loCap23M.4B0" )

  return(TAC)
}
class(MPtest_loCap23M.4B0)<-"MSMP"

# MP_msyCap - a catch cap at estimated
# MSY is applied
MP_msyCap <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(Inf,Inf),
                     F23M    = FALSE,
                     TACrule = "mean",
                     check   = checkMP,
                     AS      = AS,
                     mpName  = "MP_msyCap"  )
  return(TAC)
}
class(MP_msyCap)<-"MSMP"

# MP_msyCap - a catch cap at estimated
# MSY is applied
MP_msyCapF23M <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(Inf,Inf),
                     F23M    = TRUE,
                     TACrule = "mean",
                     check   = checkMP,
                     AS      = AS,
                     mpName  = "MP_msyCapF23M"  )
  return(TAC)
}
class(MP_msyCapF23M)<-"MSMP"

# MP_msyCap - a catch cap at estimated
# MSY is applied
MP_msyCapF23M.4B0 <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(Inf,Inf),
                     F23M    = TRUE,
                     TACrule = "mean",
                     check   = checkMP,
                     AS      = AS,
                     UCP     = ".4B0",
                     mpName  = "MP_msyCapF23M.4B0"  )
  return(TAC)
}
class(MP_msyCapF23M.4B0)<-"MSMP"


# MP_msyCap - a catch cap at estimated
# MSY is applied
MP_aic_msyCap <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(Inf,Inf),
                     F23M    = FALSE,
                     TACrule = "AIC",
                     check   = checkMP,
                     AS      = AS,
                     mpName  = "MP_aic_msyCap"  )
  return(TAC)
}
class(MP_aic_msyCap)<-"MSMP"

# MP_aic_msyCap - a catch cap at estimated
# MSY is applied
MP_aic_msyCapF23M <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(Inf,Inf),
                     F23M    = TRUE,
                     TACrule = "AIC",
                     check   = checkMP,
                     AS      = AS,
                     mpName  = "MP_aic_msyCapF23M"  )
  return(TAC)
}
class(MP_aic_msyCapF23M)<-"MSMP"

# MP_aic_msyCap - a catch cap at estimated
# MSY is applied
MP_aic_msyCapF23M.4B0 <- function( x, dset, AS )
{
  TAC <- assessDDmm( x       = x,
                     dset    = dset,
                     AMs     = c(1,2,4,7,11),
                     caps    = c(Inf,Inf),
                     F23M    = TRUE,
                     TACrule = "AIC",
                     check   = checkMP,
                     AS      = AS,
                     UCP     = ".4B0",
                     mpName  = "MP_aic_msyCapF23M.4B0"  )
  return(TAC)
}
class(MP_aic_msyCapF23M.4B0)<-"MSMP"


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
                     check   = checkMP,
                     AS      = AS,
                     mpName  = "MP_loCap"  )
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
                       check   = checkMP,
                       AS      = AS,
                       mpName  = "MP_hiCap" )
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
                       check   = checkMP,
                       AS      = AS,
                       mpName  = "MP_loCap23M" )
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
                       check   = checkMP,
                       AS      = AS,
                       mpName  = "MP_hiCap23M" )
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
                      check   = checkMP,
                      AS      = AS,
                      UCP     = ".4B0",
                      mpName  = "MP_loCap23M.4B0" )

  return(TAC)
}
class(MP_loCap23M.4B0)<-"MSMP"

# MP_loCap23M - Same as hiCap, but
# fixes Fmsy at 2/3 of the M value
# used in the MP, rather than the
# estimated value from the reference points
# calc
MP_hiCap23M.4B0 <- function( x, dset, AS )
{
  TAC <- assessDDmm(  x       = x,
                      dset    = dset,
                      AMs     = c(1,2,4,7,11),
                      caps    = c(25,4),
                      F23M    = TRUE,
                      TACrule = "mean",
                      check   = checkMP,
                      AS      = AS,
                      mpName  = "MP_hiCap23M.4B0" )
  return(TAC)
}
class(MP_hiCap23M.4B0)<-"MSMP"


loadMSE <- function(  OMid = "OM_1d",
                      prefix = "test",
                      folder = "./MSEs" )
{ 

  # Search the folder for MSE outputs
  mseObjFileList  <- list.files(folder)
  mseObjID        <- paste("MSE", prefix, "_", OMid, sep = "" )
  mseFileName     <- paste(mseObjID, ".Rdata", sep = "")

  load( file.path(folder,mseFileName) )
  
  return( get( mseObjID ) )
}


plotFitChecks <- function(  OM = "OM_1d",
                            projFolder = "./MSEs/testMP",
                            prefix = "",
                            assessInt = 2 )
{
  # Load OM object
  MSEobj <- loadMSE(  OMid = OM,
                      prefix = prefix,
                      folder = projFolder )

  # Load check tables

  fitCheckFolder <- file.path(projFolder, "fitCheck")
  checkTablePath <- file.path(fitCheckFolder, OM, "checkTables.Rdata")
  load(checkTablePath)

  # Get dimensions of objects
  nSims <- dim(MSEobj@SSB)[2]
  nMPs  <- length(MSEobj@MPs) - 1
  MPs   <- MSEobj@MPs[-1]

  for( i in 1:nSims )
    {
      for( j in 1:nMPs )
      {
        MPid <- names(MPs)[j]
        # Create a directory for the MP if it doesn't exist
        if( !dir.exists(file.path(fitCheckFolder,OM,MPid)) )
          dir.create(file.path(fitCheckFolder,OM,MPid))
        
        fitCheckPlot <- paste("fitCheck_sim",i,"_",OM,"_",MPid,".png",sep = "")
        png(  filename = file.path(fitCheckFolder,OM,MPid,fitCheckPlot),
              width = 8.5, height = 11, units = "in", res = 300 )
        plot_TACperformance(  simIdx      = i,
                              MSEobj      = MSEobj,
                              westTables  = ewCheckTableList$west,
                              eastTables  = ewCheckTableList$east,
                              MPlist      = MPs,
                              MPidx       = j,
                              interval    = assessInt )
        dev.off()
      }
    }

}


# runCMPs()
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
runCMPs <- function(  OM = "OM_1d",
                      MPs = list( test = c("MP_testMean","MP_testMean") ),
                      assessInt = 2,
                      checkMPs = FALSE,
                      projFolderName = NULL )
{
  library(ABTMSE)
  library(TMB)

  source("assessDDMP.R")
  source("calcEquilibriumDD.R")
  source("MPs.R")
  # Load ABT objects in this environment
  loadABT()

  # Set Check switch
  checkMP <<- checkMPs

  # Clear outTables directory
  if(!dir.exists("./outTables"))
    dir.create("./outTables")
  
  outTableFiles <- list.files("./outTables", full.names = TRUE)
  if( length(outTableFiles) > 0)
    unlink(outTableFiles)

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
  MSEsymbol <- paste("MSE_",OM,sep = "")

  assign( x = MSEsymbol,
          value = MSEobj )

  if(!is.null(projFolderName))
  {
    projFolderPath <- file.path("MSEs",projFolderName)
    dir.create(file.path("MSEs",projFolderName))
  }
  else projFolderPath <- "MSEs"

  # Save that MSE
  save( list = MSEsymbol, file = file.path(projFolderPath,paste(MSEsymbol,".Rdata",sep = "")) )  
  # Create the report
  MSE_report( MSEobj, 
              dir=file.path(getwd(),projFolderPath), 
              Author='Landmark Fisheries Research', 
              introtext=paste("Multi-model delay difference assessment on", OM,sep =""), 
              filenam=paste(MSEsymbol,"_report",sep = ""))  

  # Collect the checkTables
  outTableFiles <- list.files("./outTables", full.names = TRUE)
  nSims <- dim(MSEobj@SSB)[2]

  westCheckTableFiles <- outTableFiles[grepl(x = outTableFiles, pattern = "West") ]
  eastCheckTableFiles <- outTableFiles[grepl(x = outTableFiles, pattern = "East") ]

  westCheckTables <- lapply(  X = westCheckTableFiles, FUN = read.csv,
                              header = TRUE, stringsAsFactors = FALSE )

  eastCheckTables <- lapply(  X = eastCheckTableFiles, FUN = read.csv,
                              header = TRUE, stringsAsFactors = FALSE )

  if(length(westCheckTables) != nSims | length(eastCheckTables) != nSims )
    browser(cat("Wrong number of checkTables") )


  # Save to output directory
  fitCheckFolder <- file.path(projFolderPath,"fitCheck")
  if(!dir.exists(fitCheckFolder))
    dir.create(fitCheckFolder)

  if(!dir.exists(file.path(fitCheckFolder,OM)))
    dir.create(file.path(fitCheckFolder,OM))

  checkTablesSavePath <- file.path(fitCheckFolder,OM,"checkTables.Rdata")

  ewCheckTableList <- list( east = eastCheckTables, west = westCheckTables)

  save( ewCheckTableList, 
        file = checkTablesSavePath )

  if( checkMPs )
  {

    for( i in 1:nSims )
    {
      for( j in 1:nMPs )
      {
        MPid <- MPs[[j]][1]
        # Create a directory for the MP if it doesn't exist
        if( !dir.exists(file.path(fitCheckFolder,OM,MPid)) )
          dir.create(file.path(fitCheckFolder,OM,MPid))
        
        fitCheckPlot <- paste("fitCheck_sim",i,"_",OM,"_",MPid,".png",sep = "")
        png(  filename = file.path(fitCheckFolder,OM,MPid,fitCheckPlot),
              width = 8.5, height = 11, units = "in", res = 300 )
        plot_TACperformance(  simIdx      = i,
                              MSEobj      = MSEobj,
                              westTables  = ewCheckTableList$west,
                              eastTables  = ewCheckTableList$east,
                              MPlist      = MPs,
                              MPidx       = j,
                              interval    = assessInt )
        dev.off()
      }
      
    }
  }
  # Remove outTableFiles
  if( length(outTableFiles) > 0)
    unlink(outTableFiles)

  return(MSEobj)
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
runCMPsOld <- function( OM = "OM_1",
                        MPs = list( test = c("MP_loCap","MP_loCap") ),
                        assessInt = 2 )
{
  # library(ABTMSE)
  # library(TMB)

  # source("assessDDMP.R")
  # source("calcEquilibriumDD.R")
  # source("MPs.R")

  # Load ABT objects in this environment
  # loadABT()

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
  save( list = MSEsymbol, file = file.path(projFolderPath,paste(MSEsymbol,".Rdata",sep = "")) )  
  # Create the report
  MSE_report( MSEobj, 
              dir=file.path(getwd(),"MSEs"), 
              Author='Landmark Fisheries Research', 
              introtext=paste("Multi-model delay difference assessment on", OM,sep =""), 
              filenam=paste(MSEsymbol,"_report",sep = ""))  


}