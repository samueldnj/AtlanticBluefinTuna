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
runCMPs <- function(  iOM = 1,
                      MPs = list( test = c("empMPtest_Mean","empMPtest_Mean") ),
                      assessInt = 2,
                      checkMPs = TRUE,
                      reloadABT = FALSE,
                      projFolderName = NULL )
{
  library(ABTMSE)

  # Load ABT objects in this environment
  # if(!any(grepl(x = ls(), pattern = "dset_example_West")))
  #   loadABT()

  checkMP <<- checkMPs

  OM <- paste0("OM_",iOM,"d")

  # Clear outTables directory
  outTableFolder <- "./outTables"

  outTableFiles <- list.files(outTableFolder, full.names = TRUE)
  if( length(outTableFiles) > 0)
    unlink(outTableFiles)


  # get OM as an environment variable from
  # the char vector label
  OMobj <- get(OM)

  # Count MPs
  nMPs  <- length(MPs)

  checkMP <<- checkMPs

  # Run MSE
  MSEobj <- new(  Class     = "MSE",
                  OM        = OMobj,
                  MPs       = MPs )

  # Assign MSE to a symbol that is descriptive
  MSEsymbol <- paste("MSE_",OM,sep = "")

  assign( x = paste("MSE_",OM,sep = ""),
          value = MSEobj )

  suppressWarnings(dir.create("MSEs"))
  if(!is.null(projFolderName))
  {
    projFolderPath <- file.path("MSEs",projFolderName)
    dir.create(file.path("MSEs",projFolderName))
    dir.create(file.path("MSEs",projFolderName,"rdas"))
    dir.create(file.path("MSEs",projFolderName,"reports"))
  }
  else projFolderPath <- "MSEs"

  # Save that MSE
  rdsName <- paste0("MSE_",iOM,".rda")
  saveRDS( MSEobj, file=file.path("MSEs",projFolderName,"rdas",rdsName) )
  # Create the report
  # MSE_report( MSEobj, 
  #             dir=file.path(getwd(),projFolderPath), 
  #             Author='Landmark Fisheries Research', 
  #             introtext=paste("Fixed HR perfect info CMPs on OM ", OM,sep =""), 
  #             filenam=paste(MSEsymbol,"_report",sep = ""))  

  return(MSEobj)
}