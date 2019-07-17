# --------------------------------------------------------------------------
# testMP.R
# 
# Loads the ABTMSE package (local modified version) and 
# runs a test version of the TMB DD model based procedure, 
# creating a plot of AM performance
#
# Author: Samuel D N Johnson
# Date: July 16, 2019
#
# --------------------------------------------------------------------------

# Load the ABTMSE package
source("initTest.R")
source("assessDDMP.R")
source("calcEquilibriumDD.R")
source("MPs.R")
source("plots.R")

sfInit(parallel=TRUE, cpus= 2 )
sfLibrary( ABTMSE )
sfLibrary( TMB )
sfClusterCall("loadABT")
sfSource("assessDDMP.R")
sfSource("calcEquilibriumDD.R")

# Squelch warnings for Rmd output
options( warn = -1 )

if(!dir.exists("outTables"))
  dir.create("outTables")

testMPs <- list(  test = c("MP_testMean","MP_testMean") )

OMlist <- c("OM_1d","OM_2d", "OM_3d", "OM_4d", "ROM_1d")

lapply( X = OMlist, 
        FUN = runCMPtest,
        assessInt = 2,
        MPs = testMPs )

runCMPtest( OM = "OM_1",
            MPs = testMPs,
            assessInt = 2 )



# for( omIdx in 1:length(OMlist))
# {
#   OM <- OMlist[omIdx]

#   OMobj <- get(OM)

#   # Run MSE
#   MSEobj <- new(  Class     = "MSE",
#                   OM        = OMobj,
#                   MPs       = testMPs,
#                   interval  = assessInt )

#   # Assign MSE to a symbol that is descriptive
#   MSEsymbol <- paste("MSEtest_",OM,sep = "")

#   assign( x = paste("MSEtest_",OM,sep = ""),
#           value = MSEobj )

#   # Save that MSE
#   save( list = MSEsymbol, file = file.path("MSEs",paste(MSEsymbol,".Rdata",sep = "")) )  
#   # Create the report
#   MSE_report( MSE, 
#               dir=file.path(getwd(),"MSEs"), 
#               Author='Landmark Fisheries Research', 
#               introtext="Multi-model delay difference assessment", 
#               filenam=paste(MSEsymbol,"_report",sep = ""))  

#   # Collect the checkTables
#   outTableFiles <- list.files("./outTables", full.names = TRUE)
#   nSims <- length(outTableFiles)
#   checkTables <- lapply(  X = outTableFiles, FUN = read.csv,
#                           header = TRUE, stringsAsFactors = FALSE )


#   # Save to output directory
#   if(!dir.exists("MSEs/fitCheck"))
#     dir.create("MSEs/fitCheck")

#   if(!dir.exists(file.path("MSEs/fitCheck",OM)))
#     dir.create(file.path("MSEs/fitCheck",OM))

#   checkTablesSavePath <- file.path("MSEs/fitCheck",OM,"checkTables.Rdata")

#   save( checkTables, file = checkTablesSavePath )

#   for( i in 1:nSims )
#   {
#     fitCheckPlot <- paste("fitCheck_sim",i,"_",OM,".png",sep = "")
#     png(  filename = file.path("MSEs/fitCheck",OM,fitCheckPlot),
#           width = 7, height = 11, units = "in", res = 300 )
#     plot_AMfits(  simNum   = i,
#                   MSEobj   = MSE,
#                   tables   = checkTables,
#                   MPnum    = 2,
#                   interval = assessInt )
#     dev.off()
#     # Remove checkTables for next run
#     system( paste("rm -r ", outTableFiles[i], sep = "") )
#   }

# }











# MSE_OM2 <- new( Class     = "MSE",
#                 OM        = OM_2,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM2, file = "MSEs/MSE_OM2.Rdata")
# MSE_report( MSE_OM2, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM2_report")  

# MSE_OM3 <- new( Class     = "MSE",
#                 OM        = OM_3,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM3, file = "MSEs/MSE_OM3.Rdata")
# MSE_report( MSE_OM3, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM3_report")  

# MSE_OM4 <- new( Class     = "MSE",
#                 OM        = OM_4,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM4, file = "MSEs/MSE_OM4.Rdata")
# MSE_report( MSE_OM4, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM4_report")  

# MSE_OM5 <- new( Class     = "MSE",
#                 OM        = OM_5,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )
# MSE_report( MSE_OM5, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM5_report")  

# save(MSE_OM5, file = "MSEs/MSE_OM5.Rdata")

# MSE_OM6 <- new( Class     = "MSE",
#                 OM        = OM_6,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM6, file = "MSEs/MSE_OM6.Rdata")

# MSE_report( MSE_OM6, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM6_report")  

# MSE_OM7 <- new( Class     = "MSE",
#                 OM        = OM_7,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM7, file = "MSEs/MSE_OM7.Rdata")
# MSE_report( MSE_OM7, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM7_report")  


# MSE_OM8 <- new( Class     = "MSE",
#                 OM        = OM_8,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM8, file = "MSEs/MSE_OM8.Rdata")
# MSE_report( MSE_OM8, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM8_report")  


# MSE_OM9 <- new( Class     = "MSE",
#                 OM        = OM_9,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM9, file = "MSEs/MSE_OM9.Rdata")
# MSE_report( MSE_OM9, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM9_report")  

# MSE_OM10 <- new( Class     = "MSE",
#                 OM        = OM_10,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM10, file = "MSEs/MSE_OM10.Rdata")
# MSE_report( MSE_OM10, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM10_report")  

# MSE_OM11 <- new( Class     = "MSE",
#                 OM        = OM_11,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM11, file = "MSEs/MSE_OM11.Rdata")
# MSE_report( MSE_OM11, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM11_report")  

# MSE_OM12 <- new( Class     = "MSE",
#                 OM        = OM_12,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )
# MSE_report( MSE_OM12, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM12_report")  

# save(MSE_OM12, file = "MSEs/MSE_OM12.Rdata")

# MSE_OM13 <- new( Class     = "MSE",
#                 OM        = OM_13,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM13, file = "MSEs/MSE_OM13.Rdata")
# MSE_report( MSE_OM13, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM13_report")  

# MSE_OM14 <- new( Class     = "MSE",
#                 OM        = OM_14,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM14, file = "MSEs/MSE_OM14.Rdata")
# MSE_report( MSE_OM14, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM14_report")  

# MSE_OM15 <- new( Class     = "MSE",
#                 OM        = OM_15,
#                 Obs       = Perfect_Obs,
#                 MPs       = testMPs,
#                 interval  = 3,
#                 IE        = "Overage_10" )

# save(MSE_OM15, file = "MSEs/MSE_OM15.Rdata")
# MSE_report( MSE_OM15, dir=file.path(getwd(),"MSEs"), 
#             Author='Landmark Fisheries Research', 
#             introtext="Multi-model delay difference assessment", 
#             filenam="MSE_OM15_report")  

