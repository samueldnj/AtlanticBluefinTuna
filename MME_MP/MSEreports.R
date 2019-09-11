# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
#
# MSEreports.R
#
# Reads in MSE objects from the ./MSE folder and
# creates reports for each one.
#
# Author: Samuel Johnson
#
#
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(ABTMSE)
library(stringr)
loadABT()

options(warn = -1)

# First, read in MSE folder contents
MSEpath   <- file.path("./MSEs",MSEs)
nMSEs     <- length(MSEs)

# Load the MSE object and create report
load(MSEpath[1])

MSE_report( MSE_OM1, dir=file.path(getwd(),"MSEs"), 
            Author='Landmark Fisheries Research', 
            introtext="Multi-model delay difference assessment", 
            filenam="MSE_OM1_report")  


# Load the MSE object and create report
load(MSEpath[2])

MSE_report( MSE_OM2, dir=file.path(getwd(),"MSEs"), 
            Author='Landmark Fisheries Research', 
            introtext="Multi-model delay difference assessment", 
            filenam="MSE_OM2_report")  

# Load the MSE object and create report
load(MSEpath[3])

MSE_report( MSE_OM3, dir=file.path(getwd(),"MSEs"), 
            Author='Landmark Fisheries Research', 
            introtext="Multi-model delay difference assessment", 
            filenam="MSE_OM3_report")  

# Load the MSE object and create report
load(MSEpath[4])

MSE_report( MSE_OM4, dir=file.path(getwd(),"MSEs"), 
            Author='Landmark Fisheries Research', 
            introtext="Multi-model delay difference assessment", 
            filenam="MSE_OM4_report")  

# Load the MSE object and create report
load(MSEpath[5])

MSE_report( MSE_OM5, dir=file.path(getwd(),"MSEs"), 
            Author='Landmark Fisheries Research', 
            introtext="Multi-model delay difference assessment", 
            filenam="MSE_OM5_report")  


# Load the MSE object and create report
load(MSEpath[6])

MSE_report( MSE_OM6, dir=file.path(getwd(),"MSEs"), 
            Author='Landmark Fisheries Research', 
            introtext="Multi-model delay difference assessment", 
            filenam="MSE_OM6_report")  






