# --------------------------------------------------------------------------
# initTest.R
# 
# Script for loading packages for coastwide multispecies hierarchical
# assessment simulation estimation procedure
# 
# Author: Samuel D N Johnson
# Date: October 19, 2016
#
# --------------------------------------------------------------------------

# Load packages
cranPackages <- c("coda",
                  "tidyverse",
                  "reshape2",
                  "ggforce",
                  "ggplot2",
                  "GGally",
                  "TMB",
                  "RColorBrewer",
                  "parallel",
                  "stringr",
                  "wesanderson",
                  "scales",
                  "beepr",
                  "tmbstan",
                  "bookdown",
                  "kableExtra" ,
                  "ggridges",
                  "here")

for( pkg in cranPackages )
  while(!require(pkg, character.only = TRUE) )
    install.packages( pkg, repos = "https://mirror.its.sfu.ca/mirror/CRAN/" )


githubPackages <- c(  ggsidekick  = "seananderson/ggsidekick" )

for( pkgIdx in 1:length(githubPackages) )
  while(!require(names(githubPackages)[pkgIdx], character.only = TRUE))
    devtools::install_github(githubPackages[pkgIdx])


# Re-install local version of ABTMSE
devtools::install_local("../ABTMSE/", type = "source" )

library("ABTMSE")
loadABT()
