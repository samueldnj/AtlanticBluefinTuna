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
                  "TMB",
                  "RColorBrewer",
                  "parallel",
                  "stringr",
                  "reshape2",
                  "wesanderson",
                  "scales",
                  "beepr",
                  "bookdown",
                  "kableExtra" ,
                  "here",
                  "fields")

for( pkg in cranPackages )
  while(!require(pkg, character.only = TRUE) )
    install.packages( pkg, repos = "https://mirror.its.sfu.ca/mirror/CRAN/" )


#githubPackages <- c(  ggsidekick  = "seananderson/ggsidekick" )
#githubPackages <- c(  ggsidekick  = "cran/SDMTools" )
#
#for( pkgIdx in 1:length(githubPackages) )
#  while(!require(names(githubPackages)[pkgIdx], character.only = TRUE))
#    devtools::install_github(githubPackages[pkgIdx])


# Re-install local version of ABTMSE
devtools::install_local("../ABTMSE/", type = "source" )
#install.packages("../ABTMSE_6.6.4.tar", repos=NULL, type="source" )

# library(ABTMSE)

