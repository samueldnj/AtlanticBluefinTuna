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
                  "stringr",
                  "wesanderson",
                  "scales",
                  "shiny",
                  "snowfall",
                  "maps",
                  "mapdata",
                  "wordcloud",
                  "abind",
                  "geoR",
                  "shinyBS",
                  "shinyWidgets",
                  "fmsb",
                  "beepr",
                  "bookdown",
                  "kableExtra",
                  "here",
                  "dtwclust")

for( pkg in cranPackages )
  while(!require(pkg, character.only = TRUE) )
    install.packages( pkg, repos = "https://muug.ca/mirror/cran/" )

library("ABTMSE")
loadABT()
