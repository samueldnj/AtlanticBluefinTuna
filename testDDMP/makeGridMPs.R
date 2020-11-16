# Function to create a list of grid based MPs
library(tidyverse)
Mmults <- seq(from = 0.1, to = 1, length.out = 5)

# Caps
eastCaps <- seq( from = 10, to = 40, length.out = 5 )
westCaps <- seq( from = 1, to = 5, length.out = 5 )

# Add Inf and MSY
eastCaps <- c(eastCaps,"msy",Inf)
westCaps <- c(westCaps,"msy",Inf)

nllMults <- c(0.01,0.025,0.05,0.1,"last10")


makeGridMPs <- function(  M = Mmults,
                          eastCap = "msy",
                          westCap = "msy",
                          nllMults = "last10",
                          MPfile = "Mgrid.R" )
{
  # Expand.grid the factors
  gridFactors <- list(  M = M, 
                        eastCap = eastCap, 
                        westCap = westCap, 
                        nllMults = nllMults )

  grid <- expand.grid(gridFactors) %>%
            dplyr::mutate_if(is.factor, as.character)


  MPnames <- c()

  # Create outFile
  cat("# Automatically generated MP grid for ABFT tuning\n", file = MPfile, append = FALSE, sep = "")
  cat("# ", MPfile ,"\n", file = MPfile, append = TRUE,sep = "")
  cat("# ", as.character(Sys.Date()) ,"\n", file = MPfile, append = TRUE,sep = "")
  cat("# \n", file = MPfile, append = TRUE,sep = "")

  # Now loop over the grid, and make the MP
  for( k in 1:nrow(grid))
  {
    Mmult <- grid$M[k]
    Ecap  <- grid$eastCap[k]
    Wcap  <- grid$westCap[k]
    nllWt  <- grid$nllMults[k]

    if(Ecap == "msy" | Wcap == "msy")
      capVec <- "c('msy','msy')"
    else
      capVec <- paste("c(",Ecap,",",Wcap,")",sep = "")

    mpName <- paste("DD_caps",Ecap,".",Wcap,"_F",Mmult,"M_",nllWt,"nll",sep = "")

    MPnames <- c(MPnames,mpName)

    if( nllWt == "last10" )
      nllWt <- "'last10'"

    cat("\n", file = MPfile, append = TRUE,sep = "")
    cat("\n", file = MPfile, append = TRUE,sep = "")
    cat(mpName," <- function( x, dset, AS)\n", file = MPfile, append = TRUE,sep = "")
    cat("{\n", file = MPfile, append = TRUE,sep = "")
    cat("  TAC <- assessDDmm( x = x,\n", file = MPfile, append = TRUE,sep = "")
    cat("                     dset = dset,\n", file = MPfile, append = TRUE,sep = "")
    cat("                     caps = ",capVec,",\n", file = MPfile, append = TRUE,sep = "")
    cat("                     FmultM = ",Mmult,",\n", file = MPfile, append = TRUE,sep = "")
    cat("                     F23M = FALSE,\n", file = MPfile, append = TRUE,sep = "")
    cat("                     AS = AS,\n", file = MPfile, append = TRUE,sep = "")
    cat("                     mpName = '",mpName,"',\n", file = MPfile, append = TRUE,sep = "")
    cat("                     nllWt =",nllWt," )\n", file = MPfile, append = TRUE,sep = "")
    cat("}\n", file = MPfile, append = TRUE,sep = "")
    cat("class(",mpName,")<-'MSMP'",sep = "",append = TRUE, file = MPfile)
    cat("\n", file = MPfile, append = TRUE,sep = "")
    cat("\n", file = MPfile, append = TRUE,sep = "")
  }

  cat("# END auto grid\n", file = MPfile, append = TRUE, sep = "")
  cat("\n", file = MPfile, append = TRUE,sep = "")

  return(MPnames)
}