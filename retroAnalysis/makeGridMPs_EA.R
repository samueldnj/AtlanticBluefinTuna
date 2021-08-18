# SWO_fixedHRMPs.R

source("constU.R")

makeGridEA <- function( eGamma = seq(from = 0.1, to = 0.9, by = 0.2),
                        wGamma = seq(from = .1, to = .9, by = .2),
                            outFile = "autoEAgridMPs.R")
{ 
  cat("# Automatically generated EA MPs over E/W Gamma grid \n", file = outFile)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)

  


  for( eIdx in 1:length(eGamma) )
  {
    eastGamma <- eGamma[eIdx]

    mpName <- paste0("EA_1_E_gam",eastGamma)

    cat("# eGamma = ", eastGamma, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- EA_1_E \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$Gamma <- ", eastGamma, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
  
  }

  for( wIdx in 1:length(wGamma) )
  {
    westGamma <- wGamma[wIdx]

    mpName <- paste0("EA_1_W_gam",westGamma)

    cat("# wGamma = ", westGamma, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- EA_1_W \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$Gamma <- ", westGamma, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
  
  }

  cat("# End automatically generated q grid for EA MPs \n", sep = "", file = outFile, append = TRUE)


  # Now make the list of CMPs to be tested by the MSE
  # function
  gamma.list <- list( eGamma = eGamma, wGamma = wGamma )

  gamma.exp <- expand.grid(gamma.list)

  cat("# Full grid of pairs of eGamma and wGamma \n", file = outFile, append = TRUE,sep = "")
  nMPs <- nrow(gamma.exp)
  for( k in 1:nMPs)
  {
    mpNameE <- paste0("EA_1_E_gam",gamma.exp$eGamma[k])
    mpNameW <- paste0("EA_1_W_gam",gamma.exp$wGamma[k])
    if(k == 1)
      cat("gridMPs <- list(c('", mpNameE,"', '",mpNameW,"'),\n", file = outFile, append = TRUE,sep = "")

    if(  k == nMPs )
      cat("                c('", mpNameE,"', '",mpNameW,"'))\n", file = outFile, append = TRUE,sep = "")

    if( (k > 1 & k < nMPs)  )
      cat("                c('", mpNameE,"', '",mpNameW,"'),\n", file = outFile, append = TRUE,sep = "")

  }

  cat("# END auto grid MP list\n", file = outFile, append = TRUE, sep = "")
  cat("\n", file = outFile, append = TRUE,sep = "")

} # END makeGridHRMPs




