# SWO_fixedHRMPs.R

source("constU.R")

makeGridEA_gamma <- function( eGamma = seq(from = 0.1, to = 0.9, by = 0.2),
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

makeGridEA_targ <- function(  eTarg = seq(from = 0.1, to = 0.9, by = 0.2),
                              wTarg = seq(from = .1, to = .9, by = .2),
                              eGamma = 1,
                              wGamma = 1,
                              outFile = "autoEAgridMPs.R")
{ 
  cat("# Automatically generated EA MPs over E/W Gamma grid \n", file = outFile)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)

  


  for( eIdx in 1:length(eTarg) )
  {
    eastTarg <- eTarg[eIdx]

    mpName <- paste0("EA_1_E_targ",eastTarg)

    cat("# eTarg = ", eastTarg, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- EA_1_E \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$Targ <- ", eastTarg, "\n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$Gamma <- ", eGamma, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
  
  }

  for( wIdx in 1:length(wTarg) )
  {
    westTarg <- wTarg[wIdx]

    mpName <- paste0("EA_1_W_targ",westTarg)

    cat("# wTarg = ", westTarg, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- EA_1_W \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$Targ <- ", westTarg, "\n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$Gamma <- ", wGamma, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
  
  }

  cat("# End automatically generated q grid for EA MPs \n", sep = "", file = outFile, append = TRUE)


  # Now make the list of CMPs to be tested by the MSE
  # function
  targ.list <- list( eTarg = eTarg, wTarg = wTarg )

  targ.exp <- expand.grid(targ.list)

  cat("# Full grid of pairs of eTarg and wTarg \n", file = outFile, append = TRUE,sep = "")
  nMPs <- nrow(targ.exp)
  for( k in 1:nMPs)
  {
    mpNameE <- paste0("EA_1_E_targ",targ.exp$eTarg[k])
    mpNameW <- paste0("EA_1_W_targ",targ.exp$wTarg[k])
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




