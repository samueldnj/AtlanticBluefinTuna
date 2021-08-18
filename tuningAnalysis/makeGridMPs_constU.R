# SWO_fixedHRMPs.R

source("constU.R")

makeGridconstU <- function( eMult = seq(from = 1, to = 5, by = 1),
                            wMult = seq(from = 7, to = 12, by = 1),
                            outFile = "autoConstUgridMPs.R")
{ 
  cat("# Automatically generated constU MPs over E/W multiplier grid \n", file = outFile)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)

  


  for( eIdx in 1:length(eMult) )
  {
    eastMult <- eMult[eIdx]

    mpName <- paste0("constU_E_m",eastMult)

    cat("# eMult = ", eastMult, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- ConstU_E \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$multiplierE <- ", eastMult, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
  
  }

  for( wIdx in 1:length(wMult) )
  {
    westMult <- wMult[wIdx]

    mpName <- paste0("constU_W_m",westMult)

    cat("# eMult = ", westMult, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- ConstU_W \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$multiplierW <- ", westMult, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
  
  }

  cat("# End automatically generated q grid for F01 MPs \n", sep = "", file = outFile, append = TRUE)


  # Now make the list of CMPs to be tested by the MSE
  # function
  mult.list <- list( eMult = eMult, wMult = wMult )

  mult.exp <- expand.grid(mult.list)

  cat("# Full grid of pairs of eMult and wMult \n", file = outFile, append = TRUE,sep = "")
  nMPs <- nrow(mult.exp)
  for( k in 1:nMPs)
  {
    mpNameE <- paste0("constU_E_m",mult.exp$eMult[k])
    mpNameW <- paste0("constU_W_m",mult.exp$wMult[k])
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




