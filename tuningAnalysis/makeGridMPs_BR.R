# SWO_fixedHRMPs.R

source("BR3.R")

makeGridBR <- function( alp = seq(from = 1.7, to = 3.7, by = 0.5),
                        bet = seq(from = 0.8, to = 1.2, by = 0.1),
                        outFile = "autoBRgrid.R")
{ 
  cat("# Automatically generated BR3 MPs over q grid \n", file = outFile)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)

  for( aIdx in 1:length(alp) )
  {
    a <- alp[aIdx]

    mpName <- paste0("BR_E3_a",a)

    cat("# alp = ", a, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- BR_E3s \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$alp <- ", a, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)

  }

  for( bIdx in 1:length(bet) )
  {
    b <- bet[bIdx]
    
    mpName <- paste0("BR_W3_b",b)

    cat("# bet = ", b, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- BR_W3s \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$bet <- ", b, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)

  }

  cat("# End automatically generated alp and bet grid for BR3 MPs \n", sep = "", file = outFile, append = TRUE)


  # Now make the list of CMPs to be tested by the MSE
  # function
  ab.list <- list(  alp = alp,
                    bet = bet )

  ab.exp <- expand.grid(ab.list)

  cat("# Full grid of pairs of alp/bet values \n", file = outFile, append = TRUE,sep = "")
  nMPs <- nrow(ab.exp)
  for( k in 1:nMPs)
  {
    mpNameE <- paste0("BR_E3_a",ab.exp$alp[k])
    mpNameW <- paste0("BR_W3_b",ab.exp$bet[k])
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




