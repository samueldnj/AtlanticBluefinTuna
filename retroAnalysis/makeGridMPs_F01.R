# SWO_fixedHRMPs.R

source("Fzero1.R")

makeGridFzero1 <- function(   qEast = seq(from = 0.05, to = 0.75, by = 0.05),
                              qWest = seq(from = 0.05, to = 0.75, by = 0.05),
                              outFile = "autoF01gridMPs.R")
{ 
  cat("# Automatically generated F01 MPs over q grid \n", file = outFile)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)
  cat("\n", file = outFile, append = TRUE)

  for( qIdx in 1:length(qEast) )
  {
    q <- qEast[qIdx]

    mpName <- paste0("Fzero1E_q",q)

    cat("# q = ", q, "\n", file = outFile, sep = "", append = TRUE)
    cat(mpName," <- Fzero1E \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$q <- ", q, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)

  }

  for( qIdx in 1:length(qWest) )
  {
    q <- qWest[qIdx]
    
    mpName <- paste0("Fzero1W_q",q)

    cat(mpName," <- Fzero1W \n", sep = "", file = outFile, append = TRUE)
    cat("formals(",mpName,")$q <- ", q, "\n", sep = "", file = outFile, append = TRUE)
    cat("class(",mpName,") <- 'MP' \n",sep = "", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)
    cat("\n", file = outFile, append = TRUE)

  }

  cat("# End automatically generated q grid for F01 MPs \n", sep = "", file = outFile, append = TRUE)


  # Now make the list of CMPs to be tested by the MSE
  # function
  q.list <- list( qE = qEast,
                  qW = qWest )

  q.exp <- expand.grid(q.list)

  cat("# Full grid of pairs of qs \n", file = outFile, append = TRUE,sep = "")
  nMPs <- nrow(q.exp)
  for( k in 1:nMPs)
  {
    mpNameE <- paste0("Fzero1E_q",q.exp$qE[k])
    mpNameW <- paste0("Fzero1W_q",q.exp$qW[k])
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




