# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# MPs.R
#
# Wrappers for the empMMMP() function for different
# MP settings (lo/hi caps, assuming F is 2/3 of M etc.)
#
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

checkMP <- TRUE

# MP_testMean - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West,
# TACs are averaged over 5 OMs with even weighting,
# and checktables are produced for MP development
empMPtest_Mean <- function( x, dset, AS )
{
  TAC <- empMMMP( x       = x,
                  dset    = dset,
                  OMs     = c(1,2,4,7,11),
                  caps    = c(Inf,Inf),
                  TACrule = "mean",
                  AS      = AS,
                  check   = checkMP,
                  mpName  = "empMPtest_Mean"  )
  return(TAC)
}
class(empMPtest_Mean)<-"MSMP"

# MP_testAIC - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West
# TACs are averaged over 5 OMs with AIC weighting,
# and checktables are produced for MP development
empMPtest_min <- function( x, dset, AS )
{
  TAC <- empMMMP( x       = x,
                  dset    = dset,
                  OMs     = c(1,2,4,7,11),
                  caps    = c(Inf,Inf),
                  TACrule = "min",
                  AS      = AS,
                  check   = checkMP,
                  mpName  = "empMPtest_min"  )
  return(TAC)
}
class(empMPtest_min)<-"MSMP"

# MP_msyCap - a catch cap at estimated
# MSY is applied
empMP_msyCap <- function( x, dset, AS )
{
  TAC <- empMMMP( x       = x,
                  dset    = dset,
                  OMs     = c(1,2,4,7,11),
                  caps    = c(Inf,Inf),
                  TACrule = "mean",
                  check   = checkMP,
                  AS      = AS,
                  mpName  = "empMP_msyCap"  )
  return(TAC)
}
class(empMP_msyCap)<-"MSMP"


# MP_msyCap - a catch cap at estimated
# MSY is applied
empMP_msyCap.4B0 <- function( x, dset, AS )
{
  TAC <- empMMMP( x       = x,
                  dset    = dset,
                  OMs     = c(1,2,4,7,11),
                  caps    = c(Inf,Inf),
                  TACrule = "mean",
                  AS      = AS,
                  check   = checkMP,
                  UCP     = ".4B0",
                  mpName  = "empMP_msyCap.4B0"  )
  return(TAC)
}
class(empMP_msyCap.4B0)<-"MSMP"


# MP_loCap - a low catch cap is applied,
# of 20 kt in the East, and 2.5 kt in the West
empMP_loCap <- function( x, dset, AS )
{
  TAC <- empMMMP( x       = x,
                  dset    = dset,
                  OMs     = c(1,2,4,7,11),
                  caps    = c(20,2.5),
                  TACrule = "mean",
                  AS      = AS,
                  check   = checkMP,
                  mpName  = "empMP_loCap"  )
  return(TAC)
}
class(empMP_loCap)<-"MSMP"

# MP_hiCap - a higher catch cap is applied,
# of 25 kt in the East, and 4.0 kt in the West
empMP_hiCap <- function( x, dset, AS )
{
  TAC <- empMMMP( x       = x,
                  dset    = dset,
                  OMs     = c(1,2,4,7,11),
                  caps    = c(25,4),
                  TACrule = "mean",
                  AS      = AS,
                  check   = checkMP,
                  mpName  = "empMP_hiCap" )
  return(TAC)
}
class(empMP_hiCap)<-"MSMP"


# MP_loCap.4B0 - Same as loCap_, but
# uses a proxy of .4B0 for Bmsy in the MP, rather 
# than the estimated value from the reference points
# calc
empMP_loCap.4B0 <- function( x, dset, AS )
{
  TAC <- empMMMP( x       = x,
                  dset    = dset,
                  OMs     = c(1,2,4,7,11),
                  caps    = c(20,2.5),
                  TACrule = "mean",
                  AS      = AS,
                  check   = checkMP,
                  UCP     = ".4B0",
                  mpName  = "empMP_loCap.4B0" )

  return(TAC)
}
class(empMP_loCap.4B0)<-"MSMP"

# MP_hiCap.4B0 - Same as hiCap, but
# uses a proxy of .4B0 for Bmsy in the MP, rather 
# than the estimated value from the reference points
# calc
empMP_hiCap.4B0 <- function( x, dset, AS )
{
  TAC <- empMMMP( x       = x,
                  dset    = dset,
                  OMs     = c(1,2,4,7,11),
                  caps    = c(25,4),
                  TACrule = "mean",
                  AS      = AS,
                  check   = checkMP,
                  UCP     = ".4B0",
                  mpName  = "empMP_loCap.4B0" )

  return(TAC)
}
class(empMP_loCap.4B0)<-"MSMP"


# runCMPtest()
# Wrapper function for the new("MSE")
# call. Allows for lapply functions
# to be called on a list of OMs. Needed
# to deploy to servers so we can set and forget
# Inputs:
#   OM = character of OM name 
#         (e.g. "OM_1","OM_1d" or "ROM_1")
#   MPs = a list of character 2-ples of MP names (E/W)
#   assessInt = integer of assessment intervals, required
#               for plotting purposes
runCMPtest <- function( OM = "OM_1d",
                        MPs = list( test = c("MP_testMean","MP_testMean") ),
                        assessInt = 2 )
{
  library(ABTMSE)
  library(TMB)

  source("empiricalMP.R")
  source("MPs.R")
  # Load ABT objects in this environment
  loadABT()

  checkMP <- TRUE

  # Clear outTables directory
  outTableFiles <- list.files("./outTables", full.names = TRUE)
  if( length(outTableFiles) > 0)
    unlink(outTableFiles)


  # get OM as an environment variable from
  # the char vector label
  OMobj <- get(OM)

  # Count MPs
  nMPs  <- length(MPs)

  # Run MSE
  MSEobj <- new(  Class     = "MSE",
                  OM        = OMobj,
                  MPs       = MPs,
                  interval  = assessInt )

  # Assign MSE to a symbol that is descriptive
  MSEsymbol <- paste("MSEtest_",OM,sep = "")

  assign( x = paste("MSEtest_",OM,sep = ""),
          value = MSEobj )

  # Save that MSE
  save( list = MSEsymbol, file = file.path("MSEs",paste(MSEsymbol,".Rdata",sep = "")) )  
  # Create the report
  MSE_report( MSEobj, 
              dir=file.path(getwd(),"MSEs"), 
              Author='Landmark Fisheries Research', 
              introtext=paste("Multi-model delay difference assessment on", OM,sep =""), 
              filenam=paste(MSEsymbol,"_report",sep = ""))  

  # Collect the checkTables
  outTableFiles <- list.files("./outTables", full.names = TRUE)
  nSims <- dim(MSEobj@SSB)[2]

  westCheckTableFiles <- outTableFiles[grepl(x = outTableFiles, pattern = "West") ]
  eastCheckTableFiles <- outTableFiles[grepl(x = outTableFiles, pattern = "East") ]

  westCheckTables <- lapply(  X = westCheckTableFiles, FUN = read.csv,
                              header = TRUE, stringsAsFactors = FALSE )

  eastCheckTables <- lapply(  X = eastCheckTableFiles, FUN = read.csv,
                              header = TRUE, stringsAsFactors = FALSE )

  if(length(westCheckTables) != nSims | length(eastCheckTables) != nSims )
    browser(cat("Wrong number of checkTables") )


  # Save to output directory
  if(!dir.exists("MSEs/fitCheck"))
    dir.create("MSEs/fitCheck")

  if(!dir.exists(file.path("MSEs/fitCheck",OM)))
    dir.create(file.path("MSEs/fitCheck",OM))

  checkTablesSavePath <- file.path("MSEs/fitCheck",OM,"checkTables.Rdata")

  ewCheckTableList <- list( east = eastCheckTables, west = westCheckTables)

  save( ewCheckTableList, 
        file = checkTablesSavePath )

  for( i in 1:nSims )
  {
    for( j in 1:nMPs )
    {
      MPid <- names(MPs)[j]
      # Create a directory for the MP if it doesn't exist
      if( !dir.exists(file.path("MSEs/fitCheck",OM,MPid)) )
        dir.create(file.path("MSEs/fitCheck",OM,MPid))
      
      fitCheckPlot <- paste("fitCheck_sim",i,"_",OM,"_",MPid,".png",sep = "")
      png(  filename = file.path("MSEs/fitCheck",OM,MPid,fitCheckPlot),
            width = 8.5, height = 11, units = "in", res = 300 )
      plot_TACperformance(  simNum      = i,
                            MSEobj      = MSEobj,
                            westTables  = westCheckTables,
                            eastTables  = eastCheckTables,
                            MPlist      = MPs,
                            MPidx       = j,
                            interval    = assessInt )
      dev.off()
    }
    # Remove checkTables for next run
    system( paste("rm -r ", outTableFiles[i], sep = "") )
  }
  return(MSEobj)
}

# runCMP()
# Wrapper function for the new("MSE")
# call. Allows for lapply functions
# to be called on a list of OMs. Needed
# to deploy to servers so we can set and forget.
# Basically the same as runCMPtest() but
# doesn't plot fit checks
# Inputs:
#   OM = character of OM name 
#         (e.g. "OM_1","OM_1d" or "ROM_1")
#   MPs = a list of character 2-ples of MP names (E/W)
#   assessInt = integer of assessment intervals, required
#               for plotting purposes
runCMPs <- function(  OM = "OM_1",
                      MPs = list( test = c("MP_loCap","MP_loCap") ),
                      assessInt = 2,
                      report = TRUE,
                      saveObj = TRUE )
{
  library(ABTMSE)
  library(TMB)

  source("empiricalMP.R")
  source("MPs.R")

  # Load ABT objects in this environment
  loadABT()

  OMobj <- get(OM)

  checkMP <- FALSE

  # Run MSE
  MSEobj <- new(  Class     = "MSE",
                  OM        = OMobj,
                  MPs       = MPs,
                  interval  = assessInt )

  # Assign MSE to a symbol that is descriptive
  MSEsymbol <- paste("MSE_",OM,sep = "")

  assign( x = paste("MSE_",OM,sep = ""),
          value = MSEobj )

  # Save that MSE
  if( saveObj )
    save( list = MSEsymbol, file = file.path("MSEs",paste(MSEsymbol,".Rdata",sep = "")) )  
  # Create the report
  if( report )
    MSE_report( MSEobj, 
                dir=file.path(getwd(),"MSEs"), 
                Author='Landmark Fisheries Research', 
                introtext=paste("Multi-model delay difference assessment on", OM,sep =""), 
                filenam=paste(MSEsymbol,"_report",sep = ""))  

  return(MSEobj)
}

# runCMP()
# Wrapper function for the new("MSE")
# call. Allows for lapply functions
# to be called on a list of OMs. Needed
# to deploy to servers so we can set and forget.
# Basically the same as runCMPtest() but
# doesn't plot fit checks
# Inputs:
#   OM = character of OM name 
#         (e.g. "OM_1","OM_1d" or "ROM_1")
#   MPs = a list of character 2-ples of MP names (E/W)
#   assessInt = integer of assessment intervals, required
#               for plotting purposes
runCMPs <- function(  OM = "OM_1",
                      MPs = list( test = c("MP_loCap","MP_loCap") ),
                      assessInt = 2,
                      report = TRUE,
                      saveObj = TRUE )
{
  library(ABTMSE)
  library(TMB)

  source("empiricalMP.R")
  source("MPs.R")

  # Load ABT objects in this environment
  loadABT()

  OMobj <- get(OM)

  checkMP <- FALSE

  # Run MSE
  MSEobj <- new(  Class     = "MSE",
                  OM        = OMobj,
                  MPs       = MPs,
                  interval  = assessInt )

  # Assign MSE to a symbol that is descriptive
  MSEsymbol <- paste("MSE_",OM,sep = "")

  assign( x = paste("MSE_",OM,sep = ""),
          value = MSEobj )

  # Save that MSE
  if( saveObj )
    save( list = MSEsymbol, file = file.path("MSEs",paste(MSEsymbol,".Rdata",sep = "")) )  
  # Create the report
  if( report )
    MSE_report( MSEobj, 
                dir=file.path(getwd(),"MSEs"), 
                Author='Landmark Fisheries Research', 
                introtext=paste("Multi-model delay difference assessment on", OM,sep =""), 
                filenam=paste(MSEsymbol,"_report",sep = ""))  

  return(MSEobj)
}

getBr30 <- function( MSEobj, qProbs = c(0.05,0.5,0.95) )
{
  B_BMSY <- MSEobj@B_BMSY[,,,82]

  Bquants <- apply( X = B_BMSY, FUN = quantile,
                    MARGIN = c(1,3), probs = qProbs )

  dimnames( Bquants )[[2]] <- names(MSEobj@MPs)
  dimnames( Bquants )[[2]][1] <- "ZeroC"
  dimnames( Bquants )[[3]] <- c("East","West")

  return(Bquants)

}

getBr30Grad <- function( MSEobj )
{
  B_BMSY <- MSEobj@B_BMSY[,,,81:83]

  nMPs  <- dim(B_BMSY)[1]

  dimnames(B_BMSY) <- list( MPs = c(names(MSEobj@MPs)),
                            simNum = paste("sim",1:dim(B_BMSY)[2],sep = ""),
                            stock = c("East","West"),
                            tStep = 81:83 )
  dimnames(B_BMSY)[[1]][1] <- "ZeroC"

  Bquants <- apply( X = B_BMSY, FUN = quantile,
                    MARGIN = c(1,3,4), probs = c(0.05, 0.5, 0.95) )

  # Now smooth the median and calculate the 
  # derivative there
  Bmed <- Bquants[2,,,]

  gradB30 <- array( NA, dim = dim(Bmed)[1:2],
                        dimnames = dimnames(Bmed)[1:2] )

  for( mpIdx in 1:nMPs )
  {
    medB_East <- Bmed[mpIdx,"East",]
    medB_West <- Bmed[mpIdx,"West",]

    # Fit a spline
    medB_East_spline <- splinefun( x = 81:83, y = medB_East )
    medB_West_spline <- splinefun( x = 81:83, y = medB_West )

    gradB30[mpIdx,"East"] <- medB_East_spline(82, deriv = 1)
    gradB30[mpIdx,"West"] <- medB_West_spline(82, deriv = 1)
  }



  return(gradB30)

}

getCquants <- function( MSEobj, tIdx = 53:62 )
{
  CWa <- MSEobj@CWa[,,,tIdx]/1e6

  Cmean <- apply( X = CWa, FUN = mean,
                      MARGIN = c(1,2,3) )

  Cquants <- apply( X = Cmean, FUN = quantile,
                      MARGIN = c(1,3), probs = c(0.05, 0.5, 0.95) )

  dimnames( Cquants )[[2]] <- names(MSEobj@MPs)
  dimnames( Cquants )[[2]][1] <- "ZeroC"

  return(Cquants)
}

getMPnames <- function( MSEobj )
{
  MPnames <- names(MSEobj@MPs)
  MPnames[1] <- "ZeroC"

  MPnames
}

# calcObjectiveFunction()
# Takes the output from runCMP (an MSE object)
# and calculates an objective function value
# for MP tuning
calcPerfMetrics <- function(  MSElist )
{
  # Calculate some performance metrics
  B30quants <- sapply( X = MSElist, FUN = getBr30, simplify = "array" )
  B30grad   <- sapply( X = MSElist, FUN = getBr30Grad, simplify = "array" )

  # C10quants <- sapply( X = MSElist, FUN = getCquants, simplify = "array", tIdx = 53:62 )
  # C20quants <- sapply( X = MSElist, FUN = getCquants, simplify = "array", tIdx = 63:72 )
  # C30quants <- sapply( X = MSElist, FUN = getCquants, simplify = "array", tIdx = 73:82 )
  
  # Get MP names
  MPnames <- lapply( X = MSElist, FUN = getMPnames )

  # Pull median B30 value and derivative of median
  B30med <- B30quants[2,,,]
  B30der <- B30grad 




  # return perf metrics
  outList <- list(  B30med = B30med,
                    B30der = B30der )

  outList
}


wtdEmpMPevalFun <- function(  wts = rnorm(8),
                              delta = rnorm(8),
                              c = 0.5,
                              OMvec = paste( "OM_", 1:3,"d",sep = "" ),
                              par = FALSE,
                              nCores = parallel::detectCores()-1 )
{
  # Assign the weights as a global variable
  OMwts <<- wts

  # Define weighted MP
  wtdMP <<- function( x, dset, AS, weights = OMwts )
  {
    empMMMP(  x = x, 
              dset = dset, 
              OMs           = c(1,2,4,5,7,8,10,11),
              caps          = c(Inf,Inf),
              UCP           = "Bmsy",
              TACrule       = c("weighted"),
              wts           = weights,
              check         = FALSE,
              AS            = 1,
              maxDeltaTACup = 0.2,
              maxDeltaTACdn = 0.5,
              propW         = c(.102,.9),
              mpName        = "wtdMP" )
  }
  class(wtdMP) <<- "MSMP"

  # Define weighted MP
  wtdMPup <<- function( x, dset, AS, weights = OMwts + c * delta )
  {
    empMMMP(  x = x, 
              dset = dset, 
              OMs           = c(1,2,4,5,7,8,10,11),
              caps          = c(Inf,Inf),
              UCP           = "Bmsy",
              TACrule       = c("weighted"),
              wts           = weights,
              check         = FALSE,
              AS            = 1,
              maxDeltaTACup = 0.2,
              maxDeltaTACdn = 0.5,
              propW         = c(.102,.9),
              mpName        = "wtdMP" )
  }
  class(wtdMPup) <<- "MSMP"

  # Define weighted MP
  wtdMPdn <<- function( x, dset, AS, weights = OMwts - c * delta )
  {
    empMMMP(  x = x, 
              dset = dset, 
              OMs           = c(1,2,4,5,7,8,10,11),
              caps          = c(Inf,Inf),
              UCP           = "Bmsy",
              TACrule       = c("weighted"),
              wts           = weights,
              check         = FALSE,
              AS            = 1,
              maxDeltaTACup = 0.2,
              maxDeltaTACdn = 0.5,
              propW         = c(.102,.9),
              mpName        = "wtdMP" )
  }
  class(wtdMPdn) <<- "MSMP"

  # Define pars for functionalised application
  MPs <- list(  wtdMP = c("wtdMP","wtdMP"),
                wtdMPup = c("wtdMPup","wtdMPup"),
                wtdMPdn = c("wtdMPdn","wtdMPdn"))

  if( par )
  {
    library(parallel)
    clust <- makeCluster( nCores )

    parVarList <- c(  "wtdMP",
                      "wtdMPup", 
                      "wtdMPdn", 
                      "MSE_report",
                      "wts",
                      "delta",
                      "c",
                      "OMwts")

    parallel::clusterExport( cl = clust, varlist = parVarList,
                              envir = environment() )

    message("Running ", length(OMvec), " OMs on ", nCores, " cores.\n", sep = "" )



    MSElist <- clusterApplyLB(  cl = clust,
                                x = OMvec,
                                fun = runCMPs,
                                assessInt = 2,
                                MPs = MPs,
                                report = FALSE,
                                saveObj = FALSE )

    stopCluster(clust)
  } else {
    MSElist <- lapply(  X = OMvec, 
                        FUN = runCMPs,
                        assessInt = 2,
                        MPs = MPs,
                        report = FALSE,
                        saveObj = FALSE )
  }

  return(MSElist)
}



# runSPSA_MPoptim()
# A Simultaneous Perturbations Stochastic Approximation
# algorithm for tuning our empirical MP.
# inputs: Niter = number of iterations
#         c = initial finite difference (FD) width
#         a = initial gain parameter numerator
#         A = gain parameter denominator (speed)
#         gamma = FD width exponent
#         alpha = gain sequence exponent
#         OMvec = vector of OMs,
#         nCores = number of core for parallel usage
# Reference: Spall, 2003, Intro to Stochastic Search 
#             and Optimisation
# Source: SDN Johnson, September 10, 2019
runSPSA_MPoptim <- function(  Niter = 10,
                              c = 0.1,
                              a = 0.01,
                              gamma = 0.101,
                              alpha = 0.602,
                              OMs = c(1,2,4,5,7,8,10,11),
                              nCores = 3,
                              seed = 1234,
                              par = FALSE )
{
  # Generate vectors of gain parameters
  A <- 0.1 * Niter

  # Will need to add an approximation for the a parameter here...

  # Gain parameters a_k and c_k
  a_k <- a / ((1:Niter) + 1 + A)^(alpha)
  c_k <- c / (1:Niter + 1)^gamma

  # Number of loss function evaluations is
  # actually Niter * 3

  # indices:
  # k = iteration 0,...,Niter (initWts are k = 0)
  # p = dimension of weight space (P = 8)
  # i = Operating model (loss function), i = 1,..,len(OMvec)
  # d = number of loss function evals required per iter (3)

  nOMs <- length(OMs)
  nCores <- min( nCores, nOMs, detectCores()-1 )
  trainOMvec <- paste( "OM_", OMs,"d",sep = "" )

  # Arrays to hold history of optimisation
  Q_kid <-  array( NA,  dim = c(  Niter + 1, nOMs, 3),
                        dimnames = list(  iter = paste("iter",0:Niter,sep = ""),
                                          OM = trainOMvec, 
                                          evaln = c("wk","wkup","wkdn") ) )

  wts_kp <- array(NA, dim = c(Niter + 1,8),
                      dimnames = list(  iter = paste("iter",0:Niter,sep = ""),
                                        wDim = paste("wtDim",1:8,sep = "") ) )

  grad_kpi <- array(NA, dim = c( Niter + 1, 8, nOMs),
                        dimnames = list(  iter = paste("iter",0:Niter,sep = ""),
                                          wDim = paste("wtDim",1:8,sep = ""),
                                          OM = trainOMvec ) )

  # Set initial seed
  if(!is.null(seed))
    set.seed(seed)

  # Draw random initial weights
  initWts <- rnorm(8, sd = 0.1 )

  initWts <- log( exp(initWts) / sum(exp(initWts)) )

  # Draw a matrix of random perturbations
  # for each iteration.
  delta_kp <- matrix( rnorm(8 * Niter, sd = 0.1), 
                      nrow = Niter, ncol = 8 )


  MSEobjs <- vector( mode = "list", length = Niter)

  

  # fill wts_kp[1,] with initWts
  wts_kp[1,] <- initWts

  # Loop over k, evaluate loss functions
  for( k in 1:Niter )
  {
    # First, calculate loss functions with
    # the w_k
    MSElist <- wtdEmpMPevalFun( wts = wts_kp[k,],
                                delta = delta_kp[k,],
                                c = c_k[k],
                                OMvec = trainOMvec,
                                par = par,
                                nCores = nCores )
    # Save MSE objects
    MSEobjs[[k]] <- MSElist

    perfMetrics <- calcPerfMetrics(MSElist)

    # Sum perf metrics to get squared loss
    squaredLoss_area <- log(perfMetrics$B30med)^2 + (perfMetrics$B30der)^2

    # And sum over areas
    squaredLoss_tot <- apply( X = squaredLoss_area, FUN = sum,
                              MARGIN = c(1,3) )[c("wtdMP","wtdMPup","wtdMPdn"),]

    Q_kid[k,,] <- t(squaredLoss_tot)

    # This gives an MP * OM array. Save for
    # loss functions and compute the central diff
    # gradient
    for( p in 1:8 )
    {
      grad_kpi[k,p,] <- (Q_kid[k,,"wkup"] - Q_kid[k,,"wkdn"] ) / 2 / c_k[k] / delta_kp[k,p]
    }

    # Now calculate the new weights by stepping
    # in the opposite direction of the average 
    # gradient over the OMs, with gain a_k
    wts_kp[k+1,] <- wts_kp[k,] - a_k[k] * apply(X = grad_kpi[k,,], FUN = mean, MARGIN = 1 )

    wts_kp[k+1,] <- log( exp(wts_kp[k+1,]) / sum(exp(wts_kp[k+1,])) )

    # Now average successive weights to reduce variability
    wts_kp[k+1,] <- 0.5 * (wts_kp[k+1,] + wts_kp[k,] )

    gc()
  }

  finalOMvec <- paste( "OM_", OMs,sep = "" )

  # Now run the MSE over OMvec with 
  # the final weighting
  finalMSE <- wtdEmpMPevalFun(  wts = wts_kp[Niter+1,],
                                delta = rep(0,8),
                                c = 0,
                                OMvec = finalOMvec,
                                par = par,
                                nCores = nCores )

  # next: create tables of optimisation performance - save
  # final step in Q and grad.

  outList <- list(  finalMSE = finalMSE,
                    MSEobjs = MSEobjs,
                    Q_kid = Q_kid,
                    wts_kp = wts_kp,
                    grad_kpi = grad_kpi,
                    delta_kp = delta_kp )


  return(outList)

}

