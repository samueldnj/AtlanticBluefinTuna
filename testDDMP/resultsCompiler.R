#ResC <- function (dir = NULL, name = NULL, CMPdesc = NULL) 
{
    dir <- "."
    name <- "LFR-DelayDiff"
    CMPdesc <- "East:No cap, Fmax=M; West:MSY cap, Fmax=M*2/3"

    if (!is.null(dir)) 
        setwd(dir)
    files <- list.files()
    isMSE <- grepl("MSE_", files)
    MSEref <- c(paste0("MSE_", 1:96), paste0("MSE_R_", 1:12))
    nOMs <- length(MSEref)
    fileref <- paste0(MSEref, ".rda")
    MSEtab <- data.frame(MSE = MSEref, Available = fileref %in% 
        files)
    if (sum(isMSE) != 108) {
        message("You currently have ", sum(isMSE), " MSE objects in this folder. You should have ", 
            nOMs, " including all reference OMs and robustness OMs:")
        return(MSEtab)
    }
    if (sum(MSEtab$Available) != nrow(MSEtab)) {
        message("You do not have all the required MSE objects:")
        print(MSEtab)
        return(MSEtab)
    }
    MSEobj <- readRDS(fileref[1])
    MSEnames <- rep(NA, nOMs)
    perf <- getperf(MSEobj)
    pnames <- names(perf[[1]])
    pnames <- pnames[!pnames %in% c("POF", "PGK")]
    nmet <- length(pnames)
    nMPs <- MSEobj@nMPs
    nsim <- MSEobj@nsim
    nyears <- MSEobj@nyears + MSEobj@proyears
    message("MSE_1.rda has ", nsim, " simulations and ", nMPs, 
        " candidate management procedures (including the Zero catch reference CMP)")
    MET <- array(NA, c(nsim, nOMs, 2, nMPs, nmet))
    dynSSB0 <- dynSSBMSY <- array(NA, c(nsim, nOMs, 2, nyears))
    R0 <- SSB0 <- array(NA, c(nOMs, 2, nyears))
    Rec <- CW <- CWa <- B_BMSY <- F_FMSY <- array(NA, c(nsim, 
        nOMs, 2, nMPs, nyears))
    for (OM in 1:nOMs) {
        MSEobj <- readRDS(fileref[OM])
        test <- strsplit(MSEobj@Name, "/")[[1]]
        MSEnames[OM] <- paste0(test[length(test) - (1:0)], collapse = "")
        for (pp in 1:2) {
            for (mt in 1:nmet) {
                MET[, OM, pp, , mt] <- t(do.call(pnames[mt], 
                  list(MSE = MSEobj, pp = pp)))
            }
        }
        dynSSB0[, OM, , ] <- abind(MSEobj@dynB0h, MSEobj@dynB0)
        dynSSBMSY[, OM, , ] <- abind(MSEobj@dynB0h * array(MSEobj@SSBMSY_SSB0, 
            dim(MSEobj@dynB0h)), MSEobj@dynB0 * array(MSEobj@SSBMSY_SSB0, 
            dim(MSEobj@dynB0)))
        SSB0[OM, , ] <- MSEobj@R0_proj * MSEobj@SSBpR[1, ]
        R0[OM, , ] <- MSEobj@R0_proj
        Rec[, OM, , , ] <- aperm(MSEobj@Rec_mu, c(2, 3, 1, 4))
        CW[, OM, , , ] <- aperm(MSEobj@CW, c(2, 3, 1, 4))
        CWa[, OM, , , ] <- aperm(MSEobj@CWa, c(2, 3, 1, 4))
        B_BMSY[, OM, , , ] <- aperm(MSEobj@B_BMSY, c(2, 3, 1, 
            4))
        F_FMSY[, OM, , , ] <- aperm(MSEobj@F_FMSY, c(2, 3, 1, 
            4))
        print(paste(OM, "/", nOMs, "MSE objects (OMs)"))
    }
    OMcheck <- c(paste0("OMs", 1:96), paste0("ROMs", 1:12))
    if (sum(MSEnames %in% OMcheck) != nOMs) {
        message("The names of the MSE objects should correspond with the reference OMs and robustness OMs in the ABTMSE package. Currently they do not match:")
        diag <- data.frame(Needed = OMcheck, Available = MSEnames %in% 
            OMcheck)
        print(diag)
        return(diag)
    }
    if (any(is.na(MET))) {
        message("Warning: there are NA values in the summary performance metrics")
    }
    Rd2list <- function(Rd) {
        names(Rd) <- substring(sapply(Rd, attr, "Rd_tag"), 2)
        temp_args <- Rd$arguments
        Rd$arguments <- NULL
        myrd <- lapply(Rd, unlist)
        myrd <- lapply(myrd, paste, collapse = "")
        temp_args <- temp_args[sapply(temp_args, attr, "Rd_tag") == 
            "\\item"]
        temp_args <- lapply(temp_args, lapply, paste, collapse = "")
        temp_args <- lapply(temp_args, "names<-", c("arg", "description"))
        myrd$arguments <- temp_args
        return(myrd)
    }
    getHelpList <- function(...) {
        thefile <- help(...)
        myrd <- utils:::.getHelpFile(thefile)
        Rd2list(myrd)
    }
    pdesc <- rep(NA, length(pnames))
    for (pp in 1:length(pnames)) {
        x <- getHelpList(pnames[pp])
        pdesc[pp] <- strsplit(strsplit(x$description, "\n")[[1]][2], 
            " \\(a")[[1]][1]
    }
    ROMcode <- c(paste0("Sen_", c(55, 56, 58, 59)), paste0("WGr_", 
        c(55, 56, 58, 59)), paste0("BrC_", c(55, 56, 58, 59)))
    return(list(MET = MET, dynSSB0 = dynSSB0, dynSSBMSY = dynSSBMSY, 
        R0 = R0, SSB0 = SSB0, Rec = Rec, CW = CW, CWa = CWa, 
        B_BMSY = B_BMSY, F_FMSY = F_FMSY, name = name, CMPdesc = c("Zero catches", 
            CMPdesc), pdesc = pdesc, ROMcode = ROMcode, pnames = pnames, 
        MPnames = unlist(lapply(MSEobj@MPs, FUN = function(X) paste0(unlist(X), 
            collapse = "-"))), OMnames = c(as.character(1:96), 
            paste0("R", 1:12)), Design = Design))
}