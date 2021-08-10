# pullDataSet.R

source("initTest.R")
source("runCMPs.R")
source("BR3.R")

OMdvec <- 1

BR_E3s_pullD <- BR_E3s
formals(BR_E3s_pullD)$pullD <- TRUE
class(BR_E3s_pullD)<-"MP"

BR_W3s_pullD <- BR_W3s
formals(BR_W3s_pullD)$pullD <- TRUE
class(BR_W3s_pullD)<-"MP"

gridUmultMSEs <-  runCMPs(  iOM = 1,
                            MPs = list(pullD = c("BR_E3s_pullD","BR_W3s_pullD") ),
                            checkMPs = TRUE,
                            reloadABT = FALSE,
                            projFolderName = "pullDset" )

