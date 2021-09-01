# CompRes<-readRDS("C:/Users/tcar_/Dropbox/abft-mse/R_package/ABTMSE/inst/shiny_apps/ABTMSE/data/CompResd.rda")
CompRes<<-readRDS("./data/CompResd.rda") # Can be loaded by user
MET<<-CompRes$MET
pnames<<-CompRes$pnames
MPnames<<-CompRes$MPnames
OMnames<<-CompRes$OMnames[!grepl("R",CompRes$OMnames)]
ROMnames<<-CompRes$OMnames[grepl("R",CompRes$OMnames)]
ROMcode<<-CompRes$ROMcode
MPcols<<-rep(c("black","red","green","blue","orange",
               "grey","purple","brown","pink","darkblue",
               "darkgreen","darkred","deeppink3","khaki",
               "turquoise3","tan3"),10)

Syear<<-1965
OMwt<<-c(300, 300, 150, 300, 300, 150, 300, 300, 150, 300, 300, 150, 150, 150, 75, 150, 150, 75, 250, 250, 125, 250, 250, 125, 300, 300, 150, 300, 300, 150, 300, 300, 150, 300, 300, 150, 150, 150, 75, 150, 150, 75,
  250, 250, 125, 250, 250, 125, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 300, 150, 150, 150, 150,
  300, 300, 300, 300, 300, 300, 300, 300)

MPs<<-unique(sapply(MPnames,function(x)substr(x,1,2)))
tunes<<-0:4# unique(sapply(MPnames,function(x)substr(x,3,3)))
MPtypes<<-MPs[MPs!="Ze"]


#MET<<-readRDS("./data/MET.rds")
#pnames<<-readRDS("./data/pnames.rds")
#MPnames<<-readRDS("./data/MPnames.rds")
#OMnames<<-readRDS("./data/OMnames.rds")
#OMgrid<<-readRDS("./data/OMgrid.rds")
