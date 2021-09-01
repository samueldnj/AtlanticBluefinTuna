
getind1<-function(){
  (1:nOMs)[c((OMgrid[,1]%in%input$Rec1 & OMgrid[,2]%in%input$SM1 & OMgrid[,3]%in%input$Scale1 & OMgrid[,4]%in%input$Comp1),ROMnames%in%input$Rob1)]
}

getind2<-function(){
  (1:nOMs)[c((OMgrid[,1]%in%input$Rec2 & OMgrid[,2]%in%input$SM2 & OMgrid[,3]%in%input$Scale2 & OMgrid[,4]%in%input$Comp2),ROMnames%in%input$Rob2)]
}

getMPind<-function(){
  MPind<-MPnames%in%input$CMPs
  MPind[1]<-TRUE # must have zero C
  if(sum(MPind)<2)MPind[2]<-TRUE # at least 1 other CMPs
  MPind
}

updateCMPwidgets<-function(MPnames){
  updateSelectInput(session,"Zeh_MP1",selected=MPnames[2],choices=MPnames)
  updateSelectInput(session,"Zeh_MP2",selected="None",choices=c("None",MPnames))

  updateSelectInput(session,"ZehPM_MP1",selected=MPnames[1],choices=MPnames)
  updateSelectInput(session,"ZehPM_MP2",selected=MPnames[2],choices=MPnames)
  updateSelectInput(session,"ZehPM_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

  updateSelectInput(session,"S_MP1",selected=MPnames[1],choices=MPnames)
  updateSelectInput(session,"S_MP2",selected=MPnames[2],choices=MPnames)
  updateSelectInput(session,"S_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

  updateSelectInput(session,"Sm_MP1",selected=MPnames[1],choices=MPnames)
  updateSelectInput(session,"Sm_MP2",selected=MPnames[2],choices=MPnames)

  updateSelectInput(session,"R_MP1",selected=MPnames[1],choices=MPnames)
  updateSelectInput(session,"R_MP2",selected=MPnames[2],choices=MPnames)
  updateSelectInput(session,"R_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)

  updateSelectInput(session,"REW_MP1",selected=MPnames[1],choices=MPnames)
  updateSelectInput(session,"REW_MP2",selected=MPnames[2],choices=MPnames)
  updateSelectInput(session,"REW_MP3",selected=MPnames[min(3,length(MPnames))],choices=MPnames)
}
