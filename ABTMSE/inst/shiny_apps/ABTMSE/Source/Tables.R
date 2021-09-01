maketable<-function(pp,tabno){

  temp<-changed() + length(input$CMPs)
  if(tabno==1)ind<<-getind1()
  if(tabno==2)ind<<-getind2()

  if(tabno==1)doWt = input$Wt1
  if(tabno==2)doWt = input$Wt2

  pind<-(1:length(pnames))[pnames%in%c(input$PM1,input$PM2,input$PM3,input$PM4)]
  MPind<-getMPind()
  MET_s<-MET[,,,MPind,]
  MPnames_s<<-MPnames[MPind]

  if(length(ind)>1){
    if(!doWt){
      tab<-apply(MET_s[,ind,pp,,pind],3:4,quantile,p=0.5)
    }else{
      tab<-wapply34(dat=MET_s[,ind,pp,,pind],OMw=OMwt[ind],q=0.5)
    }

  }else{

    tab<-apply(MET_s[,ind,pp,,pind],2:3,quantile,p=0.5)
    #tab<-wapply23(dat=MET_s[,ind,pp,,pind],OMw=OMwt[ind],q=0.5)
  }
  tab<-as.data.frame(tab,row.names=MPnames_s)
  names(tab)<-pnames[pind]
  #mult<-c(rep(1E-6,4),rep(100,6),rep(1,4))[pind]
  #tab[,1:4]<-tab[,1:4]*rep(mult,each=nrow(tab))
  #tab[,5:10]<-tab[,5:10]*100
  round(tab,2)
}


maketable2<-function(pp,tabno){
  temp<-changed() + length(input$CMPs)
  if(tabno==1)ind<<-getind1()
  if(tabno==2)ind<<-getind2()
  if(tabno==1)doWt = input$Wt1
  if(tabno==2)doWt = input$Wt2

  quants<-0.5+c(-input$PT2_IQR,0,input$PT2_IQR)/200
  pind<-(1:length(pnames))[pnames%in%c(input$PM2_1)]
  MPind<-getMPind()
  MET_s<-MET[,,,MPind,]
  MPnames_s<<-MPnames[MPind]

  if(length(ind)>1){
    if(!doWt){
      tab<-apply(MET_s[,ind,pp,,pind],3,quantile,p=quants)
    }else{
      tab<-wapply3_qs(dat=MET_s[,ind,pp,,pind],OMw=OMwt[ind],qs=quants)
    }
  }else{
    tab<-apply(MET_s[,ind,pp,,pind],2,quantile,p=quants)
  }

  tab<-as.data.frame(t(tab),row.names=MPnames_s)
  names(tab)<-paste(input$PM2_1, paste(quants*100,"%"))#names(tab))
  #mult<-c(rep(1E-6,4),rep(100,6),rep(1,4))[pind]
  #tab[,1:4]<-tab[,1:4]*rep(mult,each=nrow(tab))
  #tab[,5:10]<-tab[,5:10]*100
  round(tab,2)

}
