## Blank Data

## Initialize Data 
init_data<-function(){

  ## Init Größe der Instanz
  n<-2 # Number of Tage
  R<-2 # Number of Fahrlagen
  S<-5 # Number of Systemtrassen
  tage<-paste("tag",1:n,sep="")
  
  ## Init Systemtrassen Graph
  el<-data.frame(id=1:5,
                 von=c("Q","Q","A","B","B"),
                 bis=c("A","B","S","A","S"),
                 weight=c(25,25,15,0,45),
                 parent=rep(0,5))
  for(i in 1:n){
    el<-cbind(el,data.frame(tag=rep(1,S)))
  }
  colnames(el)[5:(n+4)]<-tage
  
  el$von<-as.character(el$von)
  el$bis<-as.character(el$bis)

}